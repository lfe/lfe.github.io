use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::Path;

use anyhow::{Context, Result};
use sha2::{Digest, Sha256};
use walkdir::WalkDir;

use crate::util::split_front_matter;

/// Resolve blog editorial configuration into a template-ready data file.
///
/// Reads `_data/blog.yml` (editorial config) and all post front-matter from
/// `posts/*.md`, resolves editorial slots to full metadata, computes
/// deterministic image placement for river items, and writes
/// `_data/blog_resolved.yml` for template consumption.
pub fn run(src_dir: &Path) -> Result<()> {
    let data_dir = src_dir.join("_data");
    let blog_yml = data_dir.join("blog.yml");

    if !blog_yml.exists() {
        println!("  skip: _data/blog.yml not found");
        return Ok(());
    }

    let posts_dir = src_dir.join("posts");
    if !posts_dir.is_dir() {
        println!("  skip: no posts/ directory");
        return Ok(());
    }

    // 0. Assign default cover images to posts that have none
    let assigned = assign_default_covers(src_dir)?;
    if assigned > 0 {
        println!("  assigned cover images to {} posts", assigned);
    }

    // 1. Read editorial config
    let config_str = fs::read_to_string(&blog_yml).context("reading blog.yml")?;
    let config: serde_yaml::Value =
        serde_yaml::from_str(&config_str).context("parsing blog.yml")?;

    // 2. Read author name lookup
    let authors = load_authors(&data_dir)?;

    // 3. Scan all published posts
    let mut all_posts = scan_posts(&posts_dir, &authors)?;
    // Reverse-chronological: filenames are date-prefixed so string sort works.
    all_posts.sort_by(|a, b| b.slug.cmp(&a.slug));
    println!("  scanned {} published posts", all_posts.len());

    // 4. Config values
    let river_count = config
        .get("river_count")
        .and_then(|v| v.as_u64())
        .unwrap_or(5) as usize;
    let default_cover = config
        .get("default_cover_image")
        .and_then(|v| v.as_str())
        .unwrap_or("/images/LFE_Masthead_00234_.png");
    let default_alt = config
        .get("default_cover_alt")
        .and_then(|v| v.as_str())
        .unwrap_or("The LFE Blog");

    // 5. Resolve editorial slots
    let slot_names = ["centrepiece", "lede_1", "lede_2", "spotlight"];

    // Slugs explicitly configured for any slot (used to build the fallback pool).
    let configured_slugs: HashSet<&str> = slot_names
        .iter()
        .filter_map(|name| config.get(*name).and_then(|v| v.as_str()))
        .collect();

    let post_map: HashMap<&str, &PostMeta> =
        all_posts.iter().map(|p| (p.slug.as_str(), p)).collect();

    // Fallback pool: posts NOT named in any editorial slot, in reverse-chrono order.
    let fallback_pool: Vec<&PostMeta> = all_posts
        .iter()
        .filter(|p| !configured_slugs.contains(p.slug.as_str()))
        .collect();
    let mut fallback_idx: usize = 0;

    let mut used: HashSet<String> = HashSet::new();
    let mut resolved_slots: Vec<(&str, Option<serde_yaml::Value>)> = Vec::new();

    for name in &slot_names {
        let resolved = resolve_slot(
            *name,
            &config,
            &post_map,
            &fallback_pool,
            &mut fallback_idx,
            &mut used,
            default_cover,
            default_alt,
        );
        resolved_slots.push((*name, resolved));
    }

    // 6. Build sorted authors list (by post count desc, then name asc)
    let authors_sorted = build_sorted_authors(&all_posts, &data_dir)?;

    // 7. Build river
    let river: Vec<serde_yaml::Value> = all_posts
        .iter()
        .filter(|p| !used.contains(&p.slug))
        .take(river_count)
        .map(|p| {
            let side = deterministic_side(&p.slug);
            post_to_yaml(p, default_cover, default_alt, Some(&side))
        })
        .collect();

    // 8. Assemble output
    let mut out = serde_yaml::Mapping::new();

    for (name, value) in &resolved_slots {
        if let Some(v) = value {
            out.insert(ykey(name), v.clone());
        }
    }

    out.insert(ykey("river"), serde_yaml::Value::Sequence(river));
    out.insert(ykey("authors_sorted"), serde_yaml::Value::Sequence(authors_sorted));
    out.insert(ykey("default_cover_image"), ystr(default_cover));
    out.insert(ykey("default_cover_alt"), ystr(default_alt));

    let yaml_out = serde_yaml::to_string(&serde_yaml::Value::Mapping(out))
        .context("serializing blog_resolved.yml")?;

    // 9. Write (idempotent)
    let resolved_path = data_dir.join("blog_resolved.yml");
    let needs_write = if resolved_path.exists() {
        let existing = fs::read_to_string(&resolved_path).unwrap_or_default();
        existing != yaml_out
    } else {
        true
    };

    if needs_write {
        fs::write(&resolved_path, &yaml_out).context("writing blog_resolved.yml")?;
        println!("  updated: _data/blog_resolved.yml");
    } else {
        println!("  skip (unchanged): _data/blog_resolved.yml");
    }

    Ok(())
}

// ---------------------------------------------------------------------------
// Slot resolution
// ---------------------------------------------------------------------------

/// Resolve a single editorial slot: try the configured slug, fall back to
/// the next unused post from the chronological pool.
fn resolve_slot(
    name: &str,
    config: &serde_yaml::Value,
    post_map: &HashMap<&str, &PostMeta>,
    fallback_pool: &[&PostMeta],
    fallback_idx: &mut usize,
    used: &mut HashSet<String>,
    default_cover: &str,
    default_alt: &str,
) -> Option<serde_yaml::Value> {
    // Try configured slug
    if let Some(slug) = config.get(name).and_then(|v| v.as_str()) {
        if let Some(post) = post_map.get(slug) {
            used.insert(slug.to_string());
            return Some(post_to_yaml(post, default_cover, default_alt, None));
        }
        eprintln!(
            "  warning: {} slug '{}' not found, using fallback",
            name, slug
        );
    }

    // Fallback: next unused chronological post
    while *fallback_idx < fallback_pool.len() {
        let post = fallback_pool[*fallback_idx];
        *fallback_idx += 1;
        if !used.contains(&post.slug) {
            used.insert(post.slug.clone());
            return Some(post_to_yaml(post, default_cover, default_alt, None));
        }
    }

    None
}

// ---------------------------------------------------------------------------
// Post scanning
// ---------------------------------------------------------------------------

/// Metadata extracted from a single post's front-matter.
struct PostMeta {
    slug: String,
    title: String,
    description: String,
    permalink: String,
    categories: Vec<String>,
    author_slug: String,
    author_name: String,
    date_display: String,
    cover_image: Option<String>,
    cover_alt: Option<String>,
    reading_time: u32,
}

/// Scan posts directory recursively, parse front-matter, return metadata
/// for every published (non-draft) post.
///
/// Posts may live flat in `posts/` or in year subdirectories like
/// `posts/2019/05-13-1549-slug.md`. In the subdirectory case the full
/// slug is reconstructed by prepending the year: `2019-05-13-1549-slug`.
fn scan_posts(posts_dir: &Path, authors: &HashMap<String, String>) -> Result<Vec<PostMeta>> {
    let mut posts = Vec::new();

    let mut entries: Vec<_> = WalkDir::new(posts_dir)
        .into_iter()
        .filter_map(|e| e.ok())
        .filter(|e| {
            e.file_type().is_file()
                && e.path()
                    .extension()
                    .map_or(false, |ext| ext == "md")
        })
        .collect();
    entries.sort_by_key(|e| e.file_name().to_os_string());

    for entry in entries {
        let path = entry.path();

        // Reconstruct slug from relative path within posts_dir.
        // Flat:   posts/2019-05-13-1549-slug.md  → 2019-05-13-1549-slug
        // Nested: posts/2019/05-13-1549-slug.md  → 2019-05-13-1549-slug
        let slug = match slug_from_path(path, posts_dir) {
            Some(s) => s,
            None => continue,
        };

        let content = fs::read_to_string(&path)
            .with_context(|| format!("reading {}", path.display()))?;

        let (fm_str, body) = match split_front_matter(&content) {
            Some(pair) => pair,
            None => continue,
        };

        let fm: serde_yaml::Value = match serde_yaml::from_str(&fm_str) {
            Ok(v) => v,
            Err(e) => {
                eprintln!("  warning: skipping {} (YAML parse: {})", slug, e);
                continue;
            }
        };

        // Skip drafts
        if fm
            .get("is_draft")
            .and_then(|v| v.as_bool())
            .unwrap_or(false)
        {
            continue;
        }

        let title = yaml_str(&fm, "title").unwrap_or_default();
        let description = yaml_str(&fm, "description").unwrap_or_default();
        let permalink = yaml_str(&fm, "permalink").unwrap_or_default();

        let categories: Vec<String> = fm
            .get("categories")
            .and_then(|v| v.as_sequence())
            .map(|seq| {
                seq.iter()
                    .filter_map(|v| v.as_str().map(String::from))
                    .collect()
            })
            .unwrap_or_default();

        let data = fm.get("data");

        let author_slug = data
            .and_then(|d| d.get("author"))
            .and_then(|v| v.as_str())
            .unwrap_or("unknown")
            .to_string();

        let author_name = authors
            .get(&author_slug)
            .cloned()
            .unwrap_or_else(|| author_slug.clone());

        let cover_image = data
            .and_then(|d| d.get("cover_image"))
            .and_then(|v| v.as_str())
            .filter(|s| !s.is_empty())
            .map(String::from);

        let cover_alt = data
            .and_then(|d| d.get("cover_alt"))
            .and_then(|v| v.as_str())
            .filter(|s| !s.is_empty())
            .map(String::from);

        let date_display = format_date_from_slug(&slug);

        // Reading time: word count / 200 wpm, minimum 1 minute.
        let word_count = body.split_whitespace().count();
        let reading_time = std::cmp::max(1, (word_count / 200) as u32);

        posts.push(PostMeta {
            slug,
            title,
            description,
            permalink,
            categories,
            author_slug,
            author_name,
            date_display,
            cover_image,
            cover_alt,
            reading_time,
        });
    }

    Ok(posts)
}

// ---------------------------------------------------------------------------
// YAML output helpers
// ---------------------------------------------------------------------------

/// Convert a PostMeta into a serde_yaml::Value::Mapping with all fields
/// the template needs.
fn post_to_yaml(
    post: &PostMeta,
    default_cover: &str,
    default_alt: &str,
    image_side: Option<&str>,
) -> serde_yaml::Value {
    let mut map = serde_yaml::Mapping::new();

    map.insert(ykey("title"), ystr(&post.title));
    map.insert(ykey("description"), ystr(&post.description));
    map.insert(ykey("permalink"), ystr(&post.permalink));
    map.insert(ykey("author"), ystr(&post.author_slug));
    map.insert(ykey("author_name"), ystr(&post.author_name));
    map.insert(ykey("date_display"), ystr(&post.date_display));
    map.insert(
        ykey("reading_time"),
        serde_yaml::Value::Number(post.reading_time.into()),
    );

    let cats: Vec<serde_yaml::Value> = post.categories.iter().map(|c| ystr(c)).collect();
    map.insert(ykey("categories"), serde_yaml::Value::Sequence(cats));

    // Cover image: post-specific or default fallback
    let cover = post.cover_image.as_deref().unwrap_or(default_cover);
    let alt = post.cover_alt.as_deref().unwrap_or(default_alt);
    map.insert(ykey("cover_image"), ystr(cover));
    map.insert(ykey("cover_alt"), ystr(alt));

    if let Some(side) = image_side {
        map.insert(ykey("image_side"), ystr(side));
    }

    serde_yaml::Value::Mapping(map)
}

fn ykey(s: &str) -> serde_yaml::Value {
    serde_yaml::Value::String(s.to_string())
}

fn ystr(s: &str) -> serde_yaml::Value {
    serde_yaml::Value::String(s.to_string())
}

// ---------------------------------------------------------------------------
// Parsing helpers
// ---------------------------------------------------------------------------

/// Extract a string value from a YAML mapping by key.
fn yaml_str(val: &serde_yaml::Value, key: &str) -> Option<String> {
    val.get(key).and_then(|v| match v {
        serde_yaml::Value::String(s) => Some(s.clone()),
        serde_yaml::Value::Number(n) => Some(n.to_string()),
        serde_yaml::Value::Bool(b) => Some(b.to_string()),
        serde_yaml::Value::Null => None,
        // Datetime values (e.g. published_date) fall through here;
        // we derive dates from the slug instead.
        _ => None,
    })
}

/// Reconstruct a full slug from a post's path relative to the posts directory.
///
/// If the file is in a year subdirectory (e.g. `2019/05-13-1549-slug.md`),
/// the year is prepended: `2019-05-13-1549-slug`.
/// If the file is at the top level (e.g. `2019-05-13-1549-slug.md`),
/// the stem is used as-is.
fn slug_from_path(path: &Path, posts_dir: &Path) -> Option<String> {
    let rel = path.strip_prefix(posts_dir).ok()?;
    let stem = rel.file_stem()?.to_str()?;

    let parent = rel.parent().and_then(|p| p.to_str()).unwrap_or("");

    if parent.is_empty() || parent == "." {
        Some(stem.to_string())
    } else {
        // e.g. parent="2019", stem="05-13-1549-slug" → "2019-05-13-1549-slug"
        Some(format!("{}-{}", parent, stem))
    }
}


/// Build a sorted list of authors with post counts for template use.
///
/// Sorted by post count descending, then display name ascending for ties.
fn build_sorted_authors(posts: &[PostMeta], data_dir: &Path) -> Result<Vec<serde_yaml::Value>> {
    let path = data_dir.join("authors.yml");
    if !path.exists() {
        return Ok(Vec::new());
    }

    let content = fs::read_to_string(&path).context("reading authors.yml")?;
    let doc: serde_yaml::Value =
        serde_yaml::from_str(&content).context("parsing authors.yml")?;

    let mut post_counts: HashMap<String, usize> = HashMap::new();
    for post in posts {
        *post_counts.entry(post.author_slug.clone()).or_insert(0) += 1;
    }

    struct AuthorEntry {
        slug: String,
        name: String,
        bio: Option<String>,
        avatar: Option<String>,
        github_user: Option<String>,
        post_count: usize,
    }

    let mut author_entries: Vec<AuthorEntry> = Vec::new();

    if let Some(map) = doc.as_mapping() {
        for (key, value) in map {
            if let Some(slug) = key.as_str() {
                let name = value
                    .get("name")
                    .and_then(|v| v.as_str())
                    .unwrap_or(slug)
                    .to_string();
                let bio = value
                    .get("bio")
                    .and_then(|v| v.as_str())
                    .map(String::from);
                let avatar = value
                    .get("avatar")
                    .and_then(|v| v.as_str())
                    .map(String::from);
                let github_user = value
                    .get("github_user")
                    .and_then(|v| v.as_str())
                    .map(String::from);
                let count = post_counts.get(slug).copied().unwrap_or(0);
                author_entries.push(AuthorEntry {
                    slug: slug.to_string(),
                    name,
                    bio,
                    avatar,
                    github_user,
                    post_count: count,
                });
            }
        }
    }

    author_entries.sort_by(|a, b| {
        b.post_count
            .cmp(&a.post_count)
            .then_with(|| a.name.cmp(&b.name))
    });

    let sorted: Vec<serde_yaml::Value> = author_entries
        .into_iter()
        .map(|entry| {
            let mut map = serde_yaml::Mapping::new();
            map.insert(ykey("slug"), ystr(&entry.slug));
            map.insert(ykey("name"), ystr(&entry.name));
            if let Some(b) = &entry.bio {
                map.insert(ykey("bio"), ystr(b));
            }
            if let Some(a) = &entry.avatar {
                map.insert(ykey("avatar"), ystr(a));
            }
            if let Some(g) = &entry.github_user {
                map.insert(ykey("github_user"), ystr(g));
            }
            map.insert(
                ykey("post_count"),
                serde_yaml::Value::Number(serde_yaml::Number::from(entry.post_count as u64)),
            );
            serde_yaml::Value::Mapping(map)
        })
        .collect();

    Ok(sorted)
}

/// Load author slug → display name mapping from `_data/authors.yml`.
fn load_authors(data_dir: &Path) -> Result<HashMap<String, String>> {
    let path = data_dir.join("authors.yml");
    if !path.exists() {
        return Ok(HashMap::new());
    }

    let content = fs::read_to_string(&path).context("reading authors.yml")?;
    let doc: serde_yaml::Value =
        serde_yaml::from_str(&content).context("parsing authors.yml")?;

    let mut authors = HashMap::new();
    if let Some(map) = doc.as_mapping() {
        for (key, value) in map {
            if let (Some(slug), Some(name)) = (
                key.as_str(),
                value.get("name").and_then(|v| v.as_str()),
            ) {
                authors.insert(slug.to_string(), name.to_string());
            }
        }
    }

    Ok(authors)
}

/// Format a display date from the slug's date prefix.
///
/// Slugs follow the pattern `YYYY-MM-DD-HHMM-title`, so the first 10
/// characters are always `YYYY-MM-DD`.
fn format_date_from_slug(slug: &str) -> String {
    if slug.len() < 10 {
        return slug.to_string();
    }
    let date_part = &slug[..10];
    let parts: Vec<&str> = date_part.splitn(3, '-').collect();
    if parts.len() != 3 {
        return slug[..10].to_string();
    }

    let year = parts[0];
    let month = match parts[1] {
        "01" => "January",
        "02" => "February",
        "03" => "March",
        "04" => "April",
        "05" => "May",
        "06" => "June",
        "07" => "July",
        "08" => "August",
        "09" => "September",
        "10" => "October",
        "11" => "November",
        "12" => "December",
        other => other,
    };
    // Strip leading zero from day
    let day = parts[2].trim_start_matches('0');

    format!("{month} {day}, {year}")
}

// ---------------------------------------------------------------------------
// Deterministic selection
// ---------------------------------------------------------------------------

/// Deterministically pick an item from a pool using the slug's SHA-256 hash.
///
/// Same slug always picks the same item. Stable across builds, platforms,
/// and pool reorderings (the pool must be sorted before calling).
fn deterministic_pick<'a>(slug: &str, pool: &'a [String]) -> Option<&'a String> {
    if pool.is_empty() {
        return None;
    }
    let hash = Sha256::digest(slug.as_bytes());
    let index =
        u32::from_le_bytes([hash[0], hash[1], hash[2], hash[3]]) as usize % pool.len();
    Some(&pool[index])
}

/// Compute a deterministic "left" or "right" from a post slug.
fn deterministic_side(slug: &str) -> String {
    let hash = Sha256::digest(slug.as_bytes());
    if hash[0] % 2 == 0 {
        "left".to_string()
    } else {
        "right".to_string()
    }
}

// ---------------------------------------------------------------------------
// Default cover assignment
// ---------------------------------------------------------------------------

/// Scan all posts for `cover_image: null` and assign a deterministic image
/// from the default pool (`src/images/default/`). Writes directly into the
/// post's front-matter — idempotent (once assigned, kept on subsequent builds).
///
/// Uses the same `deterministic_pick` function available for Friday images
/// or any other image pool.
fn assign_default_covers(src_dir: &Path) -> Result<u32> {
    let default_dir = src_dir.join("images/default");
    if !default_dir.is_dir() {
        return Ok(0);
    }

    // Collect and sort the pool for deterministic ordering.
    let mut pool: Vec<String> = fs::read_dir(&default_dir)
        .context("reading images/default")?
        .filter_map(|e| e.ok())
        .filter(|e| {
            e.path()
                .extension()
                .map_or(false, |ext| ext == "png" || ext == "jpg" || ext == "webp")
        })
        .filter_map(|e| {
            e.file_name()
                .to_str()
                .map(|name| format!("/images/default/{name}"))
        })
        .collect();
    pool.sort();

    if pool.is_empty() {
        return Ok(0);
    }

    let posts_dir = src_dir.join("posts");
    if !posts_dir.is_dir() {
        return Ok(0);
    }

    let mut assigned = 0u32;

    for entry in WalkDir::new(&posts_dir)
        .into_iter()
        .filter_map(|e| e.ok())
        .filter(|e| {
            e.file_type().is_file()
                && e.path().extension().map_or(false, |ext| ext == "md")
        })
    {
        let path = entry.path();
        let content = fs::read_to_string(path)
            .with_context(|| format!("reading {}", path.display()))?;

        if !content.contains("cover_image: null") {
            continue;
        }

        let slug = match slug_from_path(path, &posts_dir) {
            Some(s) => s,
            None => continue,
        };

        let image = match deterministic_pick(&slug, &pool) {
            Some(img) => img.clone(),
            None => continue,
        };

        let new_content = content
            .replace(
                "cover_image: null",
                &format!("cover_image: \"{image}\""),
            )
            .replace(
                "cover_alt: null",
                "cover_alt: \"Vigdís — LFE, retro-futurist digital painting\"",
            );

        if new_content != content {
            fs::write(path, &new_content)
                .with_context(|| format!("writing {}", path.display()))?;
            assigned += 1;
        }
    }

    Ok(assigned)
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_deterministic_pick_is_stable() {
        let pool: Vec<String> = vec!["a.png", "b.png", "c.png"]
            .into_iter()
            .map(String::from)
            .collect();
        let a = deterministic_pick("some-slug", &pool);
        let b = deterministic_pick("some-slug", &pool);
        assert_eq!(a, b);
    }

    #[test]
    fn test_deterministic_pick_distributes() {
        let pool: Vec<String> = (0..5).map(|i| format!("img-{i}.png")).collect();
        let picks: HashSet<&String> = (0..50)
            .filter_map(|i| deterministic_pick(&format!("slug-{i}"), &pool))
            .collect();
        assert!(picks.len() > 1, "expected multiple distinct picks");
    }

    #[test]
    fn test_deterministic_pick_empty_pool() {
        let pool: Vec<String> = vec![];
        assert!(deterministic_pick("any-slug", &pool).is_none());
    }

    #[test]
    fn test_deterministic_side_is_stable() {
        let a = deterministic_side("2019-05-13-1549-running-lfe-in-docker-updated");
        let b = deterministic_side("2019-05-13-1549-running-lfe-in-docker-updated");
        assert_eq!(a, b);
    }

    #[test]
    fn test_format_date_from_slug() {
        assert_eq!(
            format_date_from_slug("2019-05-13-1549-running-lfe-in-docker"),
            "May 13, 2019"
        );
        assert_eq!(
            format_date_from_slug("2014-12-07-1837-running-lfe-in-docker"),
            "December 7, 2014"
        );
        assert_eq!(
            format_date_from_slug("2015-01-04-1931-lfe-friday"),
            "January 4, 2015"
        );
    }

    #[test]
    fn test_slug_from_path_nested() {
        let posts = Path::new("/project/src/posts");
        let path = Path::new("/project/src/posts/2019/05-13-1549-running-lfe-in-docker-updated.md");
        assert_eq!(
            slug_from_path(path, posts),
            Some("2019-05-13-1549-running-lfe-in-docker-updated".to_string())
        );
    }

    #[test]
    fn test_slug_from_path_flat() {
        let posts = Path::new("/project/src/posts");
        let path = Path::new("/project/src/posts/2019-05-13-1549-running-lfe-in-docker-updated.md");
        assert_eq!(
            slug_from_path(path, posts),
            Some("2019-05-13-1549-running-lfe-in-docker-updated".to_string())
        );
    }

    #[test]
    fn test_split_front_matter() {
        let input = "---\ntitle: \"Hello\"\n---\nBody here\n";
        let (fm, body) = split_front_matter(input).unwrap();
        assert!(fm.contains("title"));
        assert_eq!(body, "Body here\n");
    }

    #[test]
    fn test_split_front_matter_no_delimiters() {
        assert!(split_front_matter("no front matter here").is_none());
    }

    #[test]
    fn test_yaml_str_string() {
        let val: serde_yaml::Value = serde_yaml::from_str("title: Hello").unwrap();
        assert_eq!(yaml_str(&val, "title"), Some("Hello".to_string()));
    }

    #[test]
    fn test_yaml_str_null() {
        let val: serde_yaml::Value = serde_yaml::from_str("title: null").unwrap();
        assert_eq!(yaml_str(&val, "title"), None);
    }

    #[test]
    fn test_yaml_str_missing() {
        let val: serde_yaml::Value = serde_yaml::from_str("title: Hello").unwrap();
        assert_eq!(yaml_str(&val, "missing"), None);
    }

    #[test]
    fn test_load_authors_parses_correctly() {
        let dir = std::env::temp_dir().join("lfesite_test_authors");
        let _ = fs::remove_dir_all(&dir);
        fs::create_dir_all(&dir).unwrap();

        fs::write(
            dir.join("authors.yml"),
            "alice:\n  name: Alice Smith\n  bio: test\nbob:\n  name: Bob Jones\n  bio: test\n",
        )
        .unwrap();

        let authors = load_authors(&dir).unwrap();
        assert_eq!(authors.get("alice"), Some(&"Alice Smith".to_string()));
        assert_eq!(authors.get("bob"), Some(&"Bob Jones".to_string()));

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_post_to_yaml_uses_default_cover() {
        let post = PostMeta {
            slug: "2024-01-01-0000-test".to_string(),
            title: "Test".to_string(),
            description: "A test".to_string(),
            permalink: "/blog/test/".to_string(),
            categories: vec!["tutorials".to_string()],
            author_slug: "alice".to_string(),
            author_name: "Alice".to_string(),
            date_display: "January 1, 2024".to_string(),
            cover_image: None,
            cover_alt: None,
            reading_time: 3,
        };

        let yaml = post_to_yaml(&post, "/default.png", "Default", None);
        let map = yaml.as_mapping().unwrap();
        assert_eq!(
            map.get(&ykey("cover_image")).and_then(|v| v.as_str()),
            Some("/default.png")
        );
    }

    #[test]
    fn test_post_to_yaml_prefers_post_cover() {
        let post = PostMeta {
            slug: "2024-01-01-0000-test".to_string(),
            title: "Test".to_string(),
            description: "A test".to_string(),
            permalink: "/blog/test/".to_string(),
            categories: vec![],
            author_slug: "alice".to_string(),
            author_name: "Alice".to_string(),
            date_display: "January 1, 2024".to_string(),
            cover_image: Some("/custom.png".to_string()),
            cover_alt: Some("Custom".to_string()),
            reading_time: 1,
        };

        let yaml = post_to_yaml(&post, "/default.png", "Default", Some("left"));
        let map = yaml.as_mapping().unwrap();
        assert_eq!(
            map.get(&ykey("cover_image")).and_then(|v| v.as_str()),
            Some("/custom.png")
        );
        assert_eq!(
            map.get(&ykey("image_side")).and_then(|v| v.as_str()),
            Some("left")
        );
    }
}
