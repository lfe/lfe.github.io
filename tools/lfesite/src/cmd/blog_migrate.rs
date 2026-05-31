use std::collections::{BTreeMap, HashSet};
use std::fs;
use std::path::Path;

use anyhow::{Context, Result};
use regex::Regex;

/// Run the Jekyll-to-Cobalt blog post migration.
pub fn run(project_dir: &Path, source: &Path) -> Result<()> {
    println!("blog-migrate: discovering source posts...");

    let posts_dir = source.join("src/_posts");
    let drafts_dir = source.join("src/_drafts");
    let dest_dir = project_dir.join("src/posts");

    fs::create_dir_all(&dest_dir)
        .with_context(|| format!("creating {}", dest_dir.display()))?;

    // Collect existing posts (filenames only) for dedup
    let existing: HashSet<String> = fs::read_dir(&dest_dir)
        .map(|entries| {
            entries
                .filter_map(|e| e.ok())
                .map(|e| e.file_name().to_string_lossy().to_string())
                .collect()
        })
        .unwrap_or_default();

    // Discover source posts
    let mut post_files: Vec<(String, std::path::PathBuf, bool)> = Vec::new(); // (filename, path, is_draft)
    let mut skipped = 0u32;

    if posts_dir.exists() {
        let mut entries: Vec<_> = fs::read_dir(&posts_dir)
            .with_context(|| format!("reading {}", posts_dir.display()))?
            .filter_map(|e| e.ok())
            .filter(|e| {
                let name = e.file_name().to_string_lossy().to_string();
                name.ends_with(".md") && name != ".placeholder"
            })
            .collect();
        entries.sort_by_key(|e| e.file_name());
        for entry in entries {
            let name = entry.file_name().to_string_lossy().to_string();
            if existing.contains(&name) {
                skipped += 1;
            } else {
                post_files.push((name, entry.path(), false));
            }
        }
    }

    let posts_to_migrate = post_files.len();

    if drafts_dir.exists() {
        let mut entries: Vec<_> = fs::read_dir(&drafts_dir)
            .with_context(|| format!("reading {}", drafts_dir.display()))?
            .filter_map(|e| e.ok())
            .filter(|e| {
                let name = e.file_name().to_string_lossy().to_string();
                name.ends_with(".md") && name != ".placeholder"
            })
            .collect();
        entries.sort_by_key(|e| e.file_name());
        for entry in entries {
            let name = entry.file_name().to_string_lossy().to_string();
            if existing.contains(&name) {
                skipped += 1;
            } else {
                post_files.push((name, entry.path(), true));
            }
        }
    }

    let drafts_to_migrate = post_files.len() - posts_to_migrate;

    println!(
        "  found {} posts to migrate ({} already migrated, skipped)",
        posts_to_migrate, skipped
    );
    println!("  found {} drafts to migrate", drafts_to_migrate);

    // Process posts
    println!("blog-migrate: processing posts...");

    let filename_re =
        Regex::new(r"^(\d{4})-(\d{2})-(\d{2})-(\d{4})-(.+)\.md$").context("filename regex")?;

    let mut all_tags: BTreeMap<String, u32> = BTreeMap::new();
    let mut normalizations: Vec<String> = Vec::new();
    let mut new_authors: HashSet<String> = HashSet::new();
    let mut migrated = 0u32;

    for (filename, path, is_draft) in &post_files {
        let caps = filename_re.captures(filename).with_context(|| {
            format!("filename does not match expected pattern: {}", filename)
        })?;
        let year = &caps[1];
        let month = &caps[2];
        let day = &caps[3];
        let hhmm = &caps[4];
        let slug = &caps[5];

        let hh = &hhmm[..2];
        let mm = &hhmm[2..];

        let content =
            fs::read_to_string(&path).with_context(|| format!("reading {}", path.display()))?;

        // Split front-matter and body
        let (front_matter_str, body) = split_front_matter(&content)
            .with_context(|| format!("parsing front-matter in {}", filename))?;

        // Parse fields from front-matter
        let title = extract_field(&front_matter_str, "title")
            .unwrap_or_default()
            .trim_matches('"')
            .to_string();
        let description = extract_field(&front_matter_str, "description")
            .unwrap_or_default()
            .trim_matches('"')
            .to_string();
        let category_raw = extract_field(&front_matter_str, "category").unwrap_or_default();
        let author_name = extract_field(&front_matter_str, "author").unwrap_or_default();
        let tags_raw = extract_tags(&front_matter_str);

        // Normalize category
        let category = normalize_category(&category_raw);
        if category != category_raw.to_lowercase() && !category_raw.is_empty() {
            normalizations.push(format!(
                "category: '{}' -> '{}'",
                category_raw, category
            ));
        }

        // Normalize tags
        let tags: Vec<String> = tags_raw
            .iter()
            .map(|t| {
                let normalized = normalize_tag(t);
                if normalized != *t {
                    normalizations.push(format!("tag: '{}' -> '{}'", t, normalized));
                }
                normalized
            })
            .collect();

        // Count tags
        for tag in &tags {
            *all_tags.entry(tag.clone()).or_insert(0) += 1;
        }

        // Author slug
        let author_slug = author_to_slug(&author_name);
        if !is_known_author(&author_slug) {
            new_authors.insert(author_slug.clone());
        }

        // Check for math
        let has_math = content.contains("{% include MathJax/setup %}");

        // Build permalink
        let permalink = format!(
            "/blog/{}/{}/{}/{}/{}-{}",
            category, year, month, day, hhmm, slug
        );

        // Build tags YAML
        let tags_yaml = format!(
            "[{}]",
            tags.iter()
                .map(|t| format!("\"{}\"", t))
                .collect::<Vec<_>>()
                .join(", ")
        );

        // Build categories YAML
        let categories_yaml = format!("[\"{}\"]", category);

        // Build front-matter
        let new_front_matter = format!(
            "---\n\
             layout: post.liquid\n\
             title: \"{}\"\n\
             description: \"{}\"\n\
             permalink: \"{}\"\n\
             categories: {}\n\
             tags: {}\n\
             published_date: {}-{}-{} {}:{}:00 +0000\n\
             is_draft: {}\n\
             data:\n\
             {space}author: {}\n\
             {space}written_for: null\n\
             {space}last_validated: null\n\
             {space}cover_image: null\n\
             {space}cover_alt: null\n\
             {space}math: {}\n\
             ---\n",
            escape_yaml_string(&title),
            escape_yaml_string(&description),
            permalink,
            categories_yaml,
            tags_yaml,
            year,
            month,
            day,
            hh,
            mm,
            is_draft,
            author_slug,
            has_math,
            space = "  ",
        );

        // Transform body
        let new_body = transform_body(body, &author_name);

        // Write output
        let output = format!("{}{}", new_front_matter, new_body);
        let dest_path = dest_dir.join(filename);
        fs::write(&dest_path, output)
            .with_context(|| format!("writing {}", dest_path.display()))?;

        migrated += 1;
        if migrated % 20 == 0 {
            println!("  processed {} posts...", migrated);
        }
    }

    println!("  processed {} posts total", migrated);

    // Copy images
    println!("blog-migrate: copying images...");
    let src_images = source.join("src/assets/images/posts");
    let dest_images = project_dir.join("src/blog/assets/images/posts");
    fs::create_dir_all(&dest_images)
        .with_context(|| format!("creating {}", dest_images.display()))?;

    let mut copied = 0u32;
    let mut img_skipped = 0u32;

    if src_images.exists() {
        let entries: Vec<_> = fs::read_dir(&src_images)
            .with_context(|| format!("reading {}", src_images.display()))?
            .filter_map(|e| e.ok())
            .filter(|e| {
                let name = e.file_name().to_string_lossy().to_string();
                !name.starts_with('.')
            })
            .collect();

        for entry in entries {
            let name = entry.file_name().to_string_lossy().to_string();
            let dest_path = dest_images.join(&name);
            if dest_path.exists() {
                img_skipped += 1;
            } else {
                fs::copy(entry.path(), &dest_path).with_context(|| {
                    format!("copying {} to {}", entry.path().display(), dest_path.display())
                })?;
                copied += 1;
            }
        }
    }

    println!(
        "  copied {} images ({} already existed, skipped)",
        copied, img_skipped
    );

    // Update authors.yml
    println!("blog-migrate: updating authors.yml...");
    let authors_path = project_dir.join("src/_data/authors.yml");
    let mut authors_added = 0u32;

    if authors_path.exists() {
        let authors_content =
            fs::read_to_string(&authors_path).context("reading authors.yml")?;

        let stub_authors = [
            (
                "eric-bailey",
                "Eric Bailey",
                "Erlang and LFE developer.",
            ),
            (
                "anurag-mendhekar",
                "Anurag Mendhekar",
                "Software engineer and LFE contributor.",
            ),
            (
                "lfe-maintainers",
                "LFE Maintainers",
                "The LFE core team.",
            ),
        ];

        let mut appended = String::new();
        for (slug, name, bio) in &stub_authors {
            if !authors_content.contains(&format!("{}:", slug)) {
                appended.push_str(&format!(
                    "\n{}:\n  name: \"{}\"\n  bio: \"{}\"\n  avatar: null\n",
                    slug, name, bio
                ));
                authors_added += 1;
            }
        }

        if !appended.is_empty() {
            let mut full = authors_content;
            full.push_str(&appended);
            fs::write(&authors_path, full).context("writing authors.yml")?;
        }
    }

    println!("  added {} new authors", authors_added);

    // Generate tag report
    println!("blog-migrate: generating tag report...");
    let workbench_dir = project_dir.join("workbench");
    fs::create_dir_all(&workbench_dir).context("creating workbench dir")?;
    let report_path = workbench_dir.join("phase-4-tag-report.md");

    let mut report = String::new();
    report.push_str("# Blog Migration Tag Report\n\n");
    report.push_str("## All Tags (with post counts)\n\n");
    report.push_str("| Tag | Posts |\n");
    report.push_str("|-----|-------|\n");
    for (tag, count) in &all_tags {
        report.push_str(&format!("| {} | {} |\n", tag, count));
    }

    // Normalizations
    if !normalizations.is_empty() {
        report.push_str("\n## Normalizations Applied\n\n");
        let unique_norms: Vec<_> = normalizations
            .iter()
            .collect::<HashSet<_>>()
            .into_iter()
            .collect();
        for n in unique_norms {
            report.push_str(&format!("- {}\n", n));
        }
    }

    // Multi-word tags
    let multi_word: Vec<_> = all_tags
        .keys()
        .filter(|t| t.contains(' '))
        .collect();
    if !multi_word.is_empty() {
        report.push_str("\n## Multi-word Tags (flagged for review)\n\n");
        for t in multi_word {
            report.push_str(&format!("- `{}`\n", t));
        }
    }

    fs::write(&report_path, report).context("writing tag report")?;
    println!("  wrote workbench/phase-4-tag-report.md");

    // Summary: total posts in dest = already existing + newly migrated
    let total_in_dest = existing.len() as u32 + migrated;
    println!(
        "blog-migrate: done. {} posts migrated, {} total.",
        migrated, total_in_dest
    );

    Ok(())
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Split content into (front-matter, body). Front-matter is between `---` delimiters.
fn split_front_matter(content: &str) -> Option<(String, &str)> {
    let trimmed = content.trim_start();
    let rest = trimmed.strip_prefix("---")?;
    // Find the closing ---
    let end = rest.find("\n---")?;
    let fm = rest[..end].to_string();
    let body_start = end + 4; // skip \n---
    // Skip the newline after closing ---
    let body = if body_start < rest.len() && rest.as_bytes()[body_start] == b'\n' {
        &rest[body_start + 1..]
    } else {
        &rest[body_start..]
    };
    Some((fm, body))
}

/// Extract a simple field value from YAML front-matter text.
fn extract_field(fm: &str, key: &str) -> Option<String> {
    for line in fm.lines() {
        let trimmed = line.trim();
        if trimmed.starts_with(&format!("{}:", key)) {
            let value = trimmed[key.len() + 1..].trim();
            // Strip surrounding quotes
            let value = value
                .strip_prefix('"')
                .and_then(|v| v.strip_suffix('"'))
                .unwrap_or(value);
            return Some(value.to_string());
        }
    }
    None
}

/// Extract tags list from front-matter.
fn extract_tags(fm: &str) -> Vec<String> {
    for line in fm.lines() {
        let trimmed = line.trim();
        if trimmed.starts_with("tags:") {
            let value = trimmed["tags:".len()..].trim();
            // Parse [tag1, tag2, ...] format
            if let Some(inner) = value.strip_prefix('[').and_then(|v| v.strip_suffix(']')) {
                return parse_tag_list(inner);
            }
            // Could be a multi-line tag block, but in this corpus it's always inline
            return Vec::new();
        }
    }
    Vec::new()
}

/// Parse a comma-separated tag list (may have spaces or not).
fn parse_tag_list(inner: &str) -> Vec<String> {
    // Handle the case where some tags have spaces and commas may be missing
    // Split on comma, then trim each tag
    inner
        .split(',')
        .map(|t| t.trim().to_string())
        .filter(|t| !t.is_empty())
        .collect()
}

/// Normalize category: fix known typos, lowercase.
fn normalize_category(cat: &str) -> String {
    let trimmed = cat.trim().trim_matches('"');
    if trimmed.is_empty() {
        return "uncategorized".to_string();
    }
    match trimmed {
        "Formalwear" => "formalwear".to_string(),
        other => other.to_lowercase(),
    }
}

/// Normalize a tag.
fn normalize_tag(tag: &str) -> String {
    let trimmed = tag.trim();
    match trimmed {
        "howto" => "howtos".to_string(),
        other => other.to_string(),
    }
}

/// Map author name to slug.
fn author_to_slug(name: &str) -> String {
    match name.trim() {
        "Duncan McGreggor" => "duncan-mcgreggor".to_string(),
        "Robert Virding" => "robert-virding".to_string(),
        "Fred Hébert" => "fred-hebert".to_string(),
        "Eric Bailey" => "eric-bailey".to_string(),
        "Anurag Mendhekar" => "anurag-mendhekar".to_string(),
        "LFE Maintainers" => "lfe-maintainers".to_string(),
        other => {
            let slug = other.to_lowercase().replace(' ', "-");
            eprintln!(
                "  warning: unknown author '{}', using slug '{}'",
                other, slug
            );
            slug
        }
    }
}

/// Check if an author slug is one of the known authors.
fn is_known_author(slug: &str) -> bool {
    matches!(
        slug,
        "duncan-mcgreggor"
            | "robert-virding"
            | "fred-hebert"
            | "eric-bailey"
            | "anurag-mendhekar"
            | "lfe-maintainers"
    )
}

/// Escape a string for YAML double-quoted scalar.
fn escape_yaml_string(s: &str) -> String {
    s.replace('\\', "\\\\").replace('"', "\\\"")
}

/// Transform the body content from Jekyll to Cobalt format.
fn transform_body(body: &str, author_name: &str) -> String {
    let mut result = String::with_capacity(body.len());

    for line in body.lines() {
        // 1. Remove {% include JB/setup %}
        if line.trim() == "{% include JB/setup %}" {
            continue;
        }

        // 3. Remove {% include MathJax/setup %}
        if line.trim() == "{% include MathJax/setup %}" {
            continue;
        }

        // 2. Replace {% include LFEFriday/setup %}
        if line.trim() == "{% include LFEFriday/setup %}" {
            result.push_str(&format!(
                "<a href=\"/blog/assets/images/posts/LispFlavoredErlang-medium-square.png\">\
                 <img class=\"left tiny\" src=\"/blog/assets/images/posts/LispFlavoredErlang-medium-square.png\" /></a>\
                 This week's LFE Friday was translated with permission from the\n\
                 [Erlang Thursday](http://www.proctor-it.com/category/erlang/erlang-thursday/)\n\
                 series by [Steven Proctor](https://twitter.com/stevenproctor).\n\
                 *This week's translator*: {}.\n",
                author_name
            ));
            continue;
        }

        // 4. Replace ```cl (NOT ```clojure) with ```lfe
        // 5. Replace ```lisp with ```lfe
        let transformed_line = transform_code_fences(line);

        // 6. Replace {{ site.base_url }}/ with /blog/
        let transformed_line = transformed_line.replace("{{ site.base_url }}/", "/blog/");

        // 7. Replace bare internal links (don't double-prefix)
        let transformed_line = rewrite_internal_links(&transformed_line);

        result.push_str(&transformed_line);
        result.push('\n');
    }

    result
}

/// Transform code fence markers: ```cl -> ```lfe, ```lisp -> ```lfe
/// but NOT ```clojure
fn transform_code_fences(line: &str) -> String {
    // Match ```cl at end of line or followed by whitespace (not followed by more word chars)
    let trimmed = line.trim();

    // Check for ```cl (exact or followed by whitespace, not followed by 'o' for clojure)
    if trimmed == "```cl" || trimmed.starts_with("```cl ") || trimmed.starts_with("```cl\t") {
        return line.replacen("```cl", "```lfe", 1);
    }

    // Check for ```lisp (exact or followed by whitespace)
    if trimmed == "```lisp" || trimmed.starts_with("```lisp ") || trimmed.starts_with("```lisp\t")
    {
        return line.replacen("```lisp", "```lfe", 1);
    }

    line.to_string()
}

/// Rewrite bare internal links to add /blog/ prefix, without double-prefixing.
fn rewrite_internal_links(line: &str) -> String {
    let prefixes = [
        "/tutorials/",
        "/assets/",
        "/announcements/",
        "/news/",
        "/site/",
        "/archaeology/",
        "/formalwear/",
        "/updates/",
    ];

    let mut result = line.to_string();
    for prefix in &prefixes {
        // We need to replace occurrences of `"<prefix>` with `"/blog/<prefix>`
        // but NOT where it's already `/blog/<prefix>`
        // Strategy: find all occurrences, check character before to ensure it's not already prefixed
        let blog_prefix = format!("/blog{}", prefix);
        let mut new_result = String::with_capacity(result.len());
        let mut search_from = 0;

        while let Some(pos) = result[search_from..].find(prefix) {
            let abs_pos = search_from + pos;

            // Check if this is already prefixed with /blog
            let already_prefixed = if abs_pos >= 5 {
                &result[abs_pos - 5..abs_pos] == "/blog"
            } else {
                false
            };

            if already_prefixed {
                // Don't modify, just copy through
                new_result.push_str(&result[search_from..abs_pos + prefix.len()]);
            } else {
                // Check if preceded by a quote or ( or = (typical link context)
                // We replace regardless since the spec says bare internal links
                new_result.push_str(&result[search_from..abs_pos]);
                new_result.push_str(&blog_prefix);
            }
            search_from = abs_pos + prefix.len();
        }
        new_result.push_str(&result[search_from..]);
        result = new_result;
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_split_front_matter() {
        let input = "---\ntitle: \"Hello\"\n---\nBody here\n";
        let (fm, body) = split_front_matter(input).unwrap();
        assert!(fm.contains("title"));
        assert_eq!(body, "Body here\n");
    }

    #[test]
    fn test_extract_field() {
        let fm = "\ntitle: \"Hello World\"\ncategory: tutorials\nauthor: Duncan McGreggor\n";
        assert_eq!(
            extract_field(fm, "title").unwrap(),
            "Hello World"
        );
        assert_eq!(
            extract_field(fm, "category").unwrap(),
            "tutorials"
        );
        assert_eq!(
            extract_field(fm, "author").unwrap(),
            "Duncan McGreggor"
        );
    }

    #[test]
    fn test_extract_tags() {
        let fm = "\ntags: [lfe friday,lfe,erlang]\n";
        let tags = extract_tags(fm);
        assert_eq!(tags, vec!["lfe friday", "lfe", "erlang"]);
    }

    #[test]
    fn test_normalize_category() {
        assert_eq!(normalize_category("Formalwear"), "formalwear");
        assert_eq!(normalize_category("tutorials"), "tutorials");
        assert_eq!(normalize_category(""), "uncategorized");
        assert_eq!(normalize_category(" "), "uncategorized");
    }

    #[test]
    fn test_normalize_tag() {
        assert_eq!(normalize_tag("howto"), "howtos");
        assert_eq!(normalize_tag("lfe"), "lfe");
    }

    #[test]
    fn test_author_to_slug() {
        assert_eq!(author_to_slug("Duncan McGreggor"), "duncan-mcgreggor");
        assert_eq!(author_to_slug("Fred Hébert"), "fred-hebert");
        assert_eq!(author_to_slug("Eric Bailey"), "eric-bailey");
    }

    #[test]
    fn test_transform_code_fences() {
        assert_eq!(transform_code_fences("```cl"), "```lfe");
        assert_eq!(transform_code_fences("```clojure"), "```clojure");
        assert_eq!(transform_code_fences("```lisp"), "```lfe");
        assert_eq!(transform_code_fences("```lfe"), "```lfe");
    }

    #[test]
    fn test_rewrite_internal_links_no_double_prefix() {
        let input = r#"<a href="/blog/assets/images/foo.png">"#;
        let output = rewrite_internal_links(input);
        assert_eq!(output, input); // should not change
    }

    #[test]
    fn test_rewrite_internal_links_adds_prefix() {
        let input = r#"<a href="/assets/images/foo.png">"#;
        let output = rewrite_internal_links(input);
        assert_eq!(output, r#"<a href="/blog/assets/images/foo.png">"#);
    }

    #[test]
    fn test_body_removes_jb_setup() {
        let body = "{% include JB/setup %}\nHello\n";
        let result = transform_body(body, "Test Author");
        assert!(!result.contains("JB/setup"));
        assert!(result.contains("Hello"));
    }

    #[test]
    fn test_body_removes_mathjax() {
        let body = "{% include MathJax/setup %}\nContent\n";
        let result = transform_body(body, "Test Author");
        assert!(!result.contains("MathJax"));
        assert!(result.contains("Content"));
    }

    #[test]
    fn test_body_replaces_lfe_friday() {
        let body = "{% include LFEFriday/setup %}\nContent\n";
        let result = transform_body(body, "Robert Virding");
        assert!(result.contains("LispFlavoredErlang-medium-square.png"));
        assert!(result.contains("Robert Virding"));
        assert!(result.contains("Steven Proctor"));
    }

    #[test]
    fn test_body_replaces_site_base_url() {
        let body = "<img src=\"{{ site.base_url }}/assets/images/foo.png\" />\n";
        let result = transform_body(body, "Author");
        assert!(result.contains("/blog/assets/images/foo.png"));
        assert!(!result.contains("site.base_url"));
    }

    #[test]
    fn test_escape_yaml_string() {
        assert_eq!(escape_yaml_string(r#"Say "hello""#), r#"Say \"hello\""#);
    }
}
