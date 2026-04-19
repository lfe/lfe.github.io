use std::fs;
use std::path::Path;

use anyhow::{Context, Result};
use regex::Regex;
use serde_yaml::Value as Yaml;
use toml::Value as Toml;

/// Run the Zola-to-Cobalt migration.
///
/// Reads TOML front-matter from `content/_index.md`, the Lykn template
/// `templates/index.html`, and `config.toml`, then writes individual
/// YAML data files under `_data/home/` and `_data/`.
pub fn run(project_dir: &Path) -> Result<()> {
    let data_dir = project_dir.join("_data");
    let home_dir = data_dir.join("home");
    fs::create_dir_all(&home_dir)
        .with_context(|| format!("creating {}", home_dir.display()))?;

    // --- Parse front-matter from content/_index.md ---
    let index_path = project_dir.join("content/_index.md");
    let index_text = fs::read_to_string(&index_path)
        .with_context(|| format!("reading {}", index_path.display()))?;
    let toml_str = extract_toml_frontmatter(&index_text)
        .context("extracting TOML front-matter from content/_index.md")?;
    let root: Toml = toml::from_str(&toml_str)
        .context("parsing TOML front-matter")?;
    let extra = root
        .get("extra")
        .context("missing [extra] in front-matter")?;

    // 1. masthead.yml
    let masthead = build_masthead(extra)?;
    write_yaml(&home_dir.join("masthead.yml"), &masthead)?;

    // 2. excerpts.yml
    let excerpts = build_excerpts(extra)?;
    write_yaml(&home_dir.join("excerpts.yml"), &excerpts)?;

    // 3. features.yml
    let features = build_features(extra)?;
    write_yaml(&home_dir.join("features.yml"), &features)?;

    // 4. buildit.yml
    let buildit = build_buildit(extra)?;
    write_yaml(&home_dir.join("buildit.yml"), &buildit)?;

    // 5. books.yml
    let books = build_books(extra)?;
    write_yaml(&home_dir.join("books.yml"), &books)?;

    // 6. videos.yml
    let videos = build_videos(extra)?;
    write_yaml(&home_dir.join("videos.yml"), &videos)?;

    // 7. callouts.yml
    let callouts = build_callouts(extra)?;
    write_yaml(&home_dir.join("callouts.yml"), &callouts)?;

    // 8. quotes.yml  (from templates/index.html)
    let template_path = project_dir.join("templates/index.html");
    let template_text = fs::read_to_string(&template_path)
        .with_context(|| format!("reading {}", template_path.display()))?;
    let quotes = build_quotes(&template_text)?;
    write_yaml(&home_dir.join("quotes.yml"), &quotes)?;

    // 9. site.yml  (from config.toml)
    let config_path = project_dir.join("config.toml");
    let config_text = fs::read_to_string(&config_path)
        .with_context(|| format!("reading {}", config_path.display()))?;
    let site = build_site(&config_text)?;
    write_yaml(&data_dir.join("site.yml"), &site)?;

    let data_file_count = 9;
    println!("migrate: generated {data_file_count} data files from Zola sources");
    println!("  _data/home/masthead.yml");
    println!("  _data/home/excerpts.yml");
    println!("  _data/home/features.yml");
    println!("  _data/home/buildit.yml");
    println!("  _data/home/books.yml");
    println!("  _data/home/videos.yml");
    println!("  _data/home/callouts.yml");
    println!("  _data/home/quotes.yml");
    println!("  _data/site.yml");

    // --- Convert content file front-matter from TOML to YAML ---
    let content_files: &[&str] = &[
        "content/about.md",
        "content/books.md",
        "content/community.md",
        "content/learn.md",
        "content/use.md",
        "content/plan/_index.md",
        "content/plan/how-to-add.md",
        "content/plan/oubiwann.md",
        "content/plan/rvirding.md",
        "content/plan/yurrriq.md",
    ];

    let mut content_file_count = 0u32;
    println!();
    println!("migrate: converting content file front-matter (TOML -> YAML)");

    for rel_path in content_files {
        let path = project_dir.join(rel_path);
        convert_content_file(&path)
            .with_context(|| format!("converting {}", path.display()))?;
        println!("  {rel_path}");
        content_file_count += 1;
    }

    // Create minimal content/index.md for Cobalt (replaces content/_index.md)
    let cobalt_index_path = project_dir.join("content/index.md");
    let cobalt_index_content = "\
---
layout: home.liquid
title: \"Lisp Flavoured Erlang\"
description: \"The website for LFE, the Erlang community's own Lisp.\"
---
";
    fs::write(&cobalt_index_path, cobalt_index_content)
        .with_context(|| format!("writing {}", cobalt_index_path.display()))?;
    println!("  content/index.md (new, replaces _index.md)");
    content_file_count += 1;

    // --- Generate _cobalt.yml ---
    let cobalt_yml_path = project_dir.join("_cobalt.yml");
    let cobalt_yml_content = "\
source: \".\"
destination: \"./_site\"
";
    fs::write(&cobalt_yml_path, cobalt_yml_content)
        .with_context(|| format!("writing {}", cobalt_yml_path.display()))?;
    println!();
    println!("migrate: generated _cobalt.yml");

    // --- Summary ---
    println!();
    println!("=== Migration Summary ===");
    println!("  Data files generated:    {data_file_count}");
    println!("  Content files converted: {content_file_count}");
    println!("  Cobalt config:           {}", cobalt_yml_path.display());

    Ok(())
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Extract the TOML text between `+++` delimiters.
fn extract_toml_frontmatter(text: &str) -> Option<String> {
    let trimmed = text.trim_start();
    let rest = trimmed.strip_prefix("+++")?;
    let end = rest.find("+++")?;
    Some(rest[..end].to_string())
}

/// Extract the markdown body after the closing `+++` delimiter.
fn extract_body(text: &str) -> Option<&str> {
    let trimmed = text.trim_start();
    let rest = trimmed.strip_prefix("+++")?;
    let end = rest.find("+++")?;
    // Skip past the closing `+++`
    Some(&rest[end + 3..])
}

/// Convert a single content file from TOML front-matter to YAML front-matter.
///
/// Reads the file, parses TOML, builds a YAML mapping with the appropriate
/// `layout`, `title`, optional `description`, and optional `extra` fields,
/// then writes the file back in place with `---` delimiters.
fn convert_content_file(path: &Path) -> Result<()> {
    let text = fs::read_to_string(path)
        .with_context(|| format!("reading {}", path.display()))?;

    let toml_str = extract_toml_frontmatter(&text)
        .with_context(|| format!("no TOML front-matter in {}", path.display()))?;
    let body = extract_body(&text)
        .with_context(|| format!("no closing +++ in {}", path.display()))?;

    let root: Toml = toml::from_str(&toml_str)
        .with_context(|| format!("parsing TOML in {}", path.display()))?;

    // Determine layout from template field
    let layout = match root.get("template").and_then(|v| v.as_str()) {
        Some("plan/page.html") => "plan/page.liquid",
        Some("plan/index.html") => "plan/index.liquid",
        _ => "page.liquid",
    };

    // Build YAML front-matter with controlled key order:
    //   layout, title, [description], [extra]
    let mut fm = serde_yaml::Mapping::new();

    fm.insert(
        Yaml::String("layout".to_string()),
        Yaml::String(layout.to_string()),
    );

    if let Some(title) = root.get("title").and_then(|v| v.as_str()) {
        fm.insert(
            Yaml::String("title".to_string()),
            Yaml::String(title.to_string()),
        );
    }

    // Carry over [extra] fields if present
    if let Some(extra) = root.get("extra").and_then(|v| v.as_table()) {
        let mut extra_map = serde_yaml::Mapping::new();
        for (k, v) in extra {
            extra_map.insert(Yaml::String(k.clone()), toml_to_yaml(v));
        }
        if !extra_map.is_empty() {
            fm.insert(
                Yaml::String("extra".to_string()),
                Yaml::Mapping(extra_map),
            );
        }
    }

    let yaml_str = serde_yaml::to_string(&Yaml::Mapping(fm))
        .context("serializing YAML front-matter")?;

    // Write the converted file
    let output = format!("---\n{yaml_str}---\n{body}");
    fs::write(path, output)
        .with_context(|| format!("writing {}", path.display()))?;

    Ok(())
}

/// Convert a `toml::Value` to a `serde_yaml::Value`.
fn toml_to_yaml(v: &Toml) -> Yaml {
    match v {
        Toml::String(s) => Yaml::String(s.clone()),
        Toml::Integer(i) => Yaml::Number(serde_yaml::Number::from(*i)),
        Toml::Float(f) => {
            Yaml::Number(serde_yaml::Number::from(*f))
        }
        Toml::Boolean(b) => Yaml::Bool(*b),
        Toml::Datetime(dt) => Yaml::String(dt.to_string()),
        Toml::Array(arr) => {
            Yaml::Sequence(arr.iter().map(toml_to_yaml).collect())
        }
        Toml::Table(tbl) => {
            let mut m = serde_yaml::Mapping::new();
            for (k, val) in tbl {
                m.insert(Yaml::String(k.clone()), toml_to_yaml(val));
            }
            Yaml::Mapping(m)
        }
    }
}

/// Shorthand: get a TOML string and convert it to a YAML string value.
fn yaml_str(v: &Toml, key: &str) -> Result<Yaml> {
    let s = v
        .get(key)
        .and_then(|v| v.as_str())
        .with_context(|| format!("missing key '{key}'"))?;
    Ok(Yaml::String(s.to_string()))
}

/// Build a YAML mapping from key-value pairs.
fn yaml_map(pairs: Vec<(&str, Yaml)>) -> Yaml {
    let mut m = serde_yaml::Mapping::new();
    for (k, v) in pairs {
        m.insert(Yaml::String(k.to_string()), v);
    }
    Yaml::Mapping(m)
}

/// Write a `serde_yaml::Value` to a file.
fn write_yaml(path: &Path, value: &Yaml) -> Result<()> {
    let text = serde_yaml::to_string(value)
        .with_context(|| format!("serializing YAML for {}", path.display()))?;
    fs::write(path, text)
        .with_context(|| format!("writing {}", path.display()))?;
    Ok(())
}

// ---------------------------------------------------------------------------
// Builders
// ---------------------------------------------------------------------------

/// 1. masthead.yml
fn build_masthead(extra: &Toml) -> Result<Yaml> {
    let summary = extra
        .get("summary")
        .context("missing [extra.summary]")?;

    Ok(yaml_map(vec![
        ("logo_image", yaml_str(extra, "logo_image")?),
        ("sitetagline", yaml_str(extra, "sitetagline")?),
        (
            "summary",
            yaml_map(vec![
                ("content_md", yaml_str(summary, "content")?),
                ("link_text", yaml_str(summary, "link_text")?),
                ("link_url", yaml_str(summary, "link_url")?),
            ]),
        ),
    ]))
}

/// 2. excerpts.yml
fn build_excerpts(extra: &Toml) -> Result<Yaml> {
    let section = extra
        .get("excerpts")
        .context("missing [extra.excerpts]")?;

    let order = [
        "repl",
        "simple_types",
        "compound_types",
        "records",
        "funcs",
        "macros",
        "erlang_interop",
        "otp",
    ];

    let items: Vec<Yaml> = order
        .iter()
        .map(|key| {
            let item = section
                .get(*key)
                .with_context(|| format!("missing [extra.excerpts.{key}]"))?;
            Ok(yaml_map(vec![
                ("name", yaml_str(item, "name")?),
                ("id", yaml_str(item, "id")?),
                ("code_md", yaml_str(item, "code")?),
                ("desc_md", yaml_str(item, "desc")?),
            ]))
        })
        .collect::<Result<Vec<_>>>()?;

    Ok(yaml_map(vec![("items", Yaml::Sequence(items))]))
}

/// 3. features.yml
fn build_features(extra: &Toml) -> Result<Yaml> {
    let section = extra
        .get("features")
        .context("missing [extra.features]")?;

    let title = yaml_str(section, "title")?;

    let order = ["lisp", "erlang", "otp", "lab"];

    let items: Vec<Yaml> = order
        .iter()
        .map(|key| {
            let item = section
                .get(*key)
                .with_context(|| format!("missing [extra.features.{key}]"))?;
            Ok(yaml_map(vec![
                ("id", Yaml::String(key.to_string())),
                ("title_md_inline", yaml_str(item, "title")?),
                ("icon_md_inline", yaml_str(item, "icon")?),
                ("content_md", yaml_str(item, "content")?),
                ("link_text", yaml_str(item, "link_text")?),
                ("link_url", yaml_str(item, "link_url")?),
            ]))
        })
        .collect::<Result<Vec<_>>>()?;

    Ok(yaml_map(vec![
        ("title", title),
        ("items", Yaml::Sequence(items)),
    ]))
}

/// 4. buildit.yml
fn build_buildit(extra: &Toml) -> Result<Yaml> {
    let section = extra
        .get("buildit")
        .context("missing [extra.buildit]")?;

    let title = yaml_str(section, "title")?;

    let order = ["scripts", "libraries", "apps", "releases"];

    let items: Vec<Yaml> = order
        .iter()
        .map(|key| {
            let item = section
                .get(*key)
                .with_context(|| format!("missing [extra.buildit.{key}]"))?;
            Ok(yaml_map(vec![
                ("id", Yaml::String(key.to_string())),
                ("title", yaml_str(item, "title")?),
                ("content_md", yaml_str(item, "content")?),
                ("link_text", yaml_str(item, "link_text")?),
                ("link_url", yaml_str(item, "link_url")?),
            ]))
        })
        .collect::<Result<Vec<_>>>()?;

    Ok(yaml_map(vec![
        ("title_md_inline", title),
        ("items", Yaml::Sequence(items)),
    ]))
}

/// 5. books.yml
fn build_books(extra: &Toml) -> Result<Yaml> {
    let section = extra
        .get("books")
        .context("missing [extra.books]")?;

    let title = yaml_str(section, "title")?;
    let link_text = yaml_str(section, "link_text")?;
    let link_url = yaml_str(section, "link_url")?;

    let order = ["chineual", "lfe_tutorial", "casting_spels", "sicp"];

    let items: Vec<Yaml> = order
        .iter()
        .map(|key| {
            let item = section
                .get(*key)
                .with_context(|| format!("missing [extra.books.{key}]"))?;
            Ok(yaml_map(vec![
                ("id", Yaml::String(key.to_string())),
                ("title", yaml_str(item, "title")?),
                ("authors", yaml_str(item, "authors")?),
                ("cover_md", yaml_str(item, "cover")?),
                ("description_md", yaml_str(item, "description")?),
                ("link_text", yaml_str(item, "link_text")?),
                ("link_url", yaml_str(item, "link_url")?),
            ]))
        })
        .collect::<Result<Vec<_>>>()?;

    Ok(yaml_map(vec![
        ("title", title),
        ("link_text", link_text),
        ("link_url", link_url),
        ("items", Yaml::Sequence(items)),
    ]))
}

/// 6. videos.yml
fn build_videos(extra: &Toml) -> Result<Yaml> {
    let section = extra
        .get("videos")
        .context("missing [extra.videos]")?;

    let title = yaml_str(section, "title")?;

    let order = [
        "lispnyc2021",
        "clojutre2015",
        "codesync2022",
        "efsf2017",
        "ecu2016",
        "efsf2014",
    ];

    let items: Vec<Yaml> = order
        .iter()
        .map(|key| {
            let item = section
                .get(*key)
                .with_context(|| format!("missing [extra.videos.{key}]"))?;
            Ok(yaml_map(vec![
                ("id", Yaml::String(key.to_string())),
                ("service_name", yaml_str(item, "service_name")?),
                ("venue", yaml_str(item, "venue")?),
                ("title", yaml_str(item, "title")?),
                ("speaker", yaml_str(item, "speaker")?),
                ("content_md", yaml_str(item, "content")?),
                ("link_text", yaml_str(item, "link_text")?),
                ("link_url", yaml_str(item, "link_url")?),
                ("channel_text", yaml_str(item, "channel_text")?),
                ("channel_url", yaml_str(item, "channel_url")?),
            ]))
        })
        .collect::<Result<Vec<_>>>()?;

    Ok(yaml_map(vec![
        ("title", title),
        ("items", Yaml::Sequence(items)),
    ]))
}

/// 7. callouts.yml
fn build_callouts(extra: &Toml) -> Result<Yaml> {
    let c1 = extra
        .get("callout1")
        .context("missing [extra.callout1]")?;
    let c2 = extra
        .get("callout2")
        .context("missing [extra.callout2]")?;

    let item1 = yaml_map(vec![
        ("id", Yaml::String("discord".to_string())),
        ("prefix_md_inline", yaml_str(c1, "prefix")?),
        ("title_md_inline", yaml_str(c1, "title")?),
        ("content", yaml_str(c1, "content")?),
    ]);

    let item2 = yaml_map(vec![
        ("id", Yaml::String("xkcd".to_string())),
        ("title_md_inline", yaml_str(c2, "title")?),
        ("content_md", yaml_str(c2, "content")?),
    ]);

    Ok(yaml_map(vec![(
        "items",
        Yaml::Sequence(vec![item1, item2]),
    )]))
}

/// 8. quotes.yml  (extracted from Lykn template)
fn build_quotes(template_text: &str) -> Result<Yaml> {
    // Each quote in the Lykn source looks like:  "\"Some quote text.\""
    // In the raw file the bytes are:  " \ " <text> \ " "
    // We match the outer "\"  ...  \"" and capture the inner text.
    let re = Regex::new(r#""\\?"([^"]*?)\\?""#)
        .context("compiling quote regex")?;

    // Find the block between `(bind quotes #a(` and the closing `))`
    let start = template_text
        .find("(bind quotes #a(")
        .context("could not find quote block in template")?;
    let block_start = start + "(bind quotes #a(".len();
    let block = &template_text[block_start..];
    let end = block
        .find("))")
        .context("could not find end of quote block")?;
    let block = &block[..end];

    let items: Vec<Yaml> = re
        .captures_iter(block)
        .filter_map(|cap| {
            let text = cap[1].to_string();
            if text.is_empty() {
                None
            } else {
                Some(Yaml::String(text))
            }
        })
        .collect();

    if items.is_empty() {
        anyhow::bail!("no quotes found in template");
    }

    Ok(yaml_map(vec![("items", Yaml::Sequence(items))]))
}

/// 9. site.yml  (from config.toml)
fn build_site(config_text: &str) -> Result<Yaml> {
    let config: Toml = toml::from_str(config_text)
        .context("parsing config.toml")?;
    let download = config
        .get("extra")
        .and_then(|e| e.get("download"))
        .context("missing [extra.download] in config.toml")?;

    Ok(yaml_map(vec![(
        "download",
        toml_to_yaml(download),
    )]))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_extract_toml_frontmatter() {
        let input = "+++\ntitle = \"hello\"\n+++\nbody";
        let fm = extract_toml_frontmatter(input).unwrap();
        assert!(fm.contains("title"));
    }

    #[test]
    fn test_extract_toml_frontmatter_none() {
        assert!(extract_toml_frontmatter("no front matter").is_none());
    }

    #[test]
    fn test_build_quotes() {
        let template = concat!(
            "<script type=\"text/lykn\">\n",
            "  (bind quotes #a(\n",
            "    \"\\\"A proper Lisp.\\\"\"\n",
            "    \"\\\"Another quote.\\\"\"))\n",
            "</script>\n",
        );
        let yaml = build_quotes(template).unwrap();
        let items = yaml
            .as_mapping()
            .unwrap()
            .get(&Yaml::String("items".into()))
            .unwrap()
            .as_sequence()
            .unwrap();
        assert_eq!(items.len(), 2);
        assert_eq!(items[0].as_str().unwrap(), "A proper Lisp.");
        assert_eq!(items[1].as_str().unwrap(), "Another quote.");
    }

    #[test]
    fn test_build_site() {
        let config = r#"
base_url = "https://example.com"
[extra]
[extra.download]
repository = "https://github.com/lfe/lfe"
prior_release = "2.1.5"
current_release = "2.2.0"
pre_release = "refs/heads/develop"
"#;
        let yaml = build_site(config).unwrap();
        let dl = yaml
            .as_mapping()
            .unwrap()
            .get(&Yaml::String("download".into()))
            .unwrap()
            .as_mapping()
            .unwrap();
        assert_eq!(
            dl.get(&Yaml::String("current_release".into()))
                .unwrap()
                .as_str()
                .unwrap(),
            "2.2.0"
        );
    }

    #[test]
    fn test_toml_to_yaml_string() {
        let t = Toml::String("hello".into());
        let y = toml_to_yaml(&t);
        assert_eq!(y.as_str().unwrap(), "hello");
    }

    #[test]
    fn test_toml_to_yaml_int() {
        let t = Toml::Integer(42);
        let y = toml_to_yaml(&t);
        assert_eq!(y.as_i64().unwrap(), 42);
    }

    #[test]
    fn test_extract_body() {
        let input = "+++\ntitle = \"hello\"\n+++\n\n## Body here\n";
        let body = extract_body(input).unwrap();
        assert_eq!(body, "\n\n## Body here\n");
    }

    #[test]
    fn test_extract_body_none() {
        assert!(extract_body("no front matter").is_none());
    }

    #[test]
    fn test_convert_content_file_simple() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("test.md");
        fs::write(
            &path,
            "+++\ntitle = \"About\"\nin_search_index = true\n\n[extra]\nlong_title = \"About LFE\"\n+++\n\n## Body\n",
        )
        .unwrap();

        convert_content_file(&path).unwrap();

        let result = fs::read_to_string(&path).unwrap();
        assert!(result.starts_with("---\n"));
        assert!(result.contains("layout: page.liquid"));
        assert!(result.contains("title: About"));
        assert!(result.contains("extra:"));
        assert!(result.contains("long_title: About LFE"));
        // in_search_index should be dropped (not mapped)
        assert!(!result.contains("in_search_index"));
        // Body should be preserved
        assert!(result.contains("## Body"));
    }

    #[test]
    fn test_convert_content_file_plan_page() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("test.md");
        fs::write(
            &path,
            "+++\ntitle = \"oubiwann's .plan\"\ntemplate = \"plan/page.html\"\n\n[extra]\nlong_title = \"LFEX .plan\"\n+++\n\nBody text\n",
        )
        .unwrap();

        convert_content_file(&path).unwrap();

        let result = fs::read_to_string(&path).unwrap();
        assert!(result.contains("layout: plan/page.liquid"));
        assert!(!result.contains("plan/page.html"));
    }

    #[test]
    fn test_convert_content_file_plan_index() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("test.md");
        fs::write(
            &path,
            "+++\ntitle = \".plan Files\"\ntemplate = \"plan/index.html\"\n\n[extra]\nlong_title = \"LFEX .plan Files\"\n+++\n\nBody\n",
        )
        .unwrap();

        convert_content_file(&path).unwrap();

        let result = fs::read_to_string(&path).unwrap();
        assert!(result.contains("layout: plan/index.liquid"));
        assert!(!result.contains("plan/index.html"));
    }

    #[test]
    fn test_yaml_map_ordering() {
        let m = yaml_map(vec![
            ("b", Yaml::String("second".into())),
            ("a", Yaml::String("first".into())),
        ]);
        let mapping = m.as_mapping().unwrap();
        let keys: Vec<&str> = mapping
            .keys()
            .map(|k| k.as_str().unwrap())
            .collect();
        // serde_yaml::Mapping preserves insertion order
        assert_eq!(keys, vec!["b", "a"]);
    }
}
