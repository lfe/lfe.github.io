use std::fs;
use std::path::Path;

use anyhow::{Context, Result};
use pulldown_cmark::{html, Options, Parser};
use sha2::{Digest, Sha256};
use walkdir::WalkDir;

/// Pre-render markdown fields in data files.
///
/// Walks `_data/**/*.yml`, finds string fields whose keys end in
/// `_md` or `_md_inline`, renders them through pulldown-cmark, and
/// writes the result into a sibling field whose key ends in `_html`.
/// Uses SHA-256 content hashing so files that are already up-to-date
/// are left untouched.
pub fn run(project_dir: &Path) -> Result<()> {
    let src_dir = project_dir.join("src");
    let data_dir = if src_dir.join("_data").is_dir() {
        src_dir.join("_data")
    } else {
        project_dir.join("_data")
    };
    run_with_data_dir(&data_dir.parent().unwrap_or(project_dir).to_path_buf())
}

pub fn run_with_data_dir(base_dir: &Path) -> Result<()> {
    let data_dir = base_dir.join("_data");
    if !data_dir.is_dir() {
        anyhow::bail!("data directory not found: {}", data_dir.display());
    }

    let mut scanned: usize = 0;
    let mut updated: usize = 0;
    let mut skipped: usize = 0;

    for entry in WalkDir::new(&data_dir)
        .into_iter()
        .filter_map(|e| e.ok())
        .filter(|e| {
            e.file_type().is_file()
                && e.path()
                    .extension()
                    .map_or(false, |ext| ext == "yml" || ext == "yaml")
        })
    {
        scanned += 1;
        let path = entry.path();
        let rel = path
            .strip_prefix(base_dir)
            .unwrap_or(path)
            .display();

        let contents = fs::read_to_string(path)
            .with_context(|| format!("failed to read {rel}"))?;

        let mut doc: serde_yaml::Value = serde_yaml::from_str(&contents)
            .with_context(|| format!("failed to parse YAML in {rel}"))?;

        let changed = process_value(&mut doc);

        if changed {
            let new_yaml = serde_yaml::to_string(&doc)
                .with_context(|| format!("failed to serialize YAML for {rel}"))?;

            // Idempotency: compare SHA-256 of what we would write against
            // what is already on disk.
            let old_hash = sha256(contents.as_bytes());
            let new_hash = sha256(new_yaml.as_bytes());

            if old_hash == new_hash {
                println!("  skip (unchanged): {rel}");
                skipped += 1;
            } else {
                fs::write(path, &new_yaml)
                    .with_context(|| format!("failed to write {rel}"))?;
                println!("  updated: {rel}");
                updated += 1;
            }
        } else {
            println!("  skip (no _md fields): {rel}");
            skipped += 1;
        }
    }

    println!();
    println!(
        "prerender: {scanned} file(s) scanned, {updated} updated, {skipped} skipped"
    );
    Ok(())
}

// ---------------------------------------------------------------------------
// Markdown rendering
// ---------------------------------------------------------------------------

/// Render a markdown string to HTML using pulldown-cmark with common
/// extensions enabled.
fn render_markdown(input: &str) -> String {
    let options = Options::ENABLE_TABLES
        | Options::ENABLE_FOOTNOTES
        | Options::ENABLE_STRIKETHROUGH;
    let parser = Parser::new_ext(input, options);
    let mut html_output = String::new();
    html::push_html(&mut html_output, parser);
    html_output
}

/// Render a markdown string to inline HTML (outer `<p>...</p>` wrapper
/// stripped).  This mirrors Zola's `markdown(inline=true)` behaviour.
fn render_markdown_inline(input: &str) -> String {
    let mut rendered = render_markdown(input);
    if rendered.starts_with("<p>") && rendered.ends_with("</p>\n") {
        rendered = rendered[3..rendered.len() - 5].to_string();
    }
    rendered
}

// ---------------------------------------------------------------------------
// YAML tree traversal
// ---------------------------------------------------------------------------

/// Recursively walk a `serde_yaml::Value`, rendering `_md` / `_md_inline`
/// fields to their `_html` siblings.  Returns `true` if any value was
/// added or changed.
fn process_value(value: &mut serde_yaml::Value) -> bool {
    match value {
        serde_yaml::Value::Mapping(map) => process_mapping(map),
        serde_yaml::Value::Sequence(seq) => {
            let mut changed = false;
            for item in seq.iter_mut() {
                changed |= process_value(item);
            }
            changed
        }
        _ => false,
    }
}

/// Process a single YAML mapping: render every `*_md` / `*_md_inline`
/// string value and insert or update the corresponding `*_html` sibling.
///
/// To preserve key ordering we rebuild the mapping, inserting each new
/// `_html` key immediately after the `_md` / `_md_inline` key it was
/// derived from.
fn process_mapping(map: &mut serde_yaml::Mapping) -> bool {
    // First, recurse into nested values so the deepest levels are
    // processed before we touch this level.
    let mut changed = false;
    for (_k, v) in map.iter_mut() {
        changed |= process_value(v);
    }

    // Collect the work we need to do at *this* level.
    // Each entry is (md_key_string, html_key_string, rendered_html).
    let mut inserts: Vec<(String, String, String)> = Vec::new();

    for (key, value) in map.iter() {
        let key_str = match key.as_str() {
            Some(s) => s,
            None => continue,
        };
        let md_value = match value.as_str() {
            Some(s) => s,
            None => continue,
        };

        if let Some(base) = key_str.strip_suffix("_md_inline") {
            let html_key = format!("{base}_html");
            let rendered = render_markdown_inline(md_value);
            inserts.push((key_str.to_owned(), html_key, rendered));
        } else if let Some(base) = key_str.strip_suffix("_md") {
            let html_key = format!("{base}_html");
            let rendered = render_markdown(md_value);
            inserts.push((key_str.to_owned(), html_key, rendered));
        }
    }

    if inserts.is_empty() {
        return changed;
    }

    // Check each insert against the current value; skip if identical.
    let mut any_new = false;
    for (_md_key, html_key, rendered) in &inserts {
        let yaml_key = serde_yaml::Value::String(html_key.clone());
        match map.get(&yaml_key) {
            Some(existing) if existing.as_str() == Some(rendered.as_str()) => {
                // already up-to-date
            }
            _ => {
                any_new = true;
            }
        }
    }

    if !any_new {
        return changed;
    }

    // Rebuild the mapping so that each _html key appears right after
    // its _md counterpart while preserving original order otherwise.
    let mut new_map = serde_yaml::Mapping::new();

    // Build a lookup for quick access: html_key -> rendered value.
    let insert_map: std::collections::HashMap<String, String> = inserts
        .iter()
        .map(|(_md, html, rendered)| (html.clone(), rendered.clone()))
        .collect();

    // Set of _md keys that have a generated _html counterpart, so we
    // know which existing _html entries to skip (they will be
    // re-inserted in the right position).
    let html_keys_to_insert: std::collections::HashSet<String> = inserts
        .iter()
        .map(|(_, html, _)| html.clone())
        .collect();

    // Map from md_key -> html_key for positional insertion.
    let md_to_html: std::collections::HashMap<String, String> = inserts
        .iter()
        .map(|(md, html, _)| (md.clone(), html.clone()))
        .collect();

    for (key, value) in map.iter() {
        let key_str = key.as_str().unwrap_or("");

        // Skip _html keys that we are going to re-insert after their
        // _md counterpart.
        if html_keys_to_insert.contains(key_str) {
            continue;
        }

        // Copy the original entry.
        new_map.insert(key.clone(), value.clone());

        // If this is a _md key, insert the rendered _html right after.
        if let Some(html_key) = md_to_html.get(key_str) {
            if let Some(rendered) = insert_map.get(html_key) {
                new_map.insert(
                    serde_yaml::Value::String(html_key.clone()),
                    serde_yaml::Value::String(rendered.clone()),
                );
            }
        }
    }

    *map = new_map;
    true
}

// ---------------------------------------------------------------------------
// Hashing
// ---------------------------------------------------------------------------

/// Compute the SHA-256 digest of `data` and return it as a hex string.
fn sha256(data: &[u8]) -> String {
    let digest = Sha256::digest(data);
    digest.iter().map(|b| format!("{b:02x}")).collect()
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_render_markdown_paragraph() {
        let html = render_markdown("Hello **world**\n");
        assert_eq!(html, "<p>Hello <strong>world</strong></p>\n");
    }

    #[test]
    fn test_render_markdown_list() {
        let input = "* one\n* two\n";
        let html = render_markdown(input);
        assert!(html.contains("<ul>"));
        assert!(html.contains("<li>one</li>"));
        assert!(html.contains("<li>two</li>"));
    }

    #[test]
    fn test_render_markdown_inline_strips_p_wrapper() {
        let html = render_markdown_inline("Hello **world**\n");
        assert_eq!(html, "Hello <strong>world</strong>");
    }

    #[test]
    fn test_render_markdown_inline_multiline_no_strip() {
        // Two paragraphs -- the outer <p> strip should not fire.
        let input = "para one\n\npara two\n";
        let html = render_markdown_inline(input);
        assert!(html.contains("<p>"));
    }

    #[test]
    fn test_render_markdown_table() {
        let input = "| A | B |\n|---|---|\n| 1 | 2 |\n";
        let html = render_markdown(input);
        assert!(html.contains("<table>"));
    }

    #[test]
    fn test_render_markdown_strikethrough() {
        let html = render_markdown("~~deleted~~\n");
        assert!(html.contains("<del>deleted</del>"));
    }

    #[test]
    fn test_process_value_with_md_key() {
        let yaml = "title_md: \"Hello **world**\"\n";
        let mut doc: serde_yaml::Value = serde_yaml::from_str(yaml).unwrap();
        let changed = process_value(&mut doc);
        assert!(changed);

        let map = doc.as_mapping().unwrap();
        let html_val = map
            .get(serde_yaml::Value::String("title_html".into()))
            .unwrap();
        assert_eq!(
            html_val.as_str().unwrap(),
            "<p>Hello <strong>world</strong></p>\n"
        );
    }

    #[test]
    fn test_process_value_with_md_inline_key() {
        let yaml = "label_md_inline: \"Hello **world**\"\n";
        let mut doc: serde_yaml::Value = serde_yaml::from_str(yaml).unwrap();
        let changed = process_value(&mut doc);
        assert!(changed);

        let map = doc.as_mapping().unwrap();
        let html_val = map
            .get(serde_yaml::Value::String("label_html".into()))
            .unwrap();
        assert_eq!(
            html_val.as_str().unwrap(),
            "Hello <strong>world</strong>"
        );
    }

    #[test]
    fn test_process_value_nested_sequence() {
        let yaml = r#"
items:
  - name_md: "**bold**"
  - name_md_inline: "*italic*"
"#;
        let mut doc: serde_yaml::Value = serde_yaml::from_str(yaml).unwrap();
        let changed = process_value(&mut doc);
        assert!(changed);

        let items = doc["items"].as_sequence().unwrap();
        assert!(items[0]
            .as_mapping()
            .unwrap()
            .get(serde_yaml::Value::String("name_html".into()))
            .is_some());
        assert!(items[1]
            .as_mapping()
            .unwrap()
            .get(serde_yaml::Value::String("name_html".into()))
            .is_some());
    }

    #[test]
    fn test_process_value_idempotent() {
        let yaml = "title_md: \"Hello\"\n";
        let mut doc: serde_yaml::Value = serde_yaml::from_str(yaml).unwrap();

        let changed1 = process_value(&mut doc);
        assert!(changed1);

        // Second pass should not report any changes.
        let changed2 = process_value(&mut doc);
        assert!(!changed2);
    }

    #[test]
    fn test_html_key_inserted_after_md_key() {
        let yaml = "before: x\ncontent_md: \"hi\"\nafter: y\n";
        let mut doc: serde_yaml::Value = serde_yaml::from_str(yaml).unwrap();
        process_value(&mut doc);

        let map = doc.as_mapping().unwrap();
        let keys: Vec<&str> = map
            .keys()
            .filter_map(|k| k.as_str())
            .collect();

        let md_pos = keys.iter().position(|k| *k == "content_md").unwrap();
        let html_pos = keys.iter().position(|k| *k == "content_html").unwrap();
        assert_eq!(
            html_pos,
            md_pos + 1,
            "content_html should appear right after content_md"
        );
    }

    #[test]
    fn test_sha256_deterministic() {
        let a = sha256(b"hello");
        let b = sha256(b"hello");
        assert_eq!(a, b);
        assert_ne!(a, sha256(b"world"));
    }

    #[test]
    fn test_non_string_md_key_ignored() {
        // A key ending in _md whose value is not a string should be left alone.
        let yaml = "flags_md:\n  - one\n  - two\n";
        let mut doc: serde_yaml::Value = serde_yaml::from_str(yaml).unwrap();
        let changed = process_value(&mut doc);
        // The recursive descent into the sequence found no _md string values,
        // so no _html key should have been inserted at the top level.
        let map = doc.as_mapping().unwrap();
        assert!(
            !map.contains_key(serde_yaml::Value::String("flags_html".into())),
            "should not create _html for non-string _md values"
        );
        // `changed` is false because no rendering took place.
        assert!(!changed);
    }
}
