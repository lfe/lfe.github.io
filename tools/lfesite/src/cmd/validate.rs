use std::collections::BTreeSet;
use std::fs;
use std::path::Path;

use anyhow::Result;
use walkdir::WalkDir;

/// Validate data files and content for correctness.
///
/// Runs four sanity checks to catch common authoring errors early:
///
/// 1. **YAML validity** -- all `_data/**/*.yml` files parse as valid YAML.
/// 2. **`_md`/`_html` pairing** -- every key ending in `_md` or `_md_inline`
///    has a corresponding `_html` sibling (i.e., prerender has been run).
/// 3. **Content front-matter** -- all `.md` files at the project root and in
///    `plan/` have valid YAML front-matter with a `layout` field.
/// 4. **Layout existence** -- every `layout` referenced in front-matter has a
///    corresponding file in `_layouts/`.
pub fn run(project_dir: &Path) -> Result<()> {
    let mut errors: Vec<String> = Vec::new();

    // ------------------------------------------------------------------
    // Check 1: YAML validity
    // ------------------------------------------------------------------
    println!("validate: checking data files...");
    let data_dir = project_dir.join("_data");
    let mut data_file_count: usize = 0;

    if data_dir.is_dir() {
        for entry in WalkDir::new(&data_dir)
            .sort_by_file_name()
            .into_iter()
            .filter_map(|e| e.ok())
            .filter(|e| {
                e.file_type().is_file()
                    && e.path()
                        .extension()
                        .map_or(false, |ext| ext == "yml" || ext == "yaml")
            })
        {
            let path = entry.path();
            let rel = path.strip_prefix(project_dir).unwrap_or(path);
            let rel_display = rel.display();

            match fs::read_to_string(path) {
                Ok(contents) => match serde_yaml::from_str::<serde_yaml::Value>(&contents) {
                    Ok(doc) => {
                        data_file_count += 1;
                        println!("  \u{2713} {rel_display}");

                        // Check 2 inline: collect _md/_html pairing errors
                        // while we have the parsed document.
                        check_md_html_pairing(
                            &doc,
                            &rel_display.to_string(),
                            &mut errors,
                        );
                    }
                    Err(e) => {
                        data_file_count += 1;
                        let msg = format!("{rel_display}: invalid YAML: {e}");
                        errors.push(msg);
                    }
                },
                Err(e) => {
                    let msg = format!("{rel_display}: cannot read file: {e}");
                    errors.push(msg);
                }
            }
        }
    } else {
        println!("  (no _data directory found)");
    }

    // ------------------------------------------------------------------
    // Check 2: _md/_html pairing (report)
    // ------------------------------------------------------------------
    println!("validate: checking _md/_html pairing...");
    let pairing_errors: Vec<&String> = errors
        .iter()
        .filter(|e| e.contains("has no") && e.contains("_html sibling"))
        .collect();
    if pairing_errors.is_empty() {
        println!("  \u{2713} all _md fields have _html siblings");
    } else {
        for err in &pairing_errors {
            println!("  ERROR: {err}");
        }
    }

    // ------------------------------------------------------------------
    // Check 3: Content front-matter
    // ------------------------------------------------------------------
    println!("validate: checking content front-matter...");
    let mut content_file_count: usize = 0;
    let mut layouts_referenced: BTreeSet<String> = BTreeSet::new();

    let plan_dir = project_dir.join("plan");
    let content_dirs: Vec<&Path> = vec![project_dir, &plan_dir];

    for dir in &content_dirs {
        if !dir.is_dir() {
            continue;
        }
        // Only direct children (depth 1), not recursive.
        let read_dir = match fs::read_dir(dir) {
            Ok(rd) => rd,
            Err(_) => continue,
        };

        let mut entries: Vec<_> = read_dir.filter_map(|e| e.ok()).collect();
        entries.sort_by_key(|e| e.file_name());

        for entry in entries {
            let path = entry.path();
            if !path.is_file() {
                continue;
            }
            let ext_match = path
                .extension()
                .map_or(false, |ext| ext == "md");
            if !ext_match {
                continue;
            }
            // Skip README.md
            if path
                .file_name()
                .map_or(false, |n| n == "README.md")
            {
                continue;
            }

            let rel = path.strip_prefix(project_dir).unwrap_or(&path);
            let rel_display = rel.display();
            content_file_count += 1;

            let contents = match fs::read_to_string(&path) {
                Ok(c) => c,
                Err(e) => {
                    errors.push(format!("{rel_display}: cannot read file: {e}"));
                    continue;
                }
            };

            match extract_frontmatter(&contents) {
                None => {
                    errors.push(format!("{rel_display}: missing YAML front-matter"));
                }
                Some(fm_str) => match serde_yaml::from_str::<serde_yaml::Value>(fm_str) {
                    Err(e) => {
                        errors.push(format!(
                            "{rel_display}: invalid front-matter YAML: {e}"
                        ));
                    }
                    Ok(fm) => {
                        let layout = fm
                            .as_mapping()
                            .and_then(|m| m.get("layout"))
                            .and_then(|v| v.as_str());

                        match layout {
                            None => {
                                errors.push(format!(
                                    "{rel_display}: missing 'layout' in front-matter"
                                ));
                            }
                            Some(layout_val) => {
                                println!(
                                    "  \u{2713} {rel_display} (layout: {layout_val})"
                                );
                                layouts_referenced.insert(layout_val.to_owned());
                            }
                        }
                    }
                },
            }
        }
    }

    // ------------------------------------------------------------------
    // Check 4: Layout existence
    // ------------------------------------------------------------------
    println!("validate: checking layout references...");
    let layouts_dir = project_dir.join("_layouts");

    for layout in &layouts_referenced {
        let layout_path = layouts_dir.join(layout);
        if layout_path.is_file() {
            println!("  \u{2713} _layouts/{layout} exists");
        } else {
            errors.push(format!("layout '{layout}' not found at _layouts/{layout}"));
        }
    }

    // ------------------------------------------------------------------
    // Summary
    // ------------------------------------------------------------------
    println!();
    if errors.is_empty() {
        println!(
            "validate: all checks passed ({} data files, {} content files, {} layouts)",
            data_file_count,
            content_file_count,
            layouts_referenced.len(),
        );
        Ok(())
    } else {
        println!("validate: {} error(s) found:", errors.len());
        for err in &errors {
            println!("  ERROR: {err}");
        }
        anyhow::bail!(
            "validation failed with {} error(s)",
            errors.len()
        )
    }
}

// ---------------------------------------------------------------------------
// Front-matter extraction
// ---------------------------------------------------------------------------

/// Extract the YAML front-matter from a markdown file.
///
/// Front-matter is delimited by `---` on its own line at the very start
/// of the file, closed by a second `---` line.  Returns the text between
/// the two delimiters (not including the delimiters themselves), or
/// `None` if the file does not start with `---`.
fn extract_frontmatter(content: &str) -> Option<&str> {
    let content = content.trim_start_matches('\u{feff}'); // strip BOM
    if !content.starts_with("---") {
        return None;
    }
    let after_first = &content[3..];
    // The first delimiter line may have trailing whitespace / newline.
    let after_first = after_first.trim_start_matches(|c: char| c == '\r');
    if !after_first.starts_with('\n') {
        return None;
    }
    let after_first = &after_first[1..];
    // Find the closing `---`.
    let end = after_first.find("\n---")?;
    Some(&after_first[..end])
}

// ---------------------------------------------------------------------------
// _md/_html pairing check
// ---------------------------------------------------------------------------

/// Recursively walk a parsed YAML tree and report any `*_md` or
/// `*_md_inline` key that does not have a corresponding `*_html` sibling
/// in the same mapping.
fn check_md_html_pairing(
    value: &serde_yaml::Value,
    file_label: &str,
    errors: &mut Vec<String>,
) {
    match value {
        serde_yaml::Value::Mapping(map) => {
            for (key, val) in map.iter() {
                let key_str = match key.as_str() {
                    Some(s) => s,
                    None => continue,
                };

                let html_key = if let Some(base) = key_str.strip_suffix("_md_inline") {
                    Some(format!("{base}_html"))
                } else if let Some(base) = key_str.strip_suffix("_md") {
                    Some(format!("{base}_html"))
                } else {
                    None
                };

                if let Some(html_key) = html_key {
                    let html_yaml_key = serde_yaml::Value::String(html_key.clone());
                    if !map.contains_key(&html_yaml_key) {
                        errors.push(format!(
                            "{file_label}: {key_str} has no {html_key} sibling"
                        ));
                    }
                }

                // Recurse into nested values.
                check_md_html_pairing(val, file_label, errors);
            }
        }
        serde_yaml::Value::Sequence(seq) => {
            for item in seq {
                check_md_html_pairing(item, file_label, errors);
            }
        }
        _ => {}
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn test_extract_frontmatter_valid() {
        let content = "---\nlayout: page.liquid\ntitle: Hello\n---\n\nBody text.\n";
        let fm = extract_frontmatter(content).unwrap();
        assert!(fm.contains("layout: page.liquid"));
        assert!(fm.contains("title: Hello"));
        assert!(!fm.contains("Body text"));
    }

    #[test]
    fn test_extract_frontmatter_no_delimiter() {
        let content = "No front-matter here.\n";
        assert!(extract_frontmatter(content).is_none());
    }

    #[test]
    fn test_extract_frontmatter_with_bom() {
        let content = "\u{feff}---\nlayout: page.liquid\n---\n\nBody.\n";
        let fm = extract_frontmatter(content).unwrap();
        assert!(fm.contains("layout: page.liquid"));
    }

    #[test]
    fn test_extract_frontmatter_no_closing() {
        let content = "---\nlayout: page.liquid\n";
        assert!(extract_frontmatter(content).is_none());
    }

    #[test]
    fn test_check_md_html_pairing_ok() {
        let yaml = r#"
title_md: "**bold**"
title_html: "<p><strong>bold</strong></p>\n"
"#;
        let doc: serde_yaml::Value = serde_yaml::from_str(yaml).unwrap();
        let mut errors = Vec::new();
        check_md_html_pairing(&doc, "test.yml", &mut errors);
        assert!(errors.is_empty(), "expected no errors, got: {errors:?}");
    }

    #[test]
    fn test_check_md_html_pairing_missing() {
        let yaml = r#"
content_md: "some markdown"
"#;
        let doc: serde_yaml::Value = serde_yaml::from_str(yaml).unwrap();
        let mut errors = Vec::new();
        check_md_html_pairing(&doc, "test.yml", &mut errors);
        assert_eq!(errors.len(), 1);
        assert!(errors[0].contains("content_md has no content_html sibling"));
    }

    #[test]
    fn test_check_md_html_pairing_inline_missing() {
        let yaml = r#"
label_md_inline: "some text"
"#;
        let doc: serde_yaml::Value = serde_yaml::from_str(yaml).unwrap();
        let mut errors = Vec::new();
        check_md_html_pairing(&doc, "test.yml", &mut errors);
        assert_eq!(errors.len(), 1);
        assert!(errors[0].contains("label_md_inline has no label_html sibling"));
    }

    #[test]
    fn test_check_md_html_pairing_nested() {
        let yaml = r#"
items:
  - name_md: "hello"
    name_html: "<p>hello</p>\n"
  - desc_md_inline: "world"
"#;
        let doc: serde_yaml::Value = serde_yaml::from_str(yaml).unwrap();
        let mut errors = Vec::new();
        check_md_html_pairing(&doc, "nested.yml", &mut errors);
        assert_eq!(errors.len(), 1);
        assert!(errors[0].contains("desc_md_inline has no desc_html sibling"));
    }

    #[test]
    fn test_run_on_temp_project() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path();

        // Create _data dir with a valid YAML file.
        let data_dir = root.join("_data");
        fs::create_dir_all(&data_dir).unwrap();
        fs::write(
            data_dir.join("test.yml"),
            "title_md: \"hello\"\ntitle_html: \"<p>hello</p>\\n\"\n",
        )
        .unwrap();

        // Create _layouts dir.
        let layouts_dir = root.join("_layouts");
        fs::create_dir_all(&layouts_dir).unwrap();
        fs::write(layouts_dir.join("page.liquid"), "{{ content }}").unwrap();

        // Create a content file with valid front-matter.
        fs::write(
            root.join("about.md"),
            "---\nlayout: page.liquid\ntitle: About\n---\n\nHello.\n",
        )
        .unwrap();

        let result = run(root);
        assert!(result.is_ok(), "expected success, got: {result:?}");
    }

    #[test]
    fn test_run_missing_layout() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path();

        // Create _data dir (empty is fine).
        fs::create_dir_all(root.join("_data")).unwrap();

        // Create _layouts dir -- but do NOT create the referenced layout.
        fs::create_dir_all(root.join("_layouts")).unwrap();

        // Content file references a layout that doesn't exist.
        fs::write(
            root.join("about.md"),
            "---\nlayout: missing.liquid\ntitle: About\n---\n\nHello.\n",
        )
        .unwrap();

        let result = run(root);
        assert!(result.is_err());
    }

    #[test]
    fn test_run_missing_frontmatter() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path();

        fs::create_dir_all(root.join("_data")).unwrap();
        fs::create_dir_all(root.join("_layouts")).unwrap();

        // Content file with no front-matter at all.
        fs::write(root.join("oops.md"), "Just some text.\n").unwrap();

        let result = run(root);
        assert!(result.is_err());
    }

    #[test]
    fn test_run_skips_readme() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path();

        fs::create_dir_all(root.join("_data")).unwrap();
        fs::create_dir_all(root.join("_layouts")).unwrap();

        // README.md should be skipped -- it has no front-matter and that's fine.
        fs::write(root.join("README.md"), "# Project\n").unwrap();

        let result = run(root);
        assert!(result.is_ok(), "README.md should be skipped: {result:?}");
    }

    #[test]
    fn test_run_plan_subdir() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path();

        fs::create_dir_all(root.join("_data")).unwrap();
        let layouts_dir = root.join("_layouts").join("plan");
        fs::create_dir_all(&layouts_dir).unwrap();
        fs::write(layouts_dir.join("page.liquid"), "{{ content }}").unwrap();

        let plan_dir = root.join("plan");
        fs::create_dir_all(&plan_dir).unwrap();
        fs::write(
            plan_dir.join("user.md"),
            "---\nlayout: plan/page.liquid\ntitle: User\n---\n\nPlan.\n",
        )
        .unwrap();

        let result = run(root);
        assert!(result.is_ok(), "expected success, got: {result:?}");
    }

    #[test]
    fn test_run_invalid_yaml_data_file() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path();

        let data_dir = root.join("_data");
        fs::create_dir_all(&data_dir).unwrap();
        // Invalid YAML: tab indentation after a mapping key is a common mistake.
        fs::write(data_dir.join("bad.yml"), "key: [\n").unwrap();

        fs::create_dir_all(root.join("_layouts")).unwrap();

        let result = run(root);
        assert!(result.is_err());
    }

    #[test]
    fn test_run_md_html_pairing_error() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path();

        let data_dir = root.join("_data");
        fs::create_dir_all(&data_dir).unwrap();
        // content_md without content_html -- prerender not run.
        fs::write(
            data_dir.join("incomplete.yml"),
            "content_md: \"hello world\"\n",
        )
        .unwrap();

        fs::create_dir_all(root.join("_layouts")).unwrap();

        let result = run(root);
        assert!(result.is_err());
    }
}
