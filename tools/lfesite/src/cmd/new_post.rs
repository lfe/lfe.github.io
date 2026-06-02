use std::fs;
use std::io::{self, Write};
use std::path::Path;
use std::process::Command;

use anyhow::{bail, Context, Result};
use chrono::{NaiveDateTime, Utc};

use crate::util::slugify;

/// Create a new blog post file with front-matter scaffolding.
pub fn run(
    project_dir: &Path,
    title: &str,
    draft: bool,
    slug_override: Option<&str>,
    category: &str,
    tags: &str,
    date_override: Option<&str>,
) -> Result<()> {
    let slug = match slug_override {
        Some(s) => {
            if !s.chars().all(|c| c.is_ascii_lowercase() || c.is_ascii_digit() || c == '-') {
                bail!("slug must match [a-z0-9-]+, got: {s}");
            }
            s.to_string()
        }
        None => slugify(title),
    };

    if slug.is_empty() {
        bail!("could not generate a slug from title: {title}");
    }

    let ts = match date_override {
        Some(d) => NaiveDateTime::parse_from_str(d, "%Y-%m-%d %H:%M")
            .with_context(|| format!("invalid date format: {d} (expected YYYY-MM-DD HH:MM)"))?,
        None => Utc::now().naive_utc(),
    };

    let year = ts.format("%Y").to_string();
    let month = ts.format("%m").to_string();
    let day = ts.format("%d").to_string();
    let hour = ts.format("%H").to_string();
    let minute = ts.format("%M").to_string();
    let hhmm = format!("{hour}{minute}");
    let published_date = format!("{year}-{month}-{day} {hour}:{minute}:00 +0000");

    let author = detect_author();

    let tag_list = if tags.is_empty() {
        "[]".to_string()
    } else {
        let items: Vec<String> = tags.split(',').map(|t| format!("\"{}\"", t.trim())).collect();
        format!("[{}]", items.join(", "))
    };

    let permalink = format!("/blog/{category}/{year}/{month}/{day}/{hhmm}-{slug}");

    let content = format!(
        r#"---
layout: post.liquid
title: "{title}"
description: ""
permalink: "{permalink}"
categories: ["{category}"]
tags: {tag_list}
published_date: {published_date}
is_draft: {draft}
data:
  author: {author}
  written_for: null
  last_validated: null
  cover_image: null
  cover_alt: null
  math: false
---

"#
    );

    let dir = project_dir.join(format!("src/posts/{year}"));
    fs::create_dir_all(&dir)
        .with_context(|| format!("creating directory: {}", dir.display()))?;

    let filename = format!("{month}-{day}-{hhmm}-{slug}.md");
    let filepath = dir.join(&filename);

    if filepath.exists() {
        bail!("file already exists: {}", filepath.display());
    }

    fs::write(&filepath, &content)
        .with_context(|| format!("writing {}", filepath.display()))?;

    let rel_path = filepath.strip_prefix(project_dir).unwrap_or(&filepath);
    eprintln!();
    eprintln!("  created: {}", rel_path.display());
    eprintln!("  status:  {}", if draft { "draft" } else { "published (use --draft for draft)" });
    eprintln!("  author:  {author}");
    eprintln!();

    // Offer to open in $EDITOR
    if let Ok(editor) = std::env::var("EDITOR") {
        eprint!("  Open post in $EDITOR? [Y/n] ");
        io::stderr().flush()?;
        let mut input = String::new();
        io::stdin().read_line(&mut input)?;
        let input = input.trim().to_lowercase();
        if input.is_empty() || input == "y" || input == "yes" {
            Command::new(&editor)
                .arg(&filepath)
                .status()
                .with_context(|| format!("failed to open {editor}"))?;
        }
    }

    Ok(())
}

fn detect_author() -> String {
    let output = Command::new("git")
        .args(["config", "user.name"])
        .output();

    match output {
        Ok(o) if o.status.success() => {
            let name = String::from_utf8_lossy(&o.stdout).trim().to_string();
            if name.is_empty() {
                eprintln!("  note: git user.name is empty, using 'unknown' as author");
                "unknown".to_string()
            } else {
                slugify(&name)
            }
        }
        _ => {
            eprintln!("  note: could not detect author from git, using 'unknown'");
            "unknown".to_string()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_detect_author_returns_something() {
        let author = detect_author();
        assert!(!author.is_empty());
    }
}
