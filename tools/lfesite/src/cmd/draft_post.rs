use std::fs;
use std::io::{self, Write};
use std::path::Path;

use anyhow::{bail, Context, Result};

/// Convert a published post to draft, or create a new draft interactively.
pub fn run(project_dir: &Path, file: Option<&Path>) -> Result<()> {
    match file {
        Some(f) => draft_existing(f),
        None => draft_new(project_dir),
    }
}

fn draft_existing(file: &Path) -> Result<()> {
    if !file.exists() {
        bail!("file not found: {}", file.display());
    }

    let content =
        fs::read_to_string(file).with_context(|| format!("reading {}", file.display()))?;

    if content.contains("is_draft: true") {
        eprintln!();
        eprintln!("  already a draft: {}", file.display());
        return Ok(());
    }

    if !content.contains("is_draft: false") {
        bail!("no is_draft field found in {}", file.display());
    }

    let updated = content.replacen("is_draft: false", "is_draft: true", 1);
    fs::write(file, &updated).with_context(|| format!("writing {}", file.display()))?;

    let title = content
        .lines()
        .find(|l| l.starts_with("title:"))
        .map(|l| l.trim_start_matches("title:").trim().trim_matches('"'))
        .unwrap_or("(unknown)");

    eprintln!();
    eprintln!("  drafted: {}", file.display());
    eprintln!("  title:   \"{}\"", title);
    eprintln!();

    Ok(())
}

fn draft_new(project_dir: &Path) -> Result<()> {
    eprint!("  Title: ");
    io::stderr().flush()?;
    let mut title = String::new();
    io::stdin().read_line(&mut title)?;
    let title = title.trim();
    if title.is_empty() {
        bail!("title cannot be empty");
    }

    super::new_post::run(
        project_dir,
        title,
        true,        // draft
        None,        // slug
        "tutorials", // default category
        "",          // no tags
        None,        // current time
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_draft_already_draft() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("test.md");
        std::fs::write(&path, "---\ntitle: \"Test\"\nis_draft: true\n---\n").unwrap();
        run(dir.path(), Some(&path)).unwrap();
    }

    #[test]
    fn test_draft_published() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("test.md");
        std::fs::write(&path, "---\ntitle: \"Test\"\nis_draft: false\n---\n").unwrap();
        run(dir.path(), Some(&path)).unwrap();
        let content = std::fs::read_to_string(&path).unwrap();
        assert!(content.contains("is_draft: true"));
    }
}
