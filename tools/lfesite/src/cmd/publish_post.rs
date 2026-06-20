use std::fs;
use std::path::Path;

use anyhow::{bail, Context, Result};

/// Publish a draft post by setting `is_draft: false` in its front matter.
pub fn run(file: &Path) -> Result<()> {
    if !file.exists() {
        bail!("file not found: {}", file.display());
    }

    let content =
        fs::read_to_string(file).with_context(|| format!("reading {}", file.display()))?;

    if content.contains("is_draft: false") {
        eprintln!();
        eprintln!("  already published: {}", file.display());
        return Ok(());
    }

    if !content.contains("is_draft: true") {
        bail!("no is_draft field found in {}", file.display());
    }

    let updated = content.replacen("is_draft: true", "is_draft: false", 1);
    fs::write(file, &updated).with_context(|| format!("writing {}", file.display()))?;

    // Extract title for display
    let title = content
        .lines()
        .find(|l| l.starts_with("title:"))
        .map(|l| l.trim_start_matches("title:").trim().trim_matches('"'))
        .unwrap_or("(unknown)");

    eprintln!();
    eprintln!("  published: {}", file.display());
    eprintln!("  title:     \"{}\"", title);
    eprintln!();

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_publish_already_published() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("test.md");
        std::fs::write(&path, "---\ntitle: \"Test\"\nis_draft: false\n---\n").unwrap();
        // Should not error, just print message
        run(&path).unwrap();
    }

    #[test]
    fn test_publish_draft() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("test.md");
        std::fs::write(&path, "---\ntitle: \"Test\"\nis_draft: true\n---\n").unwrap();
        run(&path).unwrap();
        let content = std::fs::read_to_string(&path).unwrap();
        assert!(content.contains("is_draft: false"));
    }
}
