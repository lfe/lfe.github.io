use std::fs;
use std::path::Path;

use anyhow::{Context, Result};

/// SCSS source files to compile.
///
/// Each entry is the stem of a file in `{project_dir}/sass/`.  The
/// compiler reads `{stem}.scss` and writes `{stem}.css` into the
/// output directory.
const SCSS_FILES: &[&str] = &[
    "fontawesome",
    "fontawesome-brands",
    "fontawesome-regular",
    "fontawesome-solid",
    "fontawesome-v4-shims",
    "lfe",
];

/// Compile SCSS files to CSS.
///
/// Reads each source listed in [`SCSS_FILES`] from
/// `{project_dir}/sass/{stem}.scss`, compiles it with the `grass`
/// crate, and writes the resulting CSS to
/// `{output_dir}/{stem}.css`.
///
/// The `sass/` directory is added as a load path so that
/// `@import` directives (e.g. `@import "lfe/variables"`) resolve
/// to partials like `sass/lfe/_variables.scss`.
pub fn run(project_dir: &Path, output_dir: &Path) -> Result<()> {
    let sass_dir = project_dir.join("sass");
    if !sass_dir.is_dir() {
        anyhow::bail!("sass directory not found: {}", sass_dir.display());
    }

    fs::create_dir_all(output_dir)
        .with_context(|| format!("failed to create output directory: {}", output_dir.display()))?;

    let options = grass::Options::default().load_path(&sass_dir);

    for stem in SCSS_FILES {
        let src = sass_dir.join(format!("{stem}.scss"));
        let dst = output_dir.join(format!("{stem}.css"));

        let css = grass::from_path(&src, &options).map_err(|e| {
            anyhow::anyhow!(
                "failed to compile {}: {e}",
                src.strip_prefix(project_dir).unwrap_or(&src).display()
            )
        })?;

        fs::write(&dst, &css).with_context(|| {
            format!(
                "failed to write {}",
                dst.strip_prefix(project_dir).unwrap_or(&dst).display()
            )
        })?;

        println!(
            "  compiled: sass/{stem}.scss -> {}",
            dst.strip_prefix(project_dir).unwrap_or(&dst).display()
        );
    }

    println!();
    println!("sass: {} file(s) compiled", SCSS_FILES.len());
    Ok(())
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn test_scss_files_list_is_not_empty() {
        assert!(!SCSS_FILES.is_empty());
    }

    #[test]
    fn test_run_fails_when_sass_dir_missing() {
        let tmp = std::env::temp_dir().join("lfesite_test_sass_missing");
        let _ = fs::remove_dir_all(&tmp);
        fs::create_dir_all(&tmp).unwrap();

        let result = run(&tmp, &tmp.join("out"));
        assert!(result.is_err());
        let msg = result.unwrap_err().to_string();
        assert!(
            msg.contains("sass directory not found"),
            "unexpected error: {msg}"
        );

        let _ = fs::remove_dir_all(&tmp);
    }

    #[test]
    fn test_run_creates_output_dir() {
        // Set up a minimal sass directory with a tiny SCSS file so we
        // can verify the output directory is created on demand.  We
        // cannot test against the real project SCSS (path dependent),
        // so we just check the "sass dir missing" error fires first.
        let tmp = std::env::temp_dir().join("lfesite_test_sass_outdir");
        let _ = fs::remove_dir_all(&tmp);
        let sass = tmp.join("sass");
        fs::create_dir_all(&sass).unwrap();

        // Create a dummy file for the first entry so grass can try.
        fs::write(
            sass.join(format!("{}.scss", SCSS_FILES[0])),
            "body { color: red; }\n",
        )
        .unwrap();

        let out = tmp.join("nested").join("output");
        // The function will compile the first file, then fail on the
        // second because it is missing.  But the output dir should
        // exist by that point.
        let _ = run(&tmp, &out);
        assert!(out.is_dir(), "output directory should have been created");

        let _ = fs::remove_dir_all(&tmp);
    }
}
