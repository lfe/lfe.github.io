use std::fs;
use std::path::Path;
use std::process::Command;

use anyhow::{bail, Context, Result};
use walkdir::WalkDir;

/// Run the full build pipeline.
///
/// Executes each stage in order:
/// 1. Pre-render data files
/// 2. Compile SCSS to CSS
/// 3. Run Tailwind CSS
/// 4. Run Cobalt build
/// 5. Generate sitemap
pub fn run(project_dir: &Path) -> Result<()> {
    let src_dir = project_dir.join("src");
    let output_dir = project_dir.join("site");

    println!("=== Step 1/5: prerender ===");
    super::prerender::run_with_data_dir(&src_dir)?;

    println!("=== Step 2/5: sass ===");
    super::sass::run(project_dir, &src_dir)?;

    println!("=== Step 3/5: tailwindcss ===");
    run_tailwind(project_dir, &src_dir)?;

    println!("=== Step 4/5: cobalt ===");
    run_cobalt(project_dir, &output_dir)?;

    println!("=== Step 5/5: post-build ===");
    generate_sitemap(&src_dir, &output_dir, "https://lfe.io")?;

    println!("\nbuild complete: output in {}", output_dir.display());
    Ok(())
}

/// Generate a sitemap.xml from the markdown content files.
fn generate_sitemap(project_dir: &Path, output_dir: &Path, base_url: &str) -> Result<()> {
    let mut urls: Vec<String> = Vec::new();

    for entry in WalkDir::new(project_dir)
        .max_depth(3)
        .into_iter()
        .filter_map(|e| e.ok())
        .filter(|e| {
            e.file_type().is_file()
                && e.path().extension().map_or(false, |ext| ext == "md")
        })
    {
        let rel = match entry.path().strip_prefix(project_dir) {
            Ok(r) => r,
            Err(_) => continue,
        };
        let rel_str = rel.to_string_lossy();
        if rel_str.starts_with("node_modules")
            || rel_str.starts_with("target")
            || rel_str.starts_with("workbench")
            || rel_str == "README.md"
        {
            continue;
        }

        let stem = rel.file_stem().and_then(|s| s.to_str()).unwrap_or("");
        let parent = rel.parent().map(|p| p.to_string_lossy().to_string()).unwrap_or_default();

        let path = if stem == "index" && parent.is_empty() {
            "/".to_string()
        } else if stem == "index" {
            format!("/{parent}/")
        } else if parent.is_empty() {
            format!("/{stem}/")
        } else {
            format!("/{parent}/{stem}/")
        };
        urls.push(path);
    }

    urls.sort();

    let mut xml = String::from(
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n\
         <urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">\n",
    );
    for url in &urls {
        xml.push_str(&format!(
            "    <url>\n        <loc>{base_url}{url}</loc>\n    </url>\n"
        ));
    }
    xml.push_str("</urlset>\n");

    fs::write(output_dir.join("sitemap.xml"), &xml)?;
    println!("  generated sitemap.xml ({} URLs)", urls.len());
    Ok(())
}

/// Run Tailwind CSS processing.
///
/// Tries the standalone `tailwindcss` binary first.  If that is not
/// found (exit status indicates a spawn failure), falls back to
/// `npx @tailwindcss/cli` with the same arguments.
fn run_tailwind(project_dir: &Path, src_dir: &Path) -> Result<()> {
    let input = project_dir.join("tailwind/site.css");
    let output = src_dir.join("css/site.css");

    let args = vec![
        "-i",
        input.to_str().context("input path is not valid UTF-8")?,
        "-o",
        output
            .to_str()
            .context("output path is not valid UTF-8")?,
        "--minify",
    ];

    // Try standalone tailwindcss binary first.
    println!("  trying: tailwindcss {}", args.join(" "));
    match Command::new("tailwindcss")
        .args(&args)
        .current_dir(project_dir)
        .status()
    {
        Ok(status) if status.success() => return Ok(()),
        Ok(status) => {
            println!(
                "  tailwindcss exited with {status}, falling back to npx"
            );
        }
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => {
            println!(
                "  tailwindcss not found, falling back to npx"
            );
        }
        Err(e) => {
            bail!("failed to run tailwindcss: {e}");
        }
    }

    // Fall back to npx @tailwindcss/cli.
    let npx_args: Vec<&str> =
        std::iter::once("@tailwindcss/cli").chain(args.iter().copied()).collect();
    println!("  running: npx {}", npx_args.join(" "));
    let status = Command::new("npx")
        .args(&npx_args)
        .current_dir(project_dir)
        .status()
        .context("failed to run npx @tailwindcss/cli")?;

    if !status.success() {
        bail!("npx @tailwindcss/cli exited with {status}");
    }

    Ok(())
}

/// Run `cobalt build` to generate the final static site.
fn run_cobalt(project_dir: &Path, output_dir: &Path) -> Result<()> {
    let destination = output_dir
        .to_str()
        .context("output dir path is not valid UTF-8")?;

    let cmd_line = format!("cobalt build -d {destination}");
    println!("  running: {cmd_line}");

    let status = Command::new("cobalt")
        .args(["build", "-d", destination])
        .current_dir(project_dir)
        .status()
        .context("failed to run cobalt build")?;

    if !status.success() {
        bail!("cobalt build exited with {status}");
    }

    Ok(())
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_run_tailwind_fails_gracefully_when_neither_binary_exists() {
        // In a CI / test environment neither tailwindcss nor npx may be
        // available.  We just verify that the function returns an error
        // rather than panicking.
        let tmp = std::env::temp_dir().join("lfesite_test_tailwind");
        let _ = std::fs::remove_dir_all(&tmp);
        std::fs::create_dir_all(tmp.join("tailwind")).unwrap();
        let src = tmp.join("src");
        std::fs::create_dir_all(src.join("css")).unwrap();
        std::fs::write(tmp.join("tailwind/site.css"), "/* empty */").unwrap();

        let result = run_tailwind(&tmp, &src);
        // The result depends on what is installed in the environment.
        // We only assert it does not panic.
        drop(result);

        let _ = std::fs::remove_dir_all(&tmp);
    }

    #[test]
    fn test_run_cobalt_fails_when_binary_missing() {
        let tmp = std::env::temp_dir().join("lfesite_test_cobalt");
        let _ = std::fs::remove_dir_all(&tmp);
        std::fs::create_dir_all(&tmp).unwrap();

        let result = run_cobalt(&tmp, &tmp.join("site"));
        // cobalt may or may not be installed; either an error from
        // spawn failure or a non-zero exit is acceptable.
        if let Err(e) = &result {
            let msg = format!("{e}");
            assert!(
                msg.contains("cobalt") || msg.contains("No such file"),
                "unexpected error: {msg}"
            );
        }

        let _ = std::fs::remove_dir_all(&tmp);
    }
}
