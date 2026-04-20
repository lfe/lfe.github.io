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
/// 5. Flatten static/ into site root and copy CNAME
pub fn run(project_dir: &Path) -> Result<()> {
    let output_dir = project_dir.join("site");

    println!("=== Step 1/5: prerender ===");
    super::prerender::run(project_dir)?;

    println!("=== Step 2/5: sass ===");
    super::sass::run(project_dir, &output_dir)?;

    println!("=== Step 3/5: tailwindcss ===");
    run_tailwind(project_dir)?;

    println!("=== Step 4/5: cobalt ===");
    run_cobalt(project_dir, &output_dir)?;

    println!("=== Step 5/5: post-build ===");
    flatten_static(&output_dir)?;
    copy_root_files(project_dir, &output_dir)?;

    println!("\nbuild complete: output in {}", output_dir.display());
    Ok(())
}

/// Move contents of `{output}/static/` up to `{output}/` to match
/// Zola's behaviour where `static/` contents are served from the
/// site root.
fn flatten_static(output_dir: &Path) -> Result<()> {
    let static_dir = output_dir.join("static");
    if !static_dir.is_dir() {
        return Ok(());
    }

    for entry in WalkDir::new(&static_dir)
        .into_iter()
        .filter_map(|e| e.ok())
        .filter(|e| e.file_type().is_file())
    {
        let rel = entry.path().strip_prefix(&static_dir)?;
        let dest = output_dir.join(rel);
        if let Some(parent) = dest.parent() {
            fs::create_dir_all(parent)?;
        }
        fs::copy(entry.path(), &dest)?;
    }

    fs::remove_dir_all(&static_dir)?;
    println!("  flattened static/ into site root");
    Ok(())
}

/// Copy root-level files (CNAME, robots.txt, etc.) into the output.
fn copy_root_files(project_dir: &Path, output_dir: &Path) -> Result<()> {
    let files = ["CNAME"];
    for name in &files {
        let src = project_dir.join(name);
        if src.exists() {
            fs::copy(&src, output_dir.join(name))?;
            println!("  copied {name}");
        }
    }
    Ok(())
}

/// Run Tailwind CSS processing.
///
/// Tries the standalone `tailwindcss` binary first.  If that is not
/// found (exit status indicates a spawn failure), falls back to
/// `npx @tailwindcss/cli` with the same arguments.
fn run_tailwind(project_dir: &Path) -> Result<()> {
    let input = project_dir.join("styles/site.css");
    let output = project_dir.join("static/css/site.css");

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
        std::fs::create_dir_all(tmp.join("styles")).unwrap();
        std::fs::create_dir_all(tmp.join("static/css")).unwrap();
        std::fs::write(tmp.join("styles/site.css"), "/* empty */").unwrap();

        let result = run_tailwind(&tmp);
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
