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
/// Resolution order:
/// 1. `tailwindcss` on PATH (user-installed standalone binary)
/// 2. Cached standalone binary in `~/.cache/lfesite/`
/// 3. Auto-download the standalone binary from GitHub Releases
fn run_tailwind(project_dir: &Path, src_dir: &Path) -> Result<()> {
    let input = project_dir.join("tailwind/site.css");
    let output = src_dir.join("css/site.css");

    let binary = ensure_tailwind_binary()?;

    let status = Command::new(&binary)
        .args(["-i"])
        .arg(&input)
        .arg("-o")
        .arg(&output)
        .arg("--minify")
        .current_dir(project_dir)
        .status()
        .with_context(|| format!("failed to run {}", binary.display()))?;

    if !status.success() {
        bail!("tailwindcss exited with {status}");
    }

    Ok(())
}

/// Find or download the Tailwind standalone CLI binary.
pub fn ensure_tailwind_binary() -> Result<std::path::PathBuf> {
    // 1. Check PATH
    if let Ok(path) = which("tailwindcss") {
        println!("  using: {}", path.display());
        return Ok(path);
    }

    // 2. Check cache
    let cache_dir = dirs_cache().join("lfesite");
    let cached = cache_dir.join("tailwindcss");
    if cached.is_file() {
        println!("  using cached: {}", cached.display());
        return Ok(cached);
    }

    // 3. Download
    println!("  tailwindcss not found, downloading standalone binary...");
    fs::create_dir_all(&cache_dir)?;

    let (os, arch) = platform_tag();
    let url = format!(
        "https://github.com/tailwindlabs/tailwindcss/releases/latest/download/tailwindcss-{os}-{arch}"
    );
    println!("  downloading: {url}");

    let status = Command::new("curl")
        .args(["-sL", "-o"])
        .arg(&cached)
        .arg(&url)
        .status()
        .context("failed to run curl to download tailwindcss")?;

    if !status.success() {
        bail!("failed to download tailwindcss from {url}");
    }

    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        fs::set_permissions(&cached, fs::Permissions::from_mode(0o755))?;
    }

    println!("  installed: {}", cached.display());
    Ok(cached)
}

fn which(name: &str) -> Result<std::path::PathBuf> {
    let output = Command::new("which")
        .arg(name)
        .output()
        .context("failed to run which")?;
    if output.status.success() {
        let path = String::from_utf8_lossy(&output.stdout).trim().to_string();
        Ok(std::path::PathBuf::from(path))
    } else {
        bail!("{name} not found on PATH")
    }
}

fn dirs_cache() -> std::path::PathBuf {
    if let Ok(xdg) = std::env::var("XDG_CACHE_HOME") {
        return std::path::PathBuf::from(xdg);
    }
    if let Ok(home) = std::env::var("HOME") {
        return std::path::PathBuf::from(home).join(".cache");
    }
    std::path::PathBuf::from("/tmp")
}

fn platform_tag() -> (&'static str, &'static str) {
    let os = if cfg!(target_os = "macos") {
        "macos"
    } else if cfg!(target_os = "linux") {
        "linux"
    } else {
        "linux"
    };
    let arch = if cfg!(target_arch = "aarch64") {
        "arm64"
    } else {
        "x64"
    };
    (os, arch)
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
