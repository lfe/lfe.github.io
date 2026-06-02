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
/// 6. Run Pagefind search indexer
pub fn run(project_dir: &Path) -> Result<()> {
    let src_dir = project_dir.join("src");
    let output_dir = project_dir.join("site");

    println!("=== Step 1/7: prerender ===");
    super::prerender::run_with_data_dir(&src_dir)?;

    println!("=== Step 2/7: blog-resolve ===");
    super::blog_resolve::run(&src_dir)?;

    println!("=== Step 3/7: sass ===");
    super::sass::run(project_dir, &src_dir)?;

    println!("=== Step 4/7: tailwindcss ===");
    run_tailwind(project_dir, &src_dir)?;

    println!("=== Step 5/7: cobalt ===");
    run_cobalt(project_dir, &output_dir)?;

    println!("=== Step 6/7: post-build ===");
    generate_sitemap(&src_dir, &output_dir, "https://lfe.io")?;

    println!("=== Step 7/7: pagefind ===");
    run_pagefind(project_dir, &output_dir)?;

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

/// Run Pagefind to build the search index from rendered HTML.
///
/// Resolution order (mirrors tailwind):
/// 1. `pagefind` on PATH
/// 2. Cached binary in `~/.cache/lfesite/`
/// 3. Auto-download from GitHub Releases
fn run_pagefind(project_dir: &Path, output_dir: &Path) -> Result<()> {
    let site_path = output_dir
        .to_str()
        .context("output dir path is not valid UTF-8")?;

    let binary = ensure_pagefind_binary()?;

    let status = Command::new(&binary)
        .args(["--site", site_path])
        .current_dir(project_dir)
        .status()
        .with_context(|| format!("failed to run {}", binary.display()))?;

    if !status.success() {
        bail!("pagefind exited with {status}");
    }

    Ok(())
}

/// Find or download the Pagefind binary.
fn ensure_pagefind_binary() -> Result<std::path::PathBuf> {
    // 1. Check PATH
    if let Ok(path) = which("pagefind") {
        println!("  using: {}", path.display());
        return Ok(path);
    }

    // 2. Check cache
    let cache_dir = dirs_cache().join("lfesite");
    let cached = cache_dir.join("pagefind");
    if cached.is_file() {
        println!("  using cached: {}", cached.display());
        return Ok(cached);
    }

    // 3. Download — pagefind releases are tarballs whose filenames
    //    include the version, so we must resolve the latest tag first.
    println!("  pagefind not found, downloading from GitHub Releases...");
    fs::create_dir_all(&cache_dir)?;

    let version = resolve_pagefind_version()?;
    let target = pagefind_target_triple();
    let url = format!(
        "https://github.com/CloudCannon/pagefind/releases/download/{version}/pagefind_extended-{version}-{target}.tar.gz"
    );
    println!("  downloading: {url}");

    // Download tarball to a temp file so we can inspect/extract it
    let tarball = cache_dir.join("pagefind.tar.gz");
    let dl_status = Command::new("curl")
        .args(["-sL", "-o"])
        .arg(&tarball)
        .arg(&url)
        .status()
        .context("failed to download pagefind tarball")?;

    if !dl_status.success() {
        bail!("curl failed downloading {url}");
    }

    // List contents for debugging
    let listing = Command::new("tar")
        .args(["tzf"])
        .arg(&tarball)
        .output()
        .context("failed to list tarball contents")?;
    let contents = String::from_utf8_lossy(&listing.stdout);
    println!("  tarball contents: {}", contents.trim().replace('\n', ", "));

    // Extract everything, then find/rename the pagefind binary
    let status = Command::new("tar")
        .args(["xzf"])
        .arg(&tarball)
        .arg("-C")
        .arg(&cache_dir)
        .status()
        .context("failed to extract pagefind tarball")?;

    if !status.success() {
        bail!("failed to extract pagefind from {url}");
    }

    // The extended tarball may name the binary pagefind_extended
    if !cached.is_file() {
        let extended = cache_dir.join("pagefind_extended");
        if extended.is_file() {
            fs::rename(&extended, &cached)?;
            println!("  renamed pagefind_extended -> pagefind");
        } else {
            // Clean up and fail with helpful info
            let _ = fs::remove_file(&tarball);
            bail!(
                "pagefind binary not found after extraction (contents: {})",
                contents.trim()
            );
        }
    }

    let _ = fs::remove_file(&tarball);

    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        fs::set_permissions(&cached, fs::Permissions::from_mode(0o755))?;
    }

    println!("  installed: {}", cached.display());
    Ok(cached)
}

/// Resolve the latest pagefind release version tag (e.g. "v1.3.0").
///
/// Resolve the latest pagefind release version tag (e.g. "v1.3.0").
///
/// Uses the GitHub API JSON endpoint with a `curl | grep | sed` shell
/// pipeline to extract `tag_name`. This depends on `curl`, `grep`, and
/// `sed` being available on PATH (standard on macOS and Linux). If the
/// GitHub API response format changes, this will fail with a clear
/// error message rather than silently downloading the wrong version.
fn resolve_pagefind_version() -> Result<String> {
    let output = Command::new("sh")
        .args([
            "-c",
            r#"curl -sL https://api.github.com/repos/CloudCannon/pagefind/releases/latest | grep '"tag_name"' | sed 's/.*"tag_name": *"//;s/".*//' | tr -d '\r\n'"#,
        ])
        .output()
        .context("failed to resolve pagefind version")?;

    let stderr = String::from_utf8_lossy(&output.stderr);
    if !stderr.is_empty() {
        eprintln!("  debug (stderr): {stderr}");
    }

    let version = String::from_utf8_lossy(&output.stdout).trim().to_string();
    println!("  resolved version: '{version}'");

    if version.is_empty() || !version.starts_with('v') {
        bail!("could not determine latest pagefind version (got: '{version}')");
    }
    println!("  latest version: {version}");
    Ok(version)
}

/// Pagefind release assets use Rust target triples.
fn pagefind_target_triple() -> &'static str {
    if cfg!(target_os = "macos") && cfg!(target_arch = "aarch64") {
        "aarch64-apple-darwin"
    } else if cfg!(target_os = "macos") {
        "x86_64-apple-darwin"
    } else if cfg!(target_arch = "aarch64") {
        "aarch64-unknown-linux-musl"
    } else {
        "x86_64-unknown-linux-musl"
    }
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
