use std::path::Path;
use std::process::Command;
use std::sync::mpsc;
use std::time::Duration;

use anyhow::{Context, Result};
use notify::{RecursiveMode, Watcher};

/// Start a dev server with file watching.
///
/// 1. Runs a one-time prerender of `_data/**/*.yml` files.
/// 2. Spawns `cobalt serve` as a background subprocess (it has its
///    own watcher for templates and content).
/// 3. Watches `_data/` for `.yml` / `.yaml` changes and re-runs
///    prerender when a modification is detected.
/// 4. When the user presses Ctrl+C, kills the cobalt child process
///    and exits cleanly.
pub fn run(project_dir: &Path, port: u16) -> Result<()> {
    let src_dir = project_dir.join("src");

    // Step 1: initial prerender + sass
    println!("=== Initial prerender ===");
    super::prerender::run_with_data_dir(&src_dir)?;

    println!("\n=== Compiling SCSS ===");
    super::sass::run(project_dir, &src_dir)?;

    println!("\n=== Running Tailwind ===");
    let input = project_dir.join("styles/site.css");
    let output = src_dir.join("css/site.css");
    let status = Command::new("npx")
        .args(["@tailwindcss/cli", "-i"])
        .arg(&input)
        .arg("-o")
        .arg(&output)
        .arg("--minify")
        .current_dir(project_dir)
        .status();
    match status {
        Ok(s) if s.success() => println!("  Tailwind CSS updated"),
        _ => println!("  Tailwind CSS skipped (npx not available)"),
    }

    // Step 2: start cobalt serve
    println!("\n=== Starting cobalt serve on port {port} ===");
    let mut child = Command::new("cobalt")
        .args(["serve", "--port", &port.to_string()])
        .current_dir(project_dir)
        .spawn()
        .context("failed to start `cobalt serve` — is cobalt installed?")?;

    // Step 3: watch _data/ for changes
    let data_dir = src_dir.join("_data");
    if !data_dir.is_dir() {
        let _ = child.kill();
        let _ = child.wait();
        anyhow::bail!(
            "data directory not found: {}",
            data_dir.display()
        );
    }

    let (tx, rx) = mpsc::channel();

    let mut watcher = notify::recommended_watcher(
        move |res: std::result::Result<notify::Event, notify::Error>| {
            if let Ok(event) = res {
                let dominated_by_data_yaml = event.paths.iter().any(|p| {
                    p.extension()
                        .map_or(false, |ext| ext == "yml" || ext == "yaml")
                });
                if dominated_by_data_yaml && is_content_event(&event.kind) {
                    let _ = tx.send(());
                }
            }
        },
    )
    .context("failed to create filesystem watcher")?;

    watcher
        .watch(&data_dir, RecursiveMode::Recursive)
        .with_context(|| {
            format!("failed to watch directory: {}", data_dir.display())
        })?;

    println!(
        "Watching {} for changes... (Ctrl+C to stop)",
        data_dir.display()
    );

    // Step 4: event loop
    loop {
        match rx.recv_timeout(Duration::from_secs(1)) {
            Ok(()) => {
                // Debounce: drain any additional events that arrived
                // in rapid succession (e.g. editor write + rename).
                while rx.try_recv().is_ok() {}
                std::thread::sleep(Duration::from_millis(200));
                while rx.try_recv().is_ok() {}

                println!("\n=== Data file changed, re-running prerender ===");
                if let Err(e) = super::prerender::run_with_data_dir(&src_dir) {
                    eprintln!("prerender error: {e}");
                }
            }
            Err(mpsc::RecvTimeoutError::Timeout) => {
                // Check whether cobalt is still running.
                match child.try_wait() {
                    Ok(Some(status)) => {
                        println!("cobalt serve exited with {status}");
                        return Ok(());
                    }
                    Ok(None) => {} // still running
                    Err(e) => {
                        eprintln!("error checking cobalt process: {e}");
                    }
                }
            }
            Err(mpsc::RecvTimeoutError::Disconnected) => {
                break;
            }
        }
    }

    let _ = child.kill();
    let _ = child.wait();
    Ok(())
}

/// Return `true` for event kinds that indicate file content has
/// changed or a new file has appeared.
fn is_content_event(kind: &notify::EventKind) -> bool {
    use notify::EventKind;
    matches!(
        kind,
        EventKind::Create(_) | EventKind::Modify(_) | EventKind::Remove(_)
    )
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_content_event_create() {
        use notify::event::{CreateKind, EventKind};
        assert!(is_content_event(&EventKind::Create(CreateKind::File)));
    }

    #[test]
    fn test_is_content_event_modify() {
        use notify::event::{EventKind, ModifyKind};
        assert!(is_content_event(&EventKind::Modify(ModifyKind::Data(
            notify::event::DataChange::Content,
        ))));
    }

    #[test]
    fn test_is_content_event_remove() {
        use notify::event::{EventKind, RemoveKind};
        assert!(is_content_event(&EventKind::Remove(RemoveKind::File)));
    }

    #[test]
    fn test_is_content_event_access_is_false() {
        use notify::event::{AccessKind, EventKind};
        assert!(!is_content_event(&EventKind::Access(AccessKind::Read)));
    }

    #[test]
    fn test_is_content_event_other_is_false() {
        use notify::EventKind;
        assert!(!is_content_event(&EventKind::Other));
    }
}
