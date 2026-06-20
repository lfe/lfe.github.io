use std::path::Path;
use std::process::{Child, Command, Stdio};
use std::sync::mpsc;
use std::time::Duration;

use anyhow::{Context, Result};
use notify::{RecursiveMode, Watcher};

/// Start a dev server with file watching.
///
/// 1. Runs the full build pipeline (prerender → sass → tailwind →
///    cobalt build → sitemap → pagefind).
/// 2. Starts a static file server on the output directory.
/// 3. Watches source files for changes and re-runs the full pipeline
///    on each change.
///
/// This avoids the dual-build-system conflict between `cobalt serve`
/// (which wipes `site/` on rebuild) and pagefind (which writes into
/// `site/pagefind/`).
pub fn run(project_dir: &Path, port: u16) -> Result<()> {
    let output_dir = project_dir.join("site");

    // Step 1: initial full build
    println!("=== Initial build ===");
    super::build::run(project_dir)?;

    // Step 2: start static file server
    println!("\n=== Starting dev server on port {port} ===");
    let mut server = start_server(&output_dir, port)?;
    println!(
        "  serving {} at http://localhost:{port}",
        output_dir.display()
    );

    // Step 3: watch src/ for changes
    let src_dir = project_dir.join("src");
    let tailwind_dir = project_dir.join("tailwind");

    let (tx, rx) = mpsc::channel();

    let mut watcher = notify::recommended_watcher(
        move |res: std::result::Result<notify::Event, notify::Error>| {
            if let Ok(event) = res {
                if is_content_event(&event.kind) {
                    let _ = tx.send(());
                }
            }
        },
    )
    .context("failed to create filesystem watcher")?;

    watcher
        .watch(&src_dir, RecursiveMode::Recursive)
        .with_context(|| format!("failed to watch {}", src_dir.display()))?;

    if tailwind_dir.is_dir() {
        watcher
            .watch(&tailwind_dir, RecursiveMode::Recursive)
            .with_context(|| format!("failed to watch {}", tailwind_dir.display()))?;
    }

    println!("Watching for changes... (Ctrl+C to stop)\n");

    // Step 4: event loop — rebuild on changes
    loop {
        match rx.recv_timeout(Duration::from_secs(1)) {
            Ok(()) => {
                while rx.try_recv().is_ok() {}
                std::thread::sleep(Duration::from_millis(300));
                while rx.try_recv().is_ok() {}

                println!("\n=== File changed, rebuilding ===");
                if let Err(e) = super::build::run(project_dir) {
                    eprintln!("rebuild error: {e}");
                } else {
                    println!("=== Rebuild complete ===\n");
                }
            }
            Err(mpsc::RecvTimeoutError::Timeout) => match server.try_wait() {
                Ok(Some(status)) => {
                    println!("dev server exited with {status}");
                    return Ok(());
                }
                Ok(None) => {}
                Err(e) => {
                    eprintln!("error checking server process: {e}");
                }
            },
            Err(mpsc::RecvTimeoutError::Disconnected) => {
                break;
            }
        }
    }

    let _ = server.kill();
    let _ = server.wait();
    Ok(())
}

/// Start a static file server for the output directory.
fn start_server(output_dir: &Path, port: u16) -> Result<Child> {
    let dir = output_dir
        .to_str()
        .context("output dir path is not valid UTF-8")?;

    Command::new("python3")
        .args(["-m", "http.server", &port.to_string(), "-d", dir])
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .spawn()
        .context("failed to start python3 http.server — is python3 installed?")
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
