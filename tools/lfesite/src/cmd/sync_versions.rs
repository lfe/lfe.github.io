//! Regenerate `src/_data/lfe_versions.yml` from the release-history source of
//! truth so the site's version banner reports the correct current release.

use std::path::Path;

use anyhow::Result;

use crate::releases::ReleaseHistory;

/// Write `src/_data/lfe_versions.yml` from `data/release-history.json`.
///
/// # Errors
///
/// Returns an error if the release history cannot be loaded or the output file
/// cannot be written.
pub fn run(project_dir: &Path) -> Result<()> {
    let history = ReleaseHistory::load(project_dir)?;
    let src_dir = project_dir.join("src");
    history.write_lfe_versions_yml(&src_dir)?;

    if let Some(current) = history.current_lfe() {
        eprintln!();
        eprintln!(
            "  synced src/_data/lfe_versions.yml (current: LFE {})",
            current.version
        );
        eprintln!();
    }
    Ok(())
}
