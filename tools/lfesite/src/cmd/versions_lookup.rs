//! Ad-hoc lookups against the release history — the three cross-reference
//! queries the data model is built for. Useful for spot-checking the data and
//! for authors deciding a post's `written_for` by hand.

use std::path::Path;

use anyhow::{bail, Result};

use crate::releases::{Release, ReleaseHistory};

/// Run a single lookup, selected by which argument was provided.
///
/// - `date`: nearest LFE and Erlang releases at or before the date (lookup #1).
/// - `lfe`: an LFE version's release date and the contemporary Erlang (#2).
/// - `erlang`: an Erlang version's release date and the contemporary LFE (#3).
///
/// # Errors
///
/// Returns an error if the history cannot be loaded, no selector is given, or a
/// version/date cannot be resolved.
pub fn run(
    project_dir: &Path,
    date: Option<&str>,
    lfe: Option<&str>,
    erlang: Option<&str>,
) -> Result<()> {
    let history = ReleaseHistory::load(project_dir)?;

    match (date, lfe, erlang) {
        (Some(d), None, None) => {
            let day = chrono::NaiveDate::parse_from_str(d, "%Y-%m-%d")
                .map_err(|e| anyhow::anyhow!("invalid --date {d:?}: {e}"))?;
            let c = history.contemporary_at(day);
            println!("As of {d}:");
            println!("  LFE:    {}", describe(c.lfe.as_ref()));
            println!("  Erlang: {}", describe(c.erlang.as_ref()));
        }
        (None, Some(v), None) => {
            let rel = history
                .find_lfe(v)
                .ok_or_else(|| anyhow::anyhow!("unknown LFE version: {v}"))?;
            let erl = history.erlang_for_lfe(v);
            println!("LFE {} released {}{}", rel.version, rel.date, approx(rel));
            println!("  contemporary Erlang: {}", describe(erl));
        }
        (None, None, Some(v)) => {
            let rel = history
                .find_erlang(v)
                .ok_or_else(|| anyhow::anyhow!("unknown Erlang version: {v}"))?;
            let lfe_rel = history.lfe_for_erlang(v);
            println!(
                "Erlang {} released {}{}",
                rel.version,
                rel.date,
                approx(rel)
            );
            println!("  contemporary LFE: {}", describe(lfe_rel));
        }
        _ => bail!("provide exactly one of --date, --lfe, or --erlang"),
    }
    Ok(())
}

fn describe(release: Option<&Release>) -> String {
    match release {
        Some(r) => format!("{} ({}{})", r.version, r.date, approx(r)),
        None => "(none)".to_string(),
    }
}

fn approx(r: &Release) -> &'static str {
    if r.approximate {
        ", approx"
    } else {
        ""
    }
}
