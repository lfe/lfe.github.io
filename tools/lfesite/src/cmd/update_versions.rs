//! Pull newly-tagged LFE and Erlang/OTP releases from GitHub (via the `gh` CLI)
//! and refresh all version data:
//!
//! - `data/release-history.json` — the source of truth (new tags added);
//! - `src/_data/lfe_versions.yml` — regenerated (version banner);
//! - `src/_data/site.yml` — `download.current_release`/`prior_release` refreshed.
//!
//! Additive and idempotent: existing entries (and their hand-authored notes /
//! flags) are preserved; a re-run with no new tags is a no-op. By default only
//! releases newer than the most recent one already recorded are added, so the
//! first run tracks new tags going forward without back-filling OTP's entire
//! patch history; `--all` forces a full back-fill.

use std::path::Path;
use std::process::Command;

use anyhow::{bail, Context, Result};
use chrono::NaiveDate;
use regex::Regex;
use serde::Deserialize;

use crate::releases::{FileHistory, Release, ReleaseHistory};

const LFE_REPO: &str = "lfe/lfe";
const OTP_REPO: &str = "erlang/otp";

/// One GitHub release as returned by `gh api repos/<repo>/releases`.
#[derive(Debug, Deserialize)]
struct GhRelease {
    tag_name: String,
    #[serde(default)]
    published_at: Option<String>,
    #[serde(default)]
    prerelease: bool,
    #[serde(default)]
    draft: bool,
}

/// A release tag normalized to a version string and its release date.
#[derive(Debug, Clone, PartialEq, Eq)]
struct Fetched {
    version: String,
    date: NaiveDate,
}

/// What a merge did to one timeline.
#[derive(Debug, Default)]
struct MergeReport {
    added: Vec<Fetched>,
    promoted: Vec<String>,
    skipped_old: usize,
}

/// Fetch tags from GitHub, merge new releases into the history, and regenerate
/// the derived data files.
///
/// # Errors
///
/// Returns an error if `gh` is unavailable or fails, the history cannot be
/// read/written, or the derived files cannot be regenerated.
pub fn run(project_dir: &Path, dry_run: bool, all: bool) -> Result<()> {
    let lfe_re = Regex::new(r"^v?(\d+(?:\.\d+){0,2})$").context("compiling LFE tag regex")?;
    let otp_re = Regex::new(r"^OTP-(\d+(?:\.\d+){0,3})$").context("compiling OTP tag regex")?;

    let lfe = normalize(&fetch_releases(LFE_REPO)?, &lfe_re);
    let erlang = normalize(&fetch_releases(OTP_REPO)?, &otp_re);
    println!(
        "update-versions: fetched {} LFE and {} Erlang release tags",
        lfe.len(),
        erlang.len()
    );

    let mut file = FileHistory::load(project_dir)?;
    let lfe_report = merge(&mut file.lfe, &lfe, all, None);
    let otp_report = merge(&mut file.erlang, &erlang, all, Some("integer"));

    print_report("lfe/lfe", &lfe_report);
    print_report("erlang/otp", &otp_report);

    let changed = lfe_report.changed() || otp_report.changed();
    if dry_run {
        println!("\nupdate-versions: dry run — no files written");
        return Ok(());
    }
    if !changed {
        println!("\nupdate-versions: already up to date");
        return Ok(());
    }

    file.save(project_dir)?;
    let history = ReleaseHistory::load(project_dir)?;
    let src_dir = project_dir.join("src");
    history.write_lfe_versions_yml(&src_dir)?;
    history.write_site_download_versions(&src_dir)?;
    println!(
        "\nupdate-versions: wrote data/release-history.json, \
         src/_data/lfe_versions.yml, src/_data/site.yml"
    );
    Ok(())
}

impl MergeReport {
    fn changed(&self) -> bool {
        !self.added.is_empty() || !self.promoted.is_empty()
    }
}

/// Run `gh` and parse the NDJSON stream of releases for `repo`.
fn fetch_releases(repo: &str) -> Result<Vec<GhRelease>> {
    let endpoint = format!("repos/{repo}/releases");
    let output = Command::new("gh")
        .args([
            "api",
            &endpoint,
            "--paginate",
            "-q",
            ".[] | {tag_name, published_at, prerelease, draft}",
        ])
        .output()
        .context("running `gh` (is the GitHub CLI installed and authenticated?)")?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        bail!("`gh api {endpoint}` failed: {}", stderr.trim());
    }

    let stdout = String::from_utf8(output.stdout).context("`gh` output was not UTF-8")?;
    let mut releases = Vec::new();
    for line in stdout.lines().filter(|l| !l.trim().is_empty()) {
        let rel: GhRelease = serde_json::from_str(line)
            .with_context(|| format!("parsing gh release JSON: {line}"))?;
        releases.push(rel);
    }
    Ok(releases)
}

/// Filter and normalize raw GitHub releases into dated version entries.
/// Drafts, pre-releases (incl. `-rc`), and non-matching tags are discarded.
fn normalize(raw: &[GhRelease], re: &Regex) -> Vec<Fetched> {
    raw.iter()
        .filter(|r| !r.prerelease && !r.draft)
        .filter_map(|r| {
            let version = re.captures(&r.tag_name)?.get(1)?.as_str().to_string();
            let published = r.published_at.as_deref()?;
            let date = NaiveDate::parse_from_str(published.get(0..10)?, "%Y-%m-%d").ok()?;
            Some(Fetched { version, date })
        })
        .collect()
}

/// Merge fetched releases into an existing (unsorted) timeline.
///
/// - A version already present is left as-is, unless its entry is `prerelease`
///   or `approximate`, in which case it is *promoted* (real date, flags cleared).
/// - A new version is added when `all` is set, or its date is at or after the
///   most recent date already in the timeline; otherwise it is skipped.
///
/// `series` is stamped onto added entries (e.g. `"integer"` for Erlang).
fn merge(
    existing: &mut Vec<Release>,
    fetched: &[Fetched],
    all: bool,
    series: Option<&str>,
) -> MergeReport {
    let newest = existing.iter().map(|r| r.date).max();
    let mut report = MergeReport::default();

    for f in fetched {
        if let Some(pos) = existing.iter().position(|r| r.version == f.version) {
            let entry = &mut existing[pos];
            if entry.prerelease || entry.approximate {
                entry.date = f.date;
                entry.prerelease = false;
                entry.approximate = false;
                report.promoted.push(f.version.clone());
            }
            continue;
        }
        let recent_enough = all || newest.is_none_or(|n| f.date >= n);
        if recent_enough {
            existing.push(Release {
                version: f.version.clone(),
                date: f.date,
                approximate: false,
                prerelease: false,
                notes: None,
                series: series.map(String::from),
                supports_otp: Vec::new(),
            });
            report.added.push(f.clone());
        } else {
            report.skipped_old += 1;
        }
    }
    report
}

fn print_report(repo: &str, report: &MergeReport) {
    if !report.changed() && report.skipped_old == 0 {
        println!("  {repo}: up to date");
        return;
    }
    for f in &report.added {
        println!("  {repo}: + {} ({})", f.version, f.date);
    }
    for v in &report.promoted {
        println!("  {repo}: promoted {v} (prerelease/approx -> released)");
    }
    if report.skipped_old > 0 {
        println!(
            "  {repo}: {} older release(s) skipped (use --all to back-fill)",
            report.skipped_old
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn d(s: &str) -> NaiveDate {
        NaiveDate::parse_from_str(s, "%Y-%m-%d").unwrap()
    }

    fn rel(version: &str, date: &str) -> Release {
        Release {
            version: version.to_string(),
            date: d(date),
            approximate: false,
            prerelease: false,
            notes: None,
            series: None,
            supports_otp: Vec::new(),
        }
    }

    fn gh(tag: &str, date: &str, prerelease: bool, draft: bool) -> GhRelease {
        GhRelease {
            tag_name: tag.to_string(),
            published_at: Some(format!("{date}T00:00:00Z")),
            prerelease,
            draft,
        }
    }

    fn lfe_re() -> Regex {
        Regex::new(r"^v?(\d+(?:\.\d+){0,2})$").unwrap()
    }
    fn otp_re() -> Regex {
        Regex::new(r"^OTP-(\d+(?:\.\d+){0,3})$").unwrap()
    }

    #[test]
    fn normalize_lfe_tags() {
        let raw = vec![
            gh("v2.2.0", "2025-01-11", false, false),
            gh("2.1.1", "2023-01-06", false, false), // historical no-v
            gh("v2.0", "2021-06-07", false, false),
        ];
        let got = normalize(&raw, &lfe_re());
        let versions: Vec<&str> = got.iter().map(|f| f.version.as_str()).collect();
        assert_eq!(versions, ["2.2.0", "2.1.1", "2.0"]);
        assert_eq!(got[0].date, d("2025-01-11"));
    }

    #[test]
    fn normalize_otp_filters_noise_and_prereleases() {
        let raw = vec![
            gh("OTP-29.0", "2026-05-13", false, false),
            gh("OTP-29.0.2", "2026-06-10", false, false),
            gh("OTP-28.5", "2026-06-10", false, false),
            gh("OTP-29.0-rc1", "2026-04-01", true, false), // prerelease flag
            gh("patch-base-28", "2026-01-01", false, false),
            gh("erl_1211-bp", "2010-01-01", false, false),
            gh("R16B02_yielding_binary_to_term", "2013-01-01", false, false),
            gh("OTP-30.0", "2027-05-01", false, true), // draft
        ];
        let got = normalize(&raw, &otp_re());
        let versions: Vec<&str> = got.iter().map(|f| f.version.as_str()).collect();
        assert_eq!(versions, ["29.0", "29.0.2", "28.5"]);
    }

    #[test]
    fn merge_adds_only_newer_than_newest_by_default() {
        let mut existing = vec![rel("29.0", "2026-05-13"), rel("28.0", "2025-05-21")];
        let fetched = vec![
            Fetched {
                version: "29.0".into(),
                date: d("2026-05-13"),
            }, // present
            Fetched {
                version: "29.0.2".into(),
                date: d("2026-06-10"),
            }, // newer -> add
            Fetched {
                version: "27.5".into(),
                date: d("2025-01-01"),
            }, // older -> skip
        ];
        let report = merge(&mut existing, &fetched, false, Some("integer"));
        assert_eq!(report.added.len(), 1);
        assert_eq!(report.added[0].version, "29.0.2");
        assert_eq!(report.skipped_old, 1);
        assert!(existing.iter().any(|r| r.version == "29.0.2"));
        assert!(!existing.iter().any(|r| r.version == "27.5"));
        // series stamped on the added entry.
        let added = existing.iter().find(|r| r.version == "29.0.2").unwrap();
        assert_eq!(added.series.as_deref(), Some("integer"));
    }

    #[test]
    fn merge_all_backfills_old_releases() {
        let mut existing = vec![rel("29.0", "2026-05-13")];
        let fetched = vec![Fetched {
            version: "27.5".into(),
            date: d("2025-01-01"),
        }];
        let report = merge(&mut existing, &fetched, true, Some("integer"));
        assert_eq!(report.added.len(), 1);
        assert_eq!(report.skipped_old, 0);
    }

    #[test]
    fn merge_promotes_prerelease_entry() {
        let mut existing = vec![Release {
            version: "2.2.1".into(),
            date: d("2025-06-01"),
            approximate: true,
            prerelease: true,
            notes: Some("tagged, pending Hex".into()),
            series: None,
            supports_otp: Vec::new(),
        }];
        let fetched = vec![Fetched {
            version: "2.2.1".into(),
            date: d("2026-02-01"),
        }];
        let report = merge(&mut existing, &fetched, false, None);
        assert_eq!(report.promoted, ["2.2.1"]);
        let e = &existing[0];
        assert_eq!(e.date, d("2026-02-01"));
        assert!(!e.prerelease && !e.approximate);
        assert_eq!(e.notes.as_deref(), Some("tagged, pending Hex")); // note preserved
    }

    #[test]
    fn merge_is_idempotent() {
        let mut existing = vec![rel("2.2.0", "2025-01-11")];
        let fetched = vec![Fetched {
            version: "2.2.0".into(),
            date: d("2025-01-11"),
        }];
        let report = merge(&mut existing, &fetched, false, None);
        assert!(!report.changed());
        assert_eq!(existing.len(), 1);
    }
}
