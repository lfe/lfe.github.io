//! LFE and Erlang/OTP release-history data and date/version lookups.
//!
//! The single source of truth is `data/release-history.json` at the project
//! root. All lookup tables are derived here, in memory, at load time — there
//! are no materialized lookup files to drift out of sync.
//!
//! Three lookups are supported:
//!
//! 1. [`ReleaseHistory::contemporary_at`] — given a date, the nearest LFE and
//!    Erlang releases at or before it.
//! 2. [`ReleaseHistory::erlang_for_lfe`] — given an LFE version, the Erlang
//!    release contemporary with that LFE release's date.
//! 3. [`ReleaseHistory::lfe_for_erlang`] — the symmetric Erlang → LFE lookup.

use std::collections::HashMap;
use std::fs;
use std::path::Path;

use anyhow::{Context, Result};
use chrono::NaiveDate;
use serde::{Deserialize, Serialize};

/// One release of either LFE or Erlang/OTP.
///
/// A single shape serves both timelines; the language-specific fields
/// (`series`, `supports_otp`) are optional and default to empty. Default-valued
/// fields are omitted on serialization to keep `release-history.json` tidy.
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Release {
    /// Canonical display version, e.g. `"2.2.0"`, `"17.0"`, `"R16B"`.
    pub version: String,
    /// Release date.
    #[serde(deserialize_with = "de_date", serialize_with = "ser_date")]
    pub date: NaiveDate,
    /// `true` when the date is an estimate rather than a confirmed date.
    #[serde(default, skip_serializing_if = "std::ops::Not::not")]
    pub approximate: bool,
    /// `true` for tagged-but-unpublished releases (e.g. LFE 2.2.1).
    #[serde(default, skip_serializing_if = "std::ops::Not::not")]
    pub prerelease: bool,
    /// Free-text note from the release history.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub notes: Option<String>,
    /// Erlang version family: `"R"` or `"integer"` (Erlang releases only).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub series: Option<String>,
    /// OTP majors this LFE release supports (LFE releases only).
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub supports_otp: Vec<String>,
}

/// The on-disk shape of `data/release-history.json`: the two timelines plus the
/// top-level metadata, round-tripped so hand-authored fields survive edits.
#[derive(Debug, Deserialize, Serialize)]
pub struct FileHistory {
    #[serde(default = "default_schema_version")]
    pub schema_version: u32,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub note: Option<String>,
    pub lfe: Vec<Release>,
    pub erlang: Vec<Release>,
}

fn default_schema_version() -> u32 {
    1
}

impl FileHistory {
    /// Read `data/release-history.json` under `project_dir`.
    ///
    /// # Errors
    ///
    /// Returns an error if the file cannot be read or parsed.
    pub fn load(project_dir: &Path) -> Result<Self> {
        let path = history_path(project_dir);
        let raw = fs::read_to_string(&path)
            .with_context(|| format!("reading release history: {}", path.display()))?;
        serde_json::from_str(&raw)
            .with_context(|| format!("parsing release history: {}", path.display()))
    }

    /// Write `data/release-history.json` under `project_dir`, both timelines
    /// sorted ascending by date, with a trailing newline.
    ///
    /// # Errors
    ///
    /// Returns an error if the file cannot be written.
    pub fn save(&mut self, project_dir: &Path) -> Result<()> {
        self.lfe
            .sort_by(|a, b| a.date.cmp(&b.date).then(a.version.cmp(&b.version)));
        self.erlang
            .sort_by(|a, b| a.date.cmp(&b.date).then(a.version.cmp(&b.version)));
        let path = history_path(project_dir);
        let mut json = serde_json::to_string_pretty(self).context("serializing release history")?;
        json.push('\n');
        fs::write(&path, json).with_context(|| format!("writing {}", path.display()))?;
        Ok(())
    }
}

fn history_path(project_dir: &Path) -> std::path::PathBuf {
    project_dir.join("data").join("release-history.json")
}

/// Loaded, indexed release data: both timelines sorted ascending by date,
/// plus normalized version → index maps for version lookups.
#[derive(Debug)]
pub struct ReleaseHistory {
    lfe: Vec<Release>,
    erlang: Vec<Release>,
    lfe_idx: HashMap<String, usize>,
    erlang_idx: HashMap<String, usize>,
}

/// The pair of releases current at a given date.
#[derive(Debug, Clone)]
pub struct Contemporary {
    pub lfe: Option<Release>,
    pub erlang: Option<Release>,
}

impl ReleaseHistory {
    /// Load and index `data/release-history.json` under `project_dir`.
    ///
    /// # Errors
    ///
    /// Returns an error if the file cannot be read or does not parse as the
    /// expected JSON schema.
    pub fn load(project_dir: &Path) -> Result<Self> {
        let path = history_path(project_dir);
        let raw = fs::read_to_string(&path)
            .with_context(|| format!("reading release history: {}", path.display()))?;
        Self::from_json(&raw)
            .with_context(|| format!("parsing release history: {}", path.display()))
    }

    /// Build a history from raw JSON text (used by tests).
    ///
    /// # Errors
    ///
    /// Returns an error if the text does not parse as the expected schema.
    pub fn from_json(json: &str) -> Result<Self> {
        let file: FileHistory =
            serde_json::from_str(json).context("invalid release-history JSON")?;
        Ok(Self::from_file(file))
    }

    /// Build the indexed history from a parsed [`FileHistory`].
    fn from_file(file: FileHistory) -> Self {
        let mut lfe = file.lfe;
        let mut erlang = file.erlang;
        lfe.sort_by_key(|r| r.date);
        erlang.sort_by_key(|r| r.date);
        let lfe_idx = build_index(&lfe);
        let erlang_idx = build_index(&erlang);
        Self {
            lfe,
            erlang,
            lfe_idx,
            erlang_idx,
        }
    }

    /// Lookup #1: nearest LFE and Erlang releases at or before `date`.
    #[must_use]
    pub fn contemporary_at(&self, date: NaiveDate) -> Contemporary {
        Contemporary {
            lfe: self.lfe_at(date).cloned(),
            erlang: self.erlang_at(date).cloned(),
        }
    }

    /// Most recent LFE release with a date at or before `date`.
    #[must_use]
    pub fn lfe_at(&self, date: NaiveDate) -> Option<&Release> {
        nearest_at(&self.lfe, date)
    }

    /// Most recent Erlang release with a date at or before `date`.
    #[must_use]
    pub fn erlang_at(&self, date: NaiveDate) -> Option<&Release> {
        nearest_at(&self.erlang, date)
    }

    /// Lookup #2: the Erlang release contemporary with an LFE version's date.
    #[must_use]
    pub fn erlang_for_lfe(&self, lfe_version: &str) -> Option<&Release> {
        let date = self.find_lfe(lfe_version)?.date;
        self.erlang_at(date)
    }

    /// Lookup #3: the LFE release contemporary with an Erlang version's date.
    #[must_use]
    pub fn lfe_for_erlang(&self, erlang_version: &str) -> Option<&Release> {
        let date = self.find_erlang(erlang_version)?.date;
        self.lfe_at(date)
    }

    /// Resolve an LFE version string (any granularity) to its release.
    #[must_use]
    pub fn find_lfe(&self, version: &str) -> Option<&Release> {
        find(&self.lfe, &self.lfe_idx, version)
    }

    /// Resolve an Erlang version string (any granularity) to its release.
    #[must_use]
    pub fn find_erlang(&self, version: &str) -> Option<&Release> {
        find(&self.erlang, &self.erlang_idx, version)
    }

    /// The latest stable (non-prerelease, non-approximate) LFE release.
    #[must_use]
    pub fn current_lfe(&self) -> Option<&Release> {
        latest_stable(&self.lfe)
    }

    /// Regenerate `src/_data/lfe_versions.yml` from this history so the site's
    /// version banner reports the correct current release.
    ///
    /// # Errors
    ///
    /// Returns an error if the current LFE release cannot be determined or the
    /// file cannot be written.
    pub fn write_lfe_versions_yml(&self, src_dir: &Path) -> Result<()> {
        let current = self
            .current_lfe()
            .context("no stable LFE release found in history")?;
        let companion = companion_major(self.erlang_at(current.date));

        let mut out = String::new();
        out.push_str("# Generated by `lfesite sync-versions` from data/release-history.json.\n");
        out.push_str("# Do not edit by hand; edit the source of truth instead.\n");
        out.push_str("current:\n");
        out.push_str(&format!("  lfe: \"{}\"\n", current.version));
        out.push_str(&format!("  erlang: \"{companion}\"\n"));
        out.push_str(&format!("  released: \"{}\"\n", current.date));
        out.push_str("versions:\n");
        for r in self.lfe.iter().rev() {
            let erl = companion_major(self.erlang_at(r.date));
            out.push_str(&format!("  - lfe: \"{}\"\n", r.version));
            out.push_str(&format!("    erlang: \"{erl}\"\n"));
            out.push_str(&format!("    released: \"{}\"\n", r.date));
        }

        let path = src_dir.join("_data").join("lfe_versions.yml");
        fs::write(&path, &out).with_context(|| format!("writing {}", path.display()))?;
        Ok(())
    }

    /// Update the LFE download versions in `src/_data/site.yml`:
    /// `download.current_release` (latest stable LFE) and `download.prior_release`
    /// (second-latest). `pre_release`, `repository`, and all other lines are left
    /// byte-for-byte intact via targeted line replacement.
    ///
    /// # Errors
    ///
    /// Returns an error if fewer than two stable LFE releases exist, or the file
    /// cannot be read/written or lacks the expected keys.
    pub fn write_site_download_versions(&self, src_dir: &Path) -> Result<()> {
        let mut stable = self
            .lfe
            .iter()
            .rev()
            .filter(|r| !r.prerelease && !r.approximate);
        let current = stable
            .next()
            .context("no stable LFE release found in history")?;
        let prior = stable
            .next()
            .context("need at least two stable LFE releases for site.yml")?;

        let path = src_dir.join("_data").join("site.yml");
        let text =
            fs::read_to_string(&path).with_context(|| format!("reading {}", path.display()))?;
        let text = replace_scalar(&text, "current_release", &current.version)
            .with_context(|| format!("no download.current_release in {}", path.display()))?;
        let text = replace_scalar(&text, "prior_release", &prior.version)
            .with_context(|| format!("no download.prior_release in {}", path.display()))?;
        fs::write(&path, text).with_context(|| format!("writing {}", path.display()))?;
        Ok(())
    }
}

/// Replace the value of an indented `key: value` line, preserving indentation
/// and every other line. Returns `None` if the key is not found.
fn replace_scalar(text: &str, key: &str, value: &str) -> Option<String> {
    let mut found = false;
    let out: Vec<String> = text
        .lines()
        .map(|line| {
            let trimmed = line.trim_start();
            if !found && trimmed.starts_with(&format!("{key}:")) {
                found = true;
                let indent = &line[..line.len() - trimmed.len()];
                format!("{indent}{key}: {value}")
            } else {
                line.to_string()
            }
        })
        .collect();
    if !found {
        return None;
    }
    let mut joined = out.join("\n");
    if text.ends_with('\n') {
        joined.push('\n');
    }
    Some(joined)
}

/// Display form for an Erlang companion release: major only (`"29.0.2"` -> `"29"`),
/// R-series kept whole; empty when absent.
fn companion_major(release: Option<&Release>) -> String {
    release
        .map(|r| erlang_major(&r.version))
        .unwrap_or_default()
}

/// Reduce an Erlang version to its display major: integer series take the part
/// before the first `.` (`"29.0.2"` -> `"29"`, `"17.0"` -> `"17"`); R-series keep
/// their full name (`"R16B"`).
#[must_use]
pub fn erlang_major(version: &str) -> String {
    if version.starts_with(['R', 'r']) {
        version.to_string()
    } else {
        version.split('.').next().unwrap_or(version).to_string()
    }
}

/// Most recent release in a date-sorted slice with date `<= date`.
fn nearest_at(sorted: &[Release], date: NaiveDate) -> Option<&Release> {
    let i = sorted.partition_point(|r| r.date <= date);
    if i == 0 {
        None
    } else {
        Some(&sorted[i - 1])
    }
}

/// Latest non-prerelease, non-approximate release (slice sorted ascending).
fn latest_stable(sorted: &[Release]) -> Option<&Release> {
    sorted
        .iter()
        .rev()
        .find(|r| !r.prerelease && !r.approximate)
}

/// Resolve a version string against an index, most-specific key first.
fn find<'a>(
    releases: &'a [Release],
    idx: &HashMap<String, usize>,
    version: &str,
) -> Option<&'a Release> {
    normalize_keys(version)
        .iter()
        .find_map(|key| idx.get(key))
        .map(|&i| &releases[i])
}

/// Build a normalized-version → index map. When several releases share a
/// less-specific key (e.g. major `"2"`), the earliest wins, since a release
/// line "became available" at its first version.
fn build_index(sorted: &[Release]) -> HashMap<String, usize> {
    let mut idx = HashMap::new();
    for (i, r) in sorted.iter().enumerate() {
        for key in normalize_keys(&r.version) {
            idx.entry(key).or_insert(i);
        }
    }
    idx
}

/// Expand a version string into lookup keys, most-specific first.
///
/// - `"17.0"`  → `["17.0", "17"]`
/// - `"2.2.0"` → `["2.2.0", "2.2", "2"]`
/// - `"R16B"`  → `["R16B", "R16", "16"]`
fn normalize_keys(version: &str) -> Vec<String> {
    let v = version.trim();
    let mut keys = Vec::new();

    if let Some(rest) = v.strip_prefix(['R', 'r']) {
        // R-series, e.g. "R16B" → ["R16B", "R16", "16"].
        keys.push(format!("R{}", rest.to_uppercase()));
        let digits: String = rest.chars().take_while(|c| c.is_ascii_digit()).collect();
        if !digits.is_empty() {
            keys.push(format!("R{digits}"));
            keys.push(digits);
        }
        keys.dedup();
        return keys;
    }

    // Dotted numeric: progressively drop the least-significant component.
    let parts: Vec<&str> = v.split('.').collect();
    for end in (1..=parts.len()).rev() {
        keys.push(parts[..end].join("."));
    }
    keys.dedup();
    keys
}

/// Serialize a `NaiveDate` as an ISO `YYYY-MM-DD` string.
fn ser_date<S>(date: &NaiveDate, s: S) -> std::result::Result<S::Ok, S::Error>
where
    S: serde::Serializer,
{
    s.serialize_str(&date.format("%Y-%m-%d").to_string())
}

/// Deserialize a `NaiveDate` from an ISO `YYYY-MM-DD` string.
fn de_date<'de, D>(d: D) -> std::result::Result<NaiveDate, D::Error>
where
    D: serde::Deserializer<'de>,
{
    let s = String::deserialize(d)?;
    NaiveDate::parse_from_str(&s, "%Y-%m-%d").map_err(serde::de::Error::custom)
}

#[cfg(test)]
mod tests {
    use super::*;

    const FIXTURE: &str = r#"{
      "schema_version": 1,
      "lfe": [
        { "version": "0.9", "date": "2014-07-01", "approximate": true },
        { "version": "1.0.2", "date": "2016-04-17" },
        { "version": "2.2.0", "date": "2025-01-11" },
        { "version": "2.2.1", "date": "2025-06-01", "approximate": true, "prerelease": true }
      ],
      "erlang": [
        { "version": "R16B", "date": "2013-02-25", "series": "R" },
        { "version": "17.0", "date": "2014-04-07", "series": "integer" },
        { "version": "18.0", "date": "2015-06-23", "series": "integer" },
        { "version": "27.0", "date": "2024-05-17", "series": "integer" },
        { "version": "28.0", "date": "2025-05-20", "series": "integer" }
      ]
    }"#;

    fn hist() -> ReleaseHistory {
        ReleaseHistory::from_json(FIXTURE).unwrap()
    }

    fn date(s: &str) -> NaiveDate {
        NaiveDate::parse_from_str(s, "%Y-%m-%d").unwrap()
    }

    #[test]
    fn date_equal_to_release_returns_it() {
        // Exact match is included by the `<= date` semantics.
        assert_eq!(
            hist().erlang_at(date("2014-04-07")).unwrap().version,
            "17.0"
        );
    }

    #[test]
    fn date_day_before_returns_prior() {
        assert_eq!(
            hist().erlang_at(date("2014-04-06")).unwrap().version,
            "R16B"
        );
    }

    #[test]
    fn date_before_earliest_is_none() {
        assert!(hist().erlang_at(date("2000-01-01")).is_none());
        assert!(hist().lfe_at(date("2000-01-01")).is_none());
    }

    #[test]
    fn date_far_future_returns_latest() {
        assert_eq!(
            hist().erlang_at(date("2030-01-01")).unwrap().version,
            "28.0"
        );
        assert_eq!(hist().lfe_at(date("2030-01-01")).unwrap().version, "2.2.1");
    }

    #[test]
    fn contemporary_pairs_both_timelines() {
        let c = hist().contemporary_at(date("2025-02-01"));
        assert_eq!(c.lfe.unwrap().version, "2.2.0");
        assert_eq!(c.erlang.unwrap().version, "27.0");
    }

    #[test]
    fn erlang_for_lfe_uses_release_date() {
        // LFE 2.2.0 (2025-01-11) → contemporary OTP is 27.0 (28.0 ships later).
        assert_eq!(hist().erlang_for_lfe("2.2.0").unwrap().version, "27.0");
        // LFE 1.0.2 (2016-04-17) → OTP 18.0 (released 2015-06, before LFE 1.0).
        assert_eq!(hist().erlang_for_lfe("1.0.2").unwrap().version, "18.0");
    }

    #[test]
    fn lfe_for_erlang_uses_release_date() {
        // OTP 28 (2025-05-20) → LFE 2.2.0 (2.2.1 ships later, in June).
        assert_eq!(hist().lfe_for_erlang("28").unwrap().version, "2.2.0");
        // OTP 18 (2015-06-23) → LFE 0.9 (0.10 ships 2015-07).
        assert_eq!(hist().lfe_for_erlang("18").unwrap().version, "0.9");
    }

    #[test]
    fn version_lookup_multiple_granularities() {
        let h = hist();
        assert_eq!(h.find_erlang("OTP 17").map(|r| &r.version), None); // "OTP 17" not normalized here
        assert_eq!(h.find_erlang("17").unwrap().version, "17.0");
        assert_eq!(h.find_erlang("17.0").unwrap().version, "17.0");
        assert_eq!(h.find_erlang("R16B").unwrap().version, "R16B");
        assert_eq!(h.find_erlang("R16").unwrap().version, "R16B");
        assert_eq!(h.find_lfe("2.2").unwrap().version, "2.2.0"); // "2.2" → earliest 2.2.x
        assert_eq!(h.find_lfe("2.2.1").unwrap().version, "2.2.1");
    }

    #[test]
    fn current_stable_skips_prerelease_and_approximate() {
        // 2.2.1 is prerelease+approximate, so 2.2.0 is "current".
        assert_eq!(hist().current_lfe().unwrap().version, "2.2.0");
    }

    #[test]
    fn approximate_flag_propagates() {
        assert!(hist().lfe_at(date("2014-08-01")).unwrap().approximate);
        assert!(!hist().lfe_at(date("2016-05-01")).unwrap().approximate);
    }

    #[test]
    fn normalize_keys_shapes() {
        assert_eq!(normalize_keys("17.0"), vec!["17.0", "17"]);
        assert_eq!(normalize_keys("2.2.0"), vec!["2.2.0", "2.2", "2"]);
        assert_eq!(normalize_keys("R16B"), vec!["R16B", "R16", "16"]);
    }

    #[test]
    fn erlang_major_reduces_to_major() {
        assert_eq!(erlang_major("29.0.2"), "29");
        assert_eq!(erlang_major("28.5"), "28");
        assert_eq!(erlang_major("17.0"), "17");
        assert_eq!(erlang_major("R16B"), "R16B");
    }

    #[test]
    fn file_history_round_trip_omits_defaults_and_keeps_fields() {
        let json = r#"{
          "schema_version": 1,
          "note": "hello",
          "lfe": [
            { "version": "2.2.0", "date": "2025-01-11" },
            { "version": "2.2.1", "date": "2025-06-01", "approximate": true,
              "prerelease": true, "notes": "tagged" }
          ],
          "erlang": [
            { "version": "29.0", "date": "2026-05-13", "series": "integer" }
          ]
        }"#;
        let file: FileHistory = serde_json::from_str(json).unwrap();
        let out = serde_json::to_string_pretty(&file).unwrap();
        // Defaults omitted on the plain release...
        assert!(out.contains("\"version\": \"2.2.0\""));
        assert!(!out.contains("\"approximate\": false"));
        assert!(!out.contains("\"supports_otp\""));
        // ...hand-authored fields preserved.
        assert!(out.contains("\"note\": \"hello\""));
        assert!(out.contains("\"prerelease\": true"));
        assert!(out.contains("\"notes\": \"tagged\""));
        assert!(out.contains("\"series\": \"integer\""));
        assert!(out.contains("\"date\": \"2026-05-13\""));
    }

    #[test]
    fn replace_scalar_preserves_siblings() {
        let yaml = "download:\n  current_release: 2.2.0\n  pre_release: refs/heads/develop\n  repository: https://example\n";
        let out = replace_scalar(yaml, "current_release", "2.3.0").unwrap();
        assert!(out.contains("  current_release: 2.3.0\n"));
        assert!(out.contains("  pre_release: refs/heads/develop\n"));
        assert!(out.contains("  repository: https://example\n"));
        assert!(out.ends_with('\n'));
        assert!(replace_scalar(yaml, "nonexistent", "x").is_none());
    }
}
