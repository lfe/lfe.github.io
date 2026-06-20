//! Audit (and optionally fix) the `data.written_for` version banner on blog
//! posts, using the release history as the source of truth.
//!
//! Default behaviour mirrors `validate`: it checks and reports, exiting
//! non-zero when there is anything to fix, so it slots into `make check`/CI.
//! Pass `--fix` to write the computed banner into posts (filling `null`
//! values); `--fix --overwrite` also rewrites existing values that disagree.

use std::fs;
use std::path::{Path, PathBuf};

use anyhow::{bail, Context, Result};
use regex::Regex;
use walkdir::WalkDir;

use crate::releases::{Release, ReleaseHistory};
use crate::util::{replace_yaml_block, split_front_matter};

/// The `lfe`/`erlang` pair as it appears (or should appear) in front-matter.
#[derive(Debug, Clone, PartialEq, Eq)]
struct WrittenFor {
    lfe: String,
    erlang: String,
}

/// Where a computed value came from (for the report).
#[derive(Debug, Clone, Copy)]
enum Provenance {
    /// At least one version was read from the post body.
    BodyMention,
    /// Both versions inferred from the post's published date.
    DateInference,
}

/// A computed best-guess banner for a post.
#[derive(Debug, Clone)]
struct Computed {
    value: WrittenFor,
    approximate: bool,
    provenance: Provenance,
}

/// What we decided about one post.
#[derive(Debug)]
enum Outcome {
    /// `written_for` was null and we have a value to fill in.
    Fill(Computed),
    /// Existing value resolves to the same releases as computed.
    AlreadyOk,
    /// Existing value disagrees with computed.
    Mismatch {
        found: WrittenFor,
        computed: Computed,
    },
    /// Null banner, but the post shows no code/usage and no version mention.
    SkippedNoUsage,
    /// Could not determine the published date.
    SkippedNoDate,
}

/// Compiled body-scanning regexes, built once per run.
struct Scanner {
    lfe: Regex,
    erlang: Regex,
    date: Regex,
}

impl Scanner {
    fn new() -> Result<Self> {
        Ok(Self {
            // "LFE v1.3", "LFE 2.2", "LFE 0.10" (trailing -dev/.x dropped via \b).
            lfe: Regex::new(r"(?i)\bLFE\s+v?(\d+(?:\.\d+)*)\b").context("compiling LFE regex")?,
            // Integer majors require an "Erlang/OTP" keyword ("Erlang/OTP 17",
            // "Erlang 17.5", "OTP 27"); R-series tags are distinctive enough to
            // match bare ("R16B", "R12B-5", "running R13B04").
            erlang: Regex::new(
                r"(?i)(?:Erlang/OTP|Erlang|OTP)\s+([12]\d(?:\.\d+)*)\b|\b(R1\dB[A-Za-z0-9-]*)\b",
            )
            .context("compiling Erlang regex")?,
            date: Regex::new(r"(?m)^published_date:\s*(\d{4}-\d{2}-\d{2})")
                .context("compiling date regex")?,
        })
    }
}

/// Run the version-check across one post or all posts under `src/posts`.
///
/// # Errors
///
/// Returns an error (non-zero exit) when the release history cannot be loaded,
/// a file cannot be read/written, or — acting as a check — when posts still
/// need their `written_for` banner filled or corrected.
pub fn run(project_dir: &Path, file: Option<&Path>, fix: bool, overwrite: bool) -> Result<()> {
    let history = ReleaseHistory::load(project_dir)?;
    let scanner = Scanner::new()?;

    let posts = match file {
        Some(f) => vec![f.to_path_buf()],
        None => collect_posts(project_dir)?,
    };

    let mut fills = 0usize;
    let mut mismatches = 0usize;
    let mut fixed = 0usize;
    let mut unresolved = 0usize;
    let mut ok = 0usize;
    let mut skipped = 0usize;

    println!("version-check: scanning {} post(s)...", posts.len());

    for path in &posts {
        let content =
            fs::read_to_string(path).with_context(|| format!("reading {}", path.display()))?;
        let rel = path.strip_prefix(project_dir).unwrap_or(path);

        let outcome = classify(&content, &history, &scanner);
        match &outcome {
            Outcome::AlreadyOk => ok += 1,
            Outcome::SkippedNoUsage | Outcome::SkippedNoDate => skipped += 1,
            Outcome::Fill(c) => {
                fills += 1;
                if fix {
                    write_banner(path, &content, c)?;
                    fixed += 1;
                    println!("  FILLED   {} -> {}", rel.display(), describe(c));
                } else {
                    println!("  FILL     {} -> {}", rel.display(), describe(c));
                }
            }
            Outcome::Mismatch { found, computed } => {
                mismatches += 1;
                if fix && overwrite {
                    write_banner(path, &content, computed)?;
                    fixed += 1;
                    println!(
                        "  REWROTE  {} : {} -> {}",
                        rel.display(),
                        show(found),
                        describe(computed)
                    );
                } else {
                    unresolved += 1;
                    println!(
                        "  MISMATCH {} : has {}, computed {}",
                        rel.display(),
                        show(found),
                        describe(computed)
                    );
                }
            }
        }
    }

    println!();
    println!(
        "version-check: {ok} ok, {fills} to fill, {mismatches} mismatch(es), {skipped} skipped",
    );
    if fix {
        println!("version-check: {fixed} post(s) written");
    }

    let actionable = if fix { unresolved } else { fills + mismatches };
    if actionable > 0 {
        bail!(
            "{actionable} post(s) need written_for {}",
            if fix {
                "corrected (re-run with --overwrite to apply)"
            } else {
                "filled or corrected (re-run with --fix)"
            }
        );
    }
    Ok(())
}

/// Decide what to do with a single post's content.
fn classify(content: &str, history: &ReleaseHistory, scanner: &Scanner) -> Outcome {
    let (fm, body) = match split_front_matter(content) {
        Some(pair) => pair,
        None => return Outcome::SkippedNoDate,
    };

    let authored = match scanner
        .date
        .captures(&fm)
        .and_then(|c| c.get(1))
        .and_then(|m| chrono::NaiveDate::parse_from_str(m.as_str(), "%Y-%m-%d").ok())
    {
        Some(d) => d,
        None => return Outcome::SkippedNoDate,
    };

    let existing = parse_existing(&fm);
    let body_lfe = best_match(&scanner.lfe, body, authored, |v| history.find_lfe(v));
    let body_erlang = best_match(&scanner.erlang, body, authored, |v| history.find_erlang(v));

    // Eligibility: only fill posts that actually show LFE/Erlang usage.
    let has_usage = body.contains("```") || body_lfe.is_some() || body_erlang.is_some();

    let computed = compute(
        history,
        authored,
        body_lfe.as_deref(),
        body_erlang.as_deref(),
    );

    match (existing, &computed) {
        (None, Some(c)) if has_usage => Outcome::Fill(c.clone()),
        (None, _) => Outcome::SkippedNoUsage,
        (Some(found), Some(c)) => {
            if same_releases(history, &found, &c.value) {
                Outcome::AlreadyOk
            } else {
                Outcome::Mismatch {
                    found,
                    computed: c.clone(),
                }
            }
        }
        // Existing value but nothing computed (e.g. date predates all releases):
        // leave it alone.
        (Some(_), None) => Outcome::AlreadyOk,
    }
}

/// A body version mention is treated as "what the post runs on" only if it is
/// within this window of the post's date. Older mentions are historical
/// references (e.g. a 2026 post recalling "Erlang/OTP 17") and are ignored in
/// favour of date inference. Three years spans a few Erlang cycles and LFE's
/// irregular cadence without admitting decade-old call-backs.
const RECENCY_DAYS: i64 = 365 * 3;

/// Compute the best-guess banner. Each language is resolved independently: a
/// *recent* body mention wins for that language; otherwise it is inferred from
/// the post's published date. Inferring from the post date (rather than from
/// the mentioned version's release date) is more accurate — e.g. a 2019 post
/// that names "LFE 1.3" ran on the OTP current in 2019 (21), not the OTP from
/// LFE 1.3's 2018 release.
fn compute(
    history: &ReleaseHistory,
    authored: chrono::NaiveDate,
    body_lfe: Option<&str>,
    body_erlang: Option<&str>,
) -> Option<Computed> {
    let (lfe, lfe_from_body) = pick(
        body_lfe.and_then(|l| history.find_lfe(l)),
        history.lfe_at(authored),
        authored,
    );
    let (erlang, erlang_from_body) = pick(
        body_erlang.and_then(|e| history.find_erlang(e)),
        history.erlang_at(authored),
        authored,
    );
    let lfe = lfe?.clone();
    let erlang = erlang?.clone();
    let provenance = if lfe_from_body || erlang_from_body {
        Provenance::BodyMention
    } else {
        Provenance::DateInference
    };

    Some(Computed {
        approximate: lfe.approximate || erlang.approximate,
        value: WrittenFor {
            lfe: lfe.version.clone(),
            erlang: erlang_major(&erlang),
        },
        provenance,
    })
}

/// Choose between a body mention and the date-inferred release for one
/// language. The mention wins only if it resolved and is recent relative to the
/// post; otherwise the date-inferred release is used. Returns the chosen
/// release and whether it came from the body.
fn pick<'a>(
    mentioned: Option<&'a Release>,
    inferred: Option<&'a Release>,
    authored: chrono::NaiveDate,
) -> (Option<&'a Release>, bool) {
    if let Some(m) = mentioned {
        if authored.signed_duration_since(m.date).num_days() <= RECENCY_DAYS {
            return (Some(m), true);
        }
    }
    (inferred, false)
}

/// Pick the best version mention: the highest release whose date is at or
/// before the post's date (so historical references to *newer* versions don't
/// win). Returns the matched version string, validated against the history.
fn best_match<'a, F>(
    re: &Regex,
    body: &str,
    authored: chrono::NaiveDate,
    resolve: F,
) -> Option<String>
where
    F: Fn(&str) -> Option<&'a Release>,
{
    let mut best: Option<(chrono::NaiveDate, String)> = None;
    for caps in re.captures_iter(body) {
        // Use whichever capture group matched (the regex may have several
        // alternatives, e.g. integer-major vs R-series).
        let raw = match caps.iter().skip(1).flatten().next() {
            Some(m) => m.as_str(),
            None => continue,
        };
        let release = match resolve(raw) {
            Some(r) => r,
            None => continue,
        };
        if release.date > authored {
            continue;
        }
        if best.as_ref().is_none_or(|(d, _)| release.date > *d) {
            best = Some((release.date, raw.to_string()));
        }
    }
    best.map(|(_, v)| v)
}

/// Parse an existing populated `written_for` out of front-matter text.
fn parse_existing(fm: &str) -> Option<WrittenFor> {
    let value: serde_yaml::Value = serde_yaml::from_str(fm).ok()?;
    let wf = value.get("data")?.get("written_for")?;
    if wf.is_null() {
        return None;
    }
    Some(WrittenFor {
        lfe: scalar(wf.get("lfe")?)?,
        erlang: scalar(wf.get("erlang")?)?,
    })
}

/// Render a YAML scalar (string or number) as a plain string.
fn scalar(v: &serde_yaml::Value) -> Option<String> {
    match v {
        serde_yaml::Value::String(s) => Some(s.clone()),
        serde_yaml::Value::Number(n) => Some(n.to_string()),
        _ => None,
    }
}

/// True when two banners resolve to the same underlying releases (so `"1.2"`
/// and `"1.2.0"`, or `"17"` and `"17.0"`, count as equal).
fn same_releases(history: &ReleaseHistory, a: &WrittenFor, b: &WrittenFor) -> bool {
    let lfe_eq = match (history.find_lfe(&a.lfe), history.find_lfe(&b.lfe)) {
        (Some(x), Some(y)) => x.version == y.version,
        _ => a.lfe == b.lfe,
    };
    let erl_eq = match (
        history.find_erlang(&a.erlang),
        history.find_erlang(&b.erlang),
    ) {
        (Some(x), Some(y)) => x.version == y.version,
        _ => a.erlang == b.erlang,
    };
    lfe_eq && erl_eq
}

/// Erlang display form: integer majors drop `.0`; R-series keep their name.
fn erlang_major(r: &Release) -> String {
    r.version
        .strip_suffix(".0")
        .unwrap_or(&r.version)
        .to_string()
}

/// Replace the post's `written_for` node and write the file back.
fn write_banner(path: &Path, content: &str, computed: &Computed) -> Result<()> {
    let (fm, _body) = split_front_matter(content)
        .with_context(|| format!("no front-matter in {}", path.display()))?;
    let block = format!(
        "  written_for:\n    lfe: \"{}\"\n    erlang: \"{}\"",
        computed.value.lfe, computed.value.erlang
    );
    let new_fm = replace_yaml_block(&fm, "written_for", &block)
        .with_context(|| format!("no written_for field to replace in {}", path.display()))?;
    let new_content = content.replacen(&fm, &new_fm, 1);
    fs::write(path, &new_content).with_context(|| format!("writing {}", path.display()))?;
    Ok(())
}

/// Collect every `.md` post under `src/posts`.
fn collect_posts(project_dir: &Path) -> Result<Vec<PathBuf>> {
    let posts_dir = project_dir.join("src").join("posts");
    if !posts_dir.is_dir() {
        bail!("posts directory not found: {}", posts_dir.display());
    }
    let mut posts: Vec<PathBuf> = WalkDir::new(&posts_dir)
        .sort_by_file_name()
        .into_iter()
        .filter_map(std::result::Result::ok)
        .filter(|e| e.file_type().is_file())
        .map(walkdir::DirEntry::into_path)
        .filter(|p| p.extension().is_some_and(|ext| ext == "md"))
        .collect();
    posts.sort();
    Ok(posts)
}

fn describe(c: &Computed) -> String {
    let tag = match c.provenance {
        Provenance::BodyMention => "body",
        Provenance::DateInference => "date",
    };
    let approx = if c.approximate { " ~approx" } else { "" };
    format!(
        "LFE {} / OTP {} ({tag}{approx})",
        c.value.lfe, c.value.erlang
    )
}

fn show(w: &WrittenFor) -> String {
    format!("LFE {} / OTP {}", w.lfe, w.erlang)
}

#[cfg(test)]
mod tests {
    use super::*;

    const HISTORY: &str = r#"{
      "lfe": [
        { "version": "0.9", "date": "2014-07-01", "approximate": true },
        { "version": "0.10", "date": "2015-07-01", "approximate": true },
        { "version": "1.2.0", "date": "2016-09-27" },
        { "version": "2.2.0", "date": "2025-01-11" }
      ],
      "erlang": [
        { "version": "R16B", "date": "2013-02-25", "series": "R" },
        { "version": "17.0", "date": "2014-04-07", "series": "integer" },
        { "version": "18.0", "date": "2015-06-23", "series": "integer" },
        { "version": "28.0", "date": "2025-05-20", "series": "integer" }
      ]
    }"#;

    fn history() -> ReleaseHistory {
        ReleaseHistory::from_json(HISTORY).unwrap()
    }

    fn scanner() -> Scanner {
        Scanner::new().unwrap()
    }

    fn d(s: &str) -> chrono::NaiveDate {
        chrono::NaiveDate::parse_from_str(s, "%Y-%m-%d").unwrap()
    }

    #[test]
    fn erlang_regex_shapes() {
        let s = scanner();
        let auth = d("2025-12-31");
        let h = history();
        for (text, want) in [
            ("running Erlang/OTP 17 [erts-6.2]", "17.0"),
            ("on Erlang 18.0 today", "18.0"),
            ("OTP 28 is current", "28.0"),
            ("the R16B series", "R16B"),
        ] {
            let got = best_match(&s.erlang, text, auth, |v| h.find_erlang(v));
            assert_eq!(
                h.find_erlang(got.as_deref().unwrap()).unwrap().version,
                want,
                "text: {text}"
            );
        }
    }

    #[test]
    fn negative_erlang_mentions() {
        let s = scanner();
        let h = history();
        let auth = d("2025-12-31");
        for text in [
            "see the Erlang README",
            "the Erlang runtime",
            "erlang module",
        ] {
            assert!(
                best_match(&s.erlang, text, auth, |v| h.find_erlang(v)).is_none(),
                "false positive on: {text}"
            );
        }
    }

    #[test]
    fn lfe_regex_shapes() {
        let s = scanner();
        let h = history();
        let auth = d("2025-12-31");
        for (text, want) in [
            ("LFE v1.2-dev (abort with ^G)", "1.2.0"), // -dev stripped; v-prefix ok
            ("LFE 0.10 era", "0.10"),
            ("built with LFE 2.2", "2.2.0"),
        ] {
            let got = best_match(&s.lfe, text, auth, |v| h.find_lfe(v));
            assert_eq!(
                h.find_lfe(got.as_deref().unwrap()).unwrap().version,
                want,
                "text: {text}"
            );
        }
    }

    #[test]
    fn future_mentions_excluded() {
        // A 2015 post mentioning OTP 18 (2015-06) but authored before it.
        let s = scanner();
        let h = history();
        let auth = d("2015-05-24");
        let got = best_match(
            &s.erlang,
            "Erlang 17.0 ... deprecated as of v18.0",
            auth,
            |v| h.find_erlang(v),
        );
        assert_eq!(
            h.find_erlang(got.as_deref().unwrap()).unwrap().version,
            "17.0"
        );
    }

    #[test]
    fn compute_date_inference() {
        let h = history();
        let c = compute(&h, d("2016-01-01"), None, None).unwrap();
        assert_eq!(c.value.lfe, "0.10");
        assert_eq!(c.value.erlang, "18");
    }

    #[test]
    fn compute_body_mention_derives_other() {
        let h = history();
        // Erlang mentioned, LFE derived from OTP 28's date.
        let c = compute(&h, d("2025-06-01"), None, Some("28")).unwrap();
        assert_eq!(c.value.erlang, "28");
        assert_eq!(c.value.lfe, "2.2.0");
    }

    #[test]
    fn classify_fills_null_with_usage() {
        let content = "\
---
title: \"X\"
published_date: 2016-01-01 00:00:00 +0000
data:
  written_for: null
---
Body mentions ```lfe``` and Erlang/OTP 18.
";
        match classify(content, &history(), &scanner()) {
            Outcome::Fill(c) => {
                assert_eq!(c.value.erlang, "18");
            }
            other => panic!("expected Fill, got {other:?}"),
        }
    }

    #[test]
    fn classify_existing_equivalent_is_ok() {
        // Existing "18" vs computed "18.0" resolve to the same release.
        let content = "\
---
title: \"X\"
published_date: 2015-08-01 00:00:00 +0000
data:
  written_for:
    lfe: \"0.10\"
    erlang: \"18\"
---
Some Erlang/OTP 18 code: ```lfe (foo)```
";
        assert!(matches!(
            classify(content, &history(), &scanner()),
            Outcome::AlreadyOk
        ));
    }

    #[test]
    fn classify_skips_null_without_usage() {
        let content = "\
---
title: \"X\"
published_date: 2016-01-01 00:00:00 +0000
data:
  written_for: null
---
Just an announcement, no code here.
";
        assert!(matches!(
            classify(content, &history(), &scanner()),
            Outcome::SkippedNoUsage
        ));
    }

    #[test]
    fn end_to_end_fix_writes_block() {
        let dir = tempfile::tempdir().unwrap();
        let proj = dir.path();
        std::fs::create_dir_all(proj.join("data")).unwrap();
        std::fs::write(proj.join("data/release-history.json"), HISTORY).unwrap();
        let posts = proj.join("src/posts/2016");
        std::fs::create_dir_all(&posts).unwrap();
        let post = posts.join("01-01-0000-x.md");
        std::fs::write(
            &post,
            "\
---
title: \"X\"
published_date: 2016-01-01 00:00:00 +0000
data:
  author: x
  written_for: null
  last_validated: null
---
Body with ```lfe``` and Erlang/OTP 18.
",
        )
        .unwrap();

        run(proj, Some(&post), true, false).unwrap();

        let out = std::fs::read_to_string(&post).unwrap();
        assert!(out.contains("  written_for:\n    lfe: \"0.10\"\n    erlang: \"18\"\n"));
        assert!(out.contains("  last_validated: null\n"));
    }
}
