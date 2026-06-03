# Erlang/OTP and LFE Release Reference (2008 – June 2026)

## TL;DR
- **Erlang/OTP**: Five "R-series" majors (R12B–R16B) shipped Nov 2008 → Feb 2013; modern integer majors OTP 17–29 then shipped, with OTP 22–29 all released in May of consecutive years (per endoflife.date: "a new major version is released every year in May"). **OTP 29.0 (13 May 2026)** is the latest stable as of June 2026.
- **LFE**: First announced on the erlang-questions list in **March 2008** by Robert Virding; reached **v1.0 (Apr 2016)** — InfoQ (Sergio De Simone, 5 Apr 2016): "After 8 years of development, Lisp Flavoured Erlang (LFE) has reached version 1.0, bringing stable support for Lisp programming on the Erlang virtual machine (BEAM)." After v1.3 (~2018) there was a multi-year quiet period, then a rewrite cluster produced **v2.0 (June 2021)**, **v2.1.x (2023–2024)**, and **v2.2.0 (11 Jan 2025)** — the current published version on Hex.pm as of mid-2026.
- For cross-referencing blog posts: LFE v1.x targeted Erlang R14+ through OTP 21; LFE v2.x targets OTP 21+ with full support for modern OTP (24/25/26/27/28). LFE has no calendar-aligned cadence with Erlang/OTP — many years had no LFE release.

## Key Findings
1. Erlang/OTP shifted naming from `RxxB` to integer majors with OTP 17 (April 2014), and adopted a strict annual May release cadence from OTP 22 (10 May 2019) onward.
2. LFE's release cadence is *project-driven, not calendar-driven*: long gaps (2018→2021, 2019→2023) interrupt activity bursts. Hex.pm publication dates are the most authoritative for v1.0+.
3. Several pre-1.0 LFE tags (0.6 through 0.10) exist on GitHub but precise public release dates were never indexed; estimates are derived from inferred Erlang/OTP compatibility (v0.4 dropped R12 support, v0.6.1-era ran on OTP R13B04).
4. Hex.pm publish dates and GitHub "release entry" dates diverge for several recent LFE tags (e.g., 2.1.4 = Sep 3, 2024 on Hex vs. Jan 8, 2025 on the GitHub Releases page) — likely because the GitHub Release record was created later than the tag/Hex publish. Hex.pm dates are used as canonical below.
5. The most recent LFE published version on Hex.pm is **2.2.0 (11 Jan 2025)**, but the current `lfe/lfe` README REPL banner shows **LFE v2.2.1**, suggesting a tagged-but-not-yet-Hex-published 2.2.1 exists as of mid-2026.

## Details

### Erlang/OTP major releases (2008 → 2026)

| Major version | Release date | Notable changes |
|---|---|---|
| **R12B** (last patch R12B-5) | R12B-5 = 5 Nov 2008 (R12B series began 2007) | The active R12B series during 2008; LFE was first developed against R12B-0. |
| **R13B** | 21 Apr 2009 | Significant overhaul; LFE shifted development target from R12 to R13B with its v0.4 release. |
| **R14B** | 15 Sep 2010 | New `ets` improvements, native IPv6, recursive funs, `epp` macros. |
| **R15B** | 14 Dec 2011 | Native OS time, silent process loader, halfword emulator (later deprecated); tuple funs deprecated. |
| **R16B** | 25 Feb 2013 | Removed VxWorks support; non-blocking code loading; SMP smarter scheduling; the last R-named series before integer versioning. |
| **OTP 17.0** | 7 Apr 2014 | New integer version scheme. **Maps introduced via EEP-43** (authored by Björn-Egil Dahlberg, created 4 Apr 2013, Status: Final/17.0) — experimental in 17.0. |
| **OTP 18.0** | 23 Jun 2015 | Maps stabilised. License changed to **Apache 2.0**. New time API. |
| **OTP 19.0** | 22 Jun 2016 | Dirty schedulers GA. ETS performance work. Removal of crypto deprecations. |
| **OTP 20.0** | 21 Jun 2017 | Atom Unicode support. New compiler optimisations on Core/Kernel. |
| **OTP 21.0** | 19 Jun 2018 | **New `logger` framework** replacing/wrapping `error_logger`. File I/O moved to NIFs. Stack-traces unified via `try ... catch:_:_:Stk`. |
| **OTP 22.0** | 10 May 2019 | Improved binary matching, ssl improvements, distribution carrier improvements. |
| **OTP 23.0** | 11 May 2020 | Improved compiler / SSA optimisations; SSL/TLS hardening; socket NIF. |
| **OTP 24.0** | 10 May 2021 | **BeamAsm — the JIT** (x86_64) lands by default; **EEP-48 documentation API** (`-doc` chunk); compiler ssa improvements. |
| **OTP 25.0** | 17 May 2022 | Improvements to JIT (type-based optimisations); maybe-expression (experimental). |
| **OTP 26.0** | 15 May 2023 | New shell with completion + function definitions. AArch64 JIT support. Better binary syntax codegen. Incremental Dialyzer. |
| **OTP 27.0** | 17 May 2024 | **In-source doc attributes (`-doc`)**, `maybe` expression on by default. Native coverage in JIT. `json` module. |
| **OTP 28.0** | 20 May 2025 | Latest stable in the 28 series with continued JIT/compiler improvements; new built-in 3rd-party alternatives configuration. (Patch OTP-28.5, 23 Apr 2026, added the new "Secure Coding Guidelines" document under Design Principles.) |
| **OTP 29.0** | 13 May 2026 | Per erlang.org/news/188 (Henrik Nord, 13 May 2026): "Erlang/OTP 29 is a new major release with new features, improvements as well as a few incompatibilities." Current latest stable as of June 2026. |

### LFE releases (2008 → 2026)

| Version | Release date | Notes / Erlang compatibility |
|---|---|---|
| Initial announcement | **March 2008** | Robert Virding posted to erlang-questions ML. No git tags yet; built with **Erlang R12B-0**. No Lisp shell; no support for `letrec`/`receive`/`try`/binaries. |
| First GitHub commit | Aug 2008 | Repo originally `rvirding/lfe`; tracked separate dir copies before that. |
| 0.2 | ~2008–2009 | Big change: LFE became a **Lisp-2** (functions and variables in different namespaces). First LFE shell. |
| 0.3 | ~2009–2010 | More Common Lisp-inspired forms; `lfe_boot`, `lfe_gen`. |
| 0.4 | ~2010 | "Last development version for Erlang R12B-5 and older; all future development moves to R13B." Parameterised modules; record defaults. |
| 0.5 | ~2010–2011 | Unicode binary types (utf-8/16/32); `lfe_io:format`; `list*` macro. |
| 0.6 / 0.6.1 / 0.6.2 | ~2011 | Tutorials of the era pin `v0.6.1` running on OTP R13B04 (LFE Shell V5.7.4). `(when ...)` guards; based-integer literals. |
| 0.7 / 0.7a | ~2011–2013 | Travis-CI setup; List Comprehensions; match-spec generator; variadic arithmetic/comparison macros. |
| 0.8 | ~2013–2014 | Function/macro definitions in the shell; block comments `#\| ... \|#`; `(foo:bar 1 2)` calling syntax. |
| 0.9 | ~2014–2015 | Maps support era. Targets OTP R14+. |
| 0.10 | ~2015–2016 | Final 0.x series before 1.0. |
| **1.0** (Hex 1.0.2) | **17 Apr 2016** | Per InfoQ (Sergio De Simone, 5 Apr 2016): "After 8 years of development, Lisp Flavoured Erlang (LFE) has reached version 1.0, bringing stable support for Lisp programming on the Erlang virtual machine (BEAM)." Production-stable milestone. v1.0 and v1.0.1 were tagged on GitHub but not published to Hex; the first Hex publish was 1.0.2. |
| 1.1.1 | **18 Aug 2016** | Hex.pm. |
| 1.2.0 | **27 Sep 2016** | Hex.pm. |
| 1.3 | ~2018 | Long-lived stable tag. Active through 2019–2020 (LFE Docker tutorial in May 2019 references 1.3 / 1.3-dev on OTP 18–21). Not published to Hex.pm. |
| **2.0** | **7 Jun 2021** | First release of the v2.x rewrite cluster after a multi-year gap. |
| 2.0.1 | 27 Jun 2021 (GitHub) / 26 Jun 2021 (Hex) | Bug fixes for map handling on older Erlang. |
| 2.1.0 | **2 Jan 2023** | First v2.1 release; another long gap after 2.0.1. |
| 2.1.1 | 3 Jan 2023 (Hex) / 6 Jan 2023 (GitHub) | Quickfix release; rebar3_lfe plugin handling. |
| 2.1.2 | **2 Aug 2023** | Fixes for comparison macros with 3+ args. |
| 2.1.3 | **4 Jan 2024** | New `lc/list-comp` and `bc/binary-comp` implementations in the REPL. |
| 2.1.4 | **3 Sep 2024** (Hex) / 8 Jan 2025 (GitHub Release entry) | macOS build patch. |
| 2.1.5 | **13 Sep 2024** | Fix tests with recent rebar3. |
| **2.2.0** | **11 Jan 2025** | Detection for Mix projects to start the LFE REPL in project context. Most recent Hex.pm publication. |
| 2.2.1 | (Tagged after 2.2.0; date not confirmed from Hex.pm but appears in current README REPL banner) | Likely current development tip referenced as the LFE version in lfe/lfe's README banner. |

### Aligned chronological view

| Year | Erlang/OTP era (released that year) | LFE era (released or active that year) |
|---|---|---|
| 2008 | R12B-5 (Nov) | Initial LFE announcement (Mar); first commit Aug |
| 2009 | R13B (Apr) | LFE 0.2–0.3 development |
| 2010 | R14B (Sep) | LFE 0.4–0.5 development |
| 2011 | R15B (Dec) | LFE 0.6.x circulating |
| 2012 | — (R15B patch series) | LFE 0.7 development |
| 2013 | R16B (Feb) | LFE 0.7/0.8 era |
| 2014 | OTP 17.0 (Apr; maps experimental) | LFE 0.9 era |
| 2015 | OTP 18.0 (Jun; Apache 2.0) | LFE 0.9/0.10 era |
| 2016 | OTP 19.0 (Jun) | **LFE 1.0 (Apr), 1.1.1 (Aug), 1.2.0 (Sep)** |
| 2017 | OTP 20.0 (Jun) | LFE 1.2.x (1.3 in dev) |
| 2018 | OTP 21.0 (Jun; new logger) | LFE 1.3 (approx) |
| 2019 | OTP 22.0 (May) | LFE 1.3 still current |
| 2020 | OTP 23.0 (May) | LFE 1.3 still current |
| 2021 | OTP 24.0 (May; JIT/EEP-48) | **LFE 2.0 (Jun), 2.0.1 (Jun)** |
| 2022 | OTP 25.0 (May) | (no LFE release) |
| 2023 | OTP 26.0 (May) | **LFE 2.1.0 / 2.1.1 (Jan), 2.1.2 (Aug)** |
| 2024 | OTP 27.0 (May; in-source doc attrs) | **LFE 2.1.3 (Jan), 2.1.4 (Sep), 2.1.5 (Sep)** |
| 2025 | OTP 28.0 (May) | **LFE 2.2.0 (Jan)** |
| 2026 (to date) | OTP 29.0 (13 May) | LFE 2.2.1 tagged (not yet on Hex) |

## Recommendations

For Duncan's LFE blog cross-referencing table, I recommend:

1. **Use Hex.pm publish date as the authoritative date** for any LFE version that was published to Hex.pm (1.0.2 onward). For pre-1.0 versions, use git tag dates from `git log` on a clone of `lfe/lfe` (note: the GitHub web UI's `/tags` and paginated `/releases?page=2` pages were not retrievable in this research; cloning the repo and running `git for-each-ref --sort=-creatordate --format='%(refname:short) %(creatordate)' refs/tags` will give you exact tagger dates locally).
2. **Use the Erlang/OTP `otp_src_<version>.readme` "Build date" field** (e.g. `Build date : 2008-11-05` for R12B-5) as the authoritative source for Erlang dates — these are available on the erlang.org download index and are more precise than Wikipedia.
3. **For each LFE blog post**, tag it with: (a) LFE version named in the post (or "latest" at the post date), and (b) the contemporary OTP major(s) that LFE version supported. E.g., a March 2019 post that uses LFE 1.3 would be marked "LFE 1.3 / OTP 21 era". The "aligned chronological view" table above is built for exactly this lookup.
4. **Threshold for revisiting**: If Hex.pm publishes LFE 2.2.1 or 2.3.x, update the table; if Erlang/OTP 30 ships (expected May 2027 if the May cadence holds), add it as a new row.

## Caveats

1. **Pre-1.0 LFE dates are approximate**. The hand-written `doc/src/version_history.md` in the `lfe/lfe` repo describes features for versions 0.2 through 0.8 but contains no dates. GitHub's tag/release pages disallow automated fetch from the second page onward; a local `git clone` is the only fully authoritative source for these tag dates.
2. **GitHub Release dates vs Hex.pm publish dates differ** for some 2.1.x and 2.2.x entries. The most plausible explanation is that the GitHub "Release" record was created days–months after the git tag was pushed and Hex was updated. I have used Hex.pm dates as primary; GitHub Release dates are noted where they materially differ.
3. **R12B was a multi-year series** (R12B-0 through R12B-5, 2007–2008). I have used R12B-5 (5 Nov 2008) as the representative date because that is the last R12B point release and lives in 2008, the scope-start year. If you want to mark R12B as "2007", note this is the original B-0 release.
4. **OTP 29.0 is the current latest stable** (13 May 2026 per the erlang/otp GitHub releases page and erlang.org/news/188). Subsequent OTP 29.0.x patch releases follow the normal Erlang patch cadence. Note that the new "Secure Coding Guidelines" document under Design Principles, which some sources attribute to OTP 29, in fact shipped in OTP 28.5 (Date: 2026-04-23) per the OTP-28.5 README.
5. **LFE 2.2.1**: visible in the current `lfe/lfe` README REPL banner but not yet listed on `hex.pm/packages/lfe` (which still shows 2.2.0 as latest). Treat 2.2.1 as "tagged, pending Hex publish" until the Hex page updates.
6. **No formal LFE↔OTP compatibility matrix is published** by the LFE project. Compatibility above is inferred from REPL screenshots in tutorials, CI matrices, and version_history.md notes. The current Wikipedia article "LFE (programming language)" states verbatim: "LFE is actively supported on all recent releases of Erlang; the oldest version of Erlang supported is R14." Always sanity-check with the `lfe/lfe` CI configuration for the specific version of interest.