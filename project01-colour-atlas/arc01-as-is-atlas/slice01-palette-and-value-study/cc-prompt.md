# OpenDesign / Codex — LFE colour atlas, first pass

You are the executing design contributor in OpenDesign desktop. Work with the
GPT 6 Astra Codex configuration selected by the Operator; do not change it or
launch another agent. This is a design experiment, not a production site edit.

## Workspace: read here, write there

Your project directory is `/Users/oubiwann/lab/lfe/lfe.github.io`.
The current website inputs are at `./`.
Planning is a different Git worktree at `./.worktrees/planning/` on `planning`.
Do not assume those planning files exist on main or that the planning worktree
contains the site's source.

Read these exact files under `.worktrees/planning/project01-colour-atlas/`,
in order:
1. `project-plan.md` and `ledger.md`
2. `arc01-as-is-atlas/arc-plan.md` and `ledger.md`
3. `arc01-as-is-atlas/slice01-palette-and-value-study/slice-plan.md`
4. The slice's `ledger.md` and this `cc-prompt.md`

Also read `.worktrees/planning/AGENTS.md`.
Write only inside this slice:
- `artifacts/atlas.html` — the actual self-contained HTML comp.
- `artifacts/batch-record.md` — concise inputs, rationale, checks and run notes.
- `closing-report.md` — proposed return for all five ledger rows.

Do not alter site source, CSS, templates, config, Makefile, tooling, artwork,
plans, ledgers, prompts, skills or model/profile settings. The main checkout has
unrelated dirty Makefile/Rust work and a verification brief; preserve them.
Do not run a site build, commit from the site checkout, push, merge or publish.
Do not use `.worktrees/colour-atlas`; it is reserved for possible future
integration. If a write is denied, report the exact path for the Operator;
do not relocate the output or change permissions.

## Design brief

Create a designer's colour atlas of the current LFE site: an attractive, clear
working reference for understanding its existing palette and lightness
relationships before proposing any changes.

The palette is evidence, not a suggestion to improve. Keep its authored colours
and alpha values unchanged. Make the atlas composition, typography, grouping and
comparison experience thoughtful; choose and explain an approach appropriate
to this task rather than defaulting to a generic dashboard. No redesign of the
site or correction of weak colour pairs in this pass.

We should be able to inspect original values/source identities, see the declared
gold/foundation families and syntax palette, and compare lightness with a
clearly labelled neutral/value study. Distinguish a declared token from a known
consumer or an inferred role. Do not invent semantics or usage evidence.

## Apply the installed design guides

Skill directory:
`/Users/oubiwann/Library/Application Support/ChatGPT-Profiles/personal/codex-home/skills/visual-design-system/`.

Read `SKILL.md`, `guides/colour.md` and `guides/system-evolution.md` fully.
Read design-workflow.md sections 1, 2, 4 and 5, and data-and-comparison.md sections
"Define what each number means", "Choose the display for the comparison", and
"Arrange and format the values". These guide the brief, honest measures, actual
inspection and critique; you retain visual design discretion. Keep the skill's
2.0.0 input fixed for this pass; report any mismatch.
For browser JavaScript, use plain ESM/inline script, const/strict equality,
clear errors and no new dependencies. Do not introduce a Node/Deno project.

In batch-record.md, briefly note which inputs/guides you actually loaded,
their identities/sections, assumptions and the contract you are implementing.
Recover missing/truncated required content before dependent edits. Do not
replace the comp with a reading receipt or lengthy process report.

## Ground it in today's site

Baseline source commit: 036e231b09c48966da26d2227bccb4fa3b295df9.
Read `src/_includes/css.liquid`, the home/page layouts and
`tailwind/site.css:1-115` to identify the active theme.
Enumerate all root --gold-* and --color-* colour declarations in
`src/css/site.css`, and explicit colour/background paints with selectors in
`src/css/highlight.css`. Use complete bounded query results rather than
dumping the minified stylesheet's entire long line. Derive actual counts.

These are the first pass's selected population, not every paint on every route.
Retain repeated values' distinct token/selector labels. Common/search/inline/
image context and resolved state pairs belong to a later slice.
Legacy `sass/lfe/_variables.scss` and `src/lfe.css` are not this baseline.
Do not import earlier isolated LFE design examples as if they were the live site.
If relevant inputs drifted, show the discrepancy before relying on them.

## Artifact behavior

Embed the source-derived data, CSS and any JS inside `artifacts/atlas.html`.
No network requests, remote fonts/images, CSS fetches from the site root,
analytics or localStorage. The comp must open as a local HTML file and in the
OpenDesign preview without depending on the orphan planning tree containing CSS.
Keep static swatches/text useful without JavaScript; any controls enhance them.

Name the metric used by the value study. Oklab/OKLCH L is different from relative
luminance, contrast and HSV value. Do not label OKLCH as OKHSL/OKHSV. Original
OKLCH L can be read directly; hex/RGB requires sRGB decoding and Oklab conversion.
Explain approximations. Keep translucent paints explicit and do not rank their
appearance without a stated backdrop. Missing/unsupported values are unavailable,
not zero or a plausible fallback colour.

Make long names/values readable, the comparison useful at narrow/wide sizes,
and chosen controls natively labelled/keyboard operable with visible focus.
Essential information must not require hover. Use no unnecessary animation.

## Inspect, explain and return

Check complete selected source/data membership and original values. Check
black/white lightness endpoints, blue/yellow at equal HSV V having different
Oklab L, duplicate labels surviving and alpha requiring a backdrop.
Inspect the actual comp in your available preview/browser, including narrow/
wide presentation and controls. State what was really tested, its conditions,
and what was unavailable; do not install a testing stack or invent browser
observations. Native Cobalt/site build checks are not applicable here.

Return one first complete attempt, not repeated hidden self-directed refinement.
In batch-record.md include:
- short design rationale and one concrete keep/change/next-test observation;
- source/skill identities and assumptions;
- actual checks, failed attempts and unresolved limits;
- configured model and any actual model/settings/session ID or saved transcript
  path exposed by OpenDesign/Codex (otherwise "unavailable"; do not fabricate);
- Operator interventions/run count actually known, without invented timings.

Write closing-report.md with per-row proposals for S01-01 through S01-05.
Leave acceptance to Sofie and Duncan. No vendor-quality verdict or guide-causality
claim. State the exact atlas path in your final response and display/preview the
comp if OpenDesign supports it. Then stop for our visual review and feedback;
do not open another slice, edit the guides, integrate into Cobalt or publish.
