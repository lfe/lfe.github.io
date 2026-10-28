# CC draft — Project01 / Arc01 / Slice01

**Prepared, not issued.** Check slice-plan.md Assignment history. Do not begin
until CDC records the Operator's workflow/model choice and issues the prompt.

## Task, scope and authority

Implement the real palette/value page; this is not an inventory-only return.
CDC is Sofie in the coordinating conversation. Propose S01-01 through S01-05,
leaving statuses open. Source:
`/Users/oubiwann/lab/lfe/lfe.github.io/.worktrees/colour-atlas`,
`codex/lfe-colour-atlas`, initially
036e231b09c48966da26d2227bccb4fa3b295df9.
Planning:
`/Users/oubiwann/lab/lfe/lfe.github.io/.worktrees/planning/project01-colour-atlas`.

Only the five source files and three planning-return paths in slice-plan.md
are writable. Existing CSS/layouts/config/tooling and original checkout dirty
work are protected. No push, merge, publication, OpenDesign upload/submission,
dependency/runtime install or profile/browser-preference change.
After issue, local loopback preview and scoped commits are allowed.

## Required-reading manifest

Read in order; recover truncated output before dependent work. Record complete
text coverage and data-query populations separately in artifacts/batch-record.md,
then a short source-cited contract readback (preservation, coverage, lightness vs
luminance, alpha, failure and acceptance). Once issued and coherent, proceed
without a new approval ceremony.

Required-full: planning-root AGENTS.md; project plan and ledger; arc plan and
ledger; this prompt; slice plan and ledger.
Required-full: installed visual-design-system/SKILL.md, guides/colour.md and
guides/system-evolution.md; cobalt-guidelines/SKILL.md.
Installed root:
`/Users/oubiwann/Library/Application Support/ChatGPT-Profiles/personal/codex-home/skills`.

Required-section: collaboration-framework "Notes for Codex"; engineering-methods
guides/07-implementation-prompt-authoring.md "Required reading and CC intake";
cobalt guide CB-03/08/09/32 and Summary Table; JS core ID-01/02/03 and
anti-pattern ID-01/03. Source guides also resolve under
`/Users/oubiwann/lab/billosys/ai-engineering/knowledge/`.
Do not import a Deno/Node project into browser-only work.

Required-full source at baseline: src/_includes/css.liquid,
src/_layouts/home.liquid, src/_layouts/page.liquid, src/css/highlight.css,
_cobalt.yml. Required-section: tailwind/site.css:1-115 and Makefile help/build.
Required-data: every root --gold-* and --color-* colour declaration in
src/css/site.css; all syntax-highlighter explicit colour/background declarations
with selectors. Inspect complete lossless query results, not the entire
minified line. Reference-only: legacy Sass and prior example worktrees.

## Concrete implementation spine

1. Add index.html, atlas.css, atlas.js, optional colour-math.js and README.md
   under src/design/colour-atlas/. README names source/build/local route and
   limits. Keep all existing theme files unchanged.
2. Isolate the original same-origin styles in a measurement document and read
   native CSSOM. Retain original values/names/selectors; distinguish declared
   ramp variables from actual component consumers and keep duplicate labels.
3. Render gold/foundation ramps and separate syntax paints, original coordinates,
   source/coverage text, family/lightness ordering and neutral-lightness strip.
   Derive real counts. Use Oklab/OKLCH L honestly; sRGB inputs need decoding and
   conversion. Disclose 8-bit approximation. Alpha stays unranked without a
   backdrop. Unsupported/unresolved paints stay visible, not silently black.
4. Use semantic headings, native labelled controls, visible focus, wrapping/
   stacking and no hover-only details, animation, remote assets or storage.
   Resource failure leaves explanation plus a visible failed-path/recovery message.
5. Exercise the page early, check math/populations/controls, then native build
   and preservation. Return only scoped candidates and true checks/limits.

Private helpers are local discretion; coverage/model/dependencies/production
paint are binding. No OKHSL/OKHSV label, palette correction or inferred token
represented as implemented. Browser lightness is not contrast/conformance.

## Verification and return

Run all five ledger Verify routes: make help/build, source/built route/assets,
complete palette populations, endpoint/blue-yellow/duplicate/alpha witnesses,
narrow/wide browser and keyboard/failure checks. Record browser/tool/conditions.
Do not call unavailable or unrun checks passed. Inspect all generated changes;
report generator drift without broad restoration or source expansion.

Commit only owned source paths, separately from the planning return, with:
Co-authored-by: Codex <noreply@openai.com>
Co-authored-by: Billo AI <ai-engineering@billo.systems>

Report commit/parent/path scope, per-row proposals, actual checks/failures/limits
and ownership handback. Do not close or advance the slice; independent CDC
acceptance follows. No generic test harness or capture/index/archive program.
