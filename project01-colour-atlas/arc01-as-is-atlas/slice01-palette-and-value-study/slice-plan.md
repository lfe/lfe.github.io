# Slice01 — Palette and value study

Revision 1.0. Status: prepared, not issued.

## Goal and assignment

Before: colours are scattered through active CSS. After: Duncan can inspect
the actual palette with original coordinates, grouped ramps/code paints and
labelled lightness/grayscale views. Existing site colours/pages stay unchanged.
Read project plan, arc plan, this plan and their ledgers before work.

Current draft: `cc-prompt.md`. It is not issued until the pending workflow/model
decision is recorded. Source implementation and contributor dispatch are held.

## Implementation contract

Source baseline 036e231b09c48966da26d2227bccb4fa3b295df9, branch
`codex/lfe-colour-atlas`, `.worktrees/colour-atlas`.
Allowed source files, all under `src/design/colour-atlas/`:
`index.html`, `atlas.css`, `atlas.js`, `colour-math.js`, `README.md`.
Omit an unnecessary listed module if appropriate. No existing file edits,
package/runtime installation or global tool changes.
Return paths: `artifacts/batch-record.md`, `closing-report.md` and ledger
Evidence/Notes only. CDC owns row statuses. Artifacts default to this slice's
`artifacts/`; only the concise batch record is expected, not a snapshot program.

Use plain browser ESM, semantic HTML, native labelled controls and scoped CSS.
Load original same-origin `../../css/site.css` and `../../css/highlight.css`
through native CSSOM in an isolated measurement document/iframe so site
selectors cannot style atlas chrome. Load failures must visibly identify the
resource and recovery path; do not substitute invented colours. No third-party
requests, analytics, persisted state or animation.

Enumerate all root --gold-* and --color-* colour declarations in the compiled
sheet, retaining names/authored values/provenance; separately enumerate explicit
syntax-highlighter paint declarations with selectors. Keep duplicate paints'
distinct labels/roles. Derive actual counts. Label these as declared palette,
not all visible colours on every route. Common/search/inline/image paints and
resolved state pairs are outside this slice, declared for Slice02.

Composition: short baseline/source header, gold ramp, foundation families,
separate code paints, value study and limits. Native ordering control switches
family order vs ascending Oklab lightness. Show original swatches/readable values
and parallel neutral-lightness strips. Author OKLCH L may be used directly;
hex/RGB need sRGB decoding then Oklab conversion. Label approximation if using
8-bit browser/Canvas samples. Alpha paints stay explicit and unranked without a
named backdrop; unresolved paints are reported rather than silently black.
No OKHSL/OKHSV readout or false equivalence with luminance/contrast.
Wrap labels/values and stack groups at narrow widths. No hover-only details.

## Applied guidance and author readiness

Design colour.md and system-evolution.md: preserve supplied paints, distinguish
alias/owner/consumer and do not globally replace equal literals.
Design workflow sections 1/2/4: source evidence, short brief and actual artifact.
Cobalt CB-03/08/09/32: visible source subtree and correct raw page/output route.
JS core ID-01/02/03 and anti-pattern ID-01/03: const, strict equality and nullish
defaults; browser ESM, no Node/Deno project introduced.
Testing validation-gates Proportionate Proof: native build, actual consumer and
focused numerical checks rather than a new reusable validator.

Author inspected the actual stylesheet links, root/component definitions,
generated palette, syntax paints, layout/theme mechanism and Make entrypoints.
Coverage/UI/failure/precision decisions are resolved here. CSSOM mechanics and
private helpers are CC discretion; changing scope/model/dependencies is not.

## Verification and exit

Run `make help` first, then native `make build` in the isolated source worktree.
Record actual exits/warnings and inspect generated changes. Existing CSS/config/
layout inputs must match baseline at return; if generation changes them, report
the exact discrepancy before claiming preservation. Do not stage/discard unknown
work. Use this branch's Makefile, not the original dirty checkout's variant.

Check source/built route and local assets, actual browser at narrow/wide width,
native sorting and keyboard-visible controls, labels and resource-failure state.
Compare complete declared populations, not samples. Numerical witnesses:
black/white endpoints; blue/yellow at equal HSV V must differ in Oklab L;
duplicate colour labels survive; transparency requires a backdrop.
No whole-site conformance, calibrated display or OpenDesign compatibility claim.

Return local reviewable candidate, not merged/published result. All five ledger
criteria need actual evidence and independent CDC review.

## Assignment history

- 2026-10-28: `cc-prompt.md` prepared, not issued; workflow/model choice pending.
