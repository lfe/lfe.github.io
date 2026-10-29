# Arc01 — As-is LFE atlas in OpenDesign

Revision 1.4 — 2026-10-29. Open; Slice01 Iteration01 returned for review.

## Capability and slices

Use OpenDesign + its configured GPT 6 Astra Codex CLI to produce a faithful
colour/value atlas, then extend it with role/state context. These are local HTML
design experiments, not changes to the site.

1. `slice01-palette-and-value-study`: self-contained palette/value comp in
   `artifacts/atlas.html`, reviewed for fidelity, design quality and usefulness.
   Initial pass returned; Operator approved Iteration01 adding named component
   literals and an eight-region dark-homepage usage study before slice acceptance.
2. `slice02-roles-states-and-handoff`: after acceptance, extend the atlas with a
   remaining mode/state/search role study, building on the Slice01 homepage
   context plate without duplicating it, and explicit
   inferred-vs-implemented semantics. Detail only after the first result.

Do not open/execute Slice02 automatically. Human design feedback guides the
increment. Normal corrections stay in distinct numbered slice prompts; no new
planning project per feedback message. Preserve first-pass bytes through Git
before revising them. No separate audit/validation-only arcs.

## Inputs, artifact boundary and implementation freedom

Site materials at `./`, plans under `./.worktrees/planning/`; project-plan.md
records the baseline and workflow. Read current stylesheet links and real values,
not legacy Sass or previous example themes. Declared tokens do not prove use.

OpenDesign designs the atlas composition. Specify truthful content and usability
checks without prescribing a CSS grid/card pattern, component hierarchy,
typeface or visual solution. Use the design guides to make and explain choices.
The atlas can use a neutral presentation surround; palette swatches must preserve
the site's original authored paints, alpha and distinctions. Do not silently
clip/round supplied colours into replacement source values.

Deliver a self-contained HTML file in Slice01 artifacts: inline CSS/JS and
source-derived data, no fetch back to the site or assumption that the orphan
planning tree contains site CSS. Include source paths/revision and coverage.
No image assets, network dependencies, site build, route, navigation or Cobalt
template change. Artifact-only scope is an explicit Operator-selected output
home, repeated in the slice prompt.

## Review, endpoint and gates

Technical checks cover palette/data fidelity, model-lightness calculations,
local HTML behavior, narrow/wide presentation and source preservation.
Duncan supplies aesthetic/usefulness judgment; Sofie independently checks the
actual artifact and supports the critique. Successful execution is not proof
of design quality, competitive parity or a guide's causal effect.

Stop each trial for review and the arc before redesign/integration/publication.
No Autonomous execution/internal implementer. Arc closure needs the assembled
artifact and independent composition/Operator review. Historical skill-source
fidelity does not block this site-source task.

## Version history

- 1.0 — 2026-10-28: Two-slice site-source atlas proposed, never executed.
- 1.1 — 2026-10-28: Applied Operator's OpenDesign trial workflow, artifact-only
  HTML output, design freedom and per-pass review. Native site build/integration
  moved to the future explicitly gated integration arc.

- 1.2 — 2026-10-28: Imported first return and screenshots; reproduced selected palette membership and recorded usage/context and component-literal gaps. No successor opened; acceptance remains pending.

- 1.3 — 2026-10-28: Operator requested the updated atlas with missing bits. Added the bounded homepage/literal refinement to open Slice01; retained remaining mode/state/search work in Slice02. Issued a distinct iteration prompt; no slice accepted or advanced.

- 1.4 — 2026-10-29: Imported Iteration01, preserved the first result via Git, and recorded focused numeric/data and wide-render review. Remaining behavior/acceptance gates stay open; no Slice02 opening.
