# Arc01 — As-is LFE colour atlas

Revision 1.0 — 2026-10-28. Open; source execution awaits workflow selection.

## Capability and slices

Deliver a locally viewable HTML reference of the current CSS palette, lightness
relationships and selected component/state pairs, with provenance and a useful
OpenDesign handoff. No production colour corrections or token migration.

1. `slice01-palette-and-value-study`: actual responsive palette/lightness page,
   original values, declared ramps and syntax paints; test its rendered use early.
2. `slice02-roles-states-and-handoff`: extend the same page with source-traced
   navigation/action/body/code/search roles, state pairs, gradients/alpha context
   and an OpenDesign handoff. Detail after Slice01 acceptance.

Both slices deliver usable page increments with source/build/browser checks.
Do not introduce research/inventory/validation-only phases, a site crawler,
generic token platform, CSS compiler or tool installation.

## Inputs, integration and semantics

Use project-plan.md's baseline, homes, workflow hold and protected changes.
`tailwind/site.css:1-115` defines gold/Pagefind scopes and component classes.
Actual layout links determine active CSS. Root token declarations do not prove
every token is visibly consumed. Distinguish declaration, observed consumer,
inferred role and unresolved mapping.

Add only `src/design/colour-atlas/`, reachable at `/design/colour-atlas/`
after the local build; no public navigation change. Keep atlas chrome separate
from measured specimens. Preserve gradients' stops/space and alpha context.
Model-based grayscale is an aid, not a calibrated physical value chart.
Slice01 does not claim OKHSL/OKHSV readouts; add those only with a verified
algorithm and checks in a later scoped increment.

## Acceptance and boundaries

Sibling ledger governs. Actual assembled page, applicable native build,
preservation diff, dispositioned limits and independent arc composition review
gate closure. Duncan owns design/taste and later concept selection.
Historical design-skill source-fidelity gaps do not block this source-based tool.
If Autonomous Mode is selected, stop after this arc; no redesign, OpenDesign
submission, merge or publication. No mode/model choice is assumed yet.

## Version history

- 1.0 — 2026-10-28: Two delivery slices with early page checks; later roles/state
  work extends the same usable artifact.
