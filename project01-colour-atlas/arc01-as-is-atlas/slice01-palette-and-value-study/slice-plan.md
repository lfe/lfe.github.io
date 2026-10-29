# Slice01 — First OpenDesign palette/value trial

Revision 1.4. Status: Iteration01 imported; independent review in progress.

## Brief and active assignment

Duncan wants a designer's usable view of the current LFE palette and its
lightness relationships, as the first small trial of visual-design-system
guidance through OpenDesign/GPT. Produce the actual HTML comp, then stop.
The site's paints are evidence to preserve; the atlas's composition is a design
decision OpenDesign should make using the supplied guides.

Current assignment: `cc-prompt-iteration01.md`, issued 2026-10-28 at the
Operator's request for an updated atlas including the missing bits.
The initial `cc-prompt.md` and all first-return files remain preserved.
CC: OpenDesign desktop using its configured Codex CLI / GPT 6 Astra, as reported
by Duncan. Reasoning setting/backend identity not independently verified.
CDC: Sofie here; Operator launches/relays and provides visual review. No
Autonomous Mode, internal CC or automatic next slice.

## Input and output contracts

OpenDesign workspace: `/Users/oubiwann/lab/lfe/lfe.github.io`.
Site materials at `./`; planning at `./.worktrees/planning/`.
Source baseline: 036e231b09c48966da26d2227bccb4fa3b295df9. Confirm relevant
inputs still match; report drift instead of silently selecting another design.
Unrelated Makefile/Rust work and verification brief are read-only/protected.

Canonical return homes, relative to this slice (OpenDesign writes project-local
copies for ZIP export; CDC imports them):
- `artifacts/atlas.html`: self-contained HTML comp with inline CSS/JS/data.
- `artifacts/batch-record.md`: concise run/inputs/checks/limits/decision record.
- `closing-report.md`: proposed-done return, all six current rows accounted for.

This is the Operator's explicit prototype-delivery home. Do not put the comp in
site `src/`, do not use the unused `.worktrees/colour-atlas` worktree, and do not
modify source/CSS/templates/config/tools, plans, ledgers, prompts, skills or
model/profile settings. No push/merge/deployment/subagents/automatic follow-up.
CC should not commit from the dirty source checkout; Sofie records the returned
owned files on the planning branch before revision.

## Design and data

Read the design skill and relevant colour/system-evolution guidance. Inspect
current stylesheet links and the live-theme sources. Present all declared root
gold/Tailwind colour variables from `src/css/site.css`, and the syntax
highlighter's explicit colour/background paints from `src/css/highlight.css`.
Iteration01's additional scope below extends this original selection.
Keep token/selector names, original authored values, source identity and
distinct labels for repeated values. Derive counts. Label declared palette
rather than claiming every declared token has a rendered consumer.

Use source-derived data embedded in the HTML. No network requests or runtime
site-CSS fetches: the planning checkout is orphaned, and OpenDesign preview may
not serve the site root. The result should also open directly as a local file.
No remote fonts/assets/analytics/storage. Static swatches/text remain useful
without JS; interactions may enhance them. Missing/unsupported data is labelled,
not replaced with a plausible colour or a zero.

Show a meaningful lightness/value study with neutral counterparts. Name the
actual metric: Oklab/OKLCH L differs from luminance, contrast and HSV V.
Preserve alpha and background assumptions; rank opaque paints only unless a
backdrop is explicit. Author OKLCH L can be used directly; hex/RGB needs sRGB
decoding and Oklab conversion. Disclose browser/8-bit approximation if used.
Do not label OKLCH as OKHSL/OKHSV or fabricate those readouts.

The visual arrangement, typography, sequencing and useful small controls are
OpenDesign decisions. Make original values readable, keep comparison meaningful,
allow long labels to wrap, and make chosen controls labelled/keyboard operable
with visible focus. No hover-only essential content. No automatic palette fixes.

## Small trial and review criteria

This is one developmental first-use trial, not an A/B test or vendor ranking.
Keep the guide version, source baseline and task stable for this pass. Record
actual guide sections used, tool/model/settings if exposed, assumptions and
any OpenDesign/Codex session/transcript pointer actually available. Unknown is
acceptable; do not create telemetry or claim a monitoring connection.

Predeclared review:
- Fidelity: can we trace swatches/labels/metrics to the selected current inputs?
- Design: does hierarchy, spacing, type and optical balance help compare rather
  than decorate or obscure the colours?
- Usefulness: can Duncan identify a colour's original value/source and compare
  lightness without decoding an unlabelled dashboard?
- Behavior: readable narrow/wide, labelled keyboard controls if present,
  no invented values, clear alpha/unsupported states.
- Effort: what did Duncan have to explain/correct; what run/feedback count
  actually occurred? Do not invent time measurements.

Record a brief design rationale and self-critique with one keep/change/next-test
observation. Do not run repeated self-refinements to claim success. Return the
first complete attempt, with any unresolved defects; stop for our review.
Duncan's visual verdict and independent CDC checks remain pending.

## Checks and author readiness

Check the complete selected token/syntax population against source, not a
sample. Numerical witnesses: black/white endpoints; blue/yellow at equal HSV V
must differ in Oklab L; duplicate labels survive; alpha needs a backdrop.
Use actual HTML preview/browser checks available in OpenDesign, report sizes/
conditions and distinguish observed from unrun. Do not install tools or create
a generic test harness. `make build` is not applicable to this artifact-only
slice; no site routes/assets are added.

CDC inspected current CSS links/root gold/components, generated palette, syntax
paints, layout and theme mechanism. Design colour/system-evolution preserve
supplied values and distinguish owners/consumers. Design workflow/data comparison
guides support a task-led hierarchy and truthful measures without fixing a
visual solution. Scientific-methods framing/measures/confounds/protocol guidance
supports a qualitative first pass, preserved inputs and explicit limits.
Source integration/Cobalt checks belong to a future issued assignment.


## Operator-approved Iteration01 amendment

The 2026-10-28 request approves a bounded refinement inside this still-open
slice: preserve all 95 first-pass records, add the named tab/header/card-action
literal declarations, and add eight dark-homepage colour-context specimens.
This supersedes earlier broad deferral of all component literals/role context
to Slice02 only for the listed scope. It does not accept the first pass or
open the next slice.

The current prompt is the detailed implementation/design brief:
`cc-prompt-iteration01.md`. It covers header, main, masthead, code tabs/surround,
feature card, video card, closing/CTA and footer; actual gradients/pairs;
existing-vs-inferred role mapping; source/cascade qualification; image exclusion;
and unchanged source paints. Composition remains OpenDesign's judgment.
Source references and proof route are in that prompt and the bounded review.

OpenDesign's demonstrated output route is project-local atlas.html,
batch-record.md and closing-report.md, followed by user ZIP export/manual import.
Do not ask it to write canonical files or broaden permissions.
Host-prohibited post-generation checks remain explicitly unperformed, assigned
to CDC/Operator review; implementation coverage and independent gates remain.

Readiness: CDC rechecked the source blocks/markup and actual first artifact.
Preservation population, new literal identities, gradient/state context,
mutation homes, host limitations and discriminating source checks are resolved.
The new S01-06 verifies the approved addition. All six rows remain open.

## Assignment history

- 2026-10-28: Earlier cc-prompt.md was prepared but explicitly unissued; no run.
- 2026-10-28: Operator clarified OpenDesign/GPT 6 Astra, artifact comps and
  incremental review. Replaced that unissued draft and issued this initial
  `cc-prompt.md` to the Operator. No executed refinement counted.

- 2026-10-28: Operator returned Colour-Atlas-Study.zip after project-local output relocation; CDC imported its five members unchanged and reviewed screenshot/source coverage in artifacts/import-and-coverage-review.md. All criteria remain open; no new assignment issued.

- 2026-10-28: Operator requested the missing-bits update; issued `cc-prompt-iteration01.md`, predecessor `cc-prompt.md`. First attempt preserved at afcfa66. Active refinement count: one issued, zero executed. No Slice02 opening or acceptance.

- 2026-10-29: Iteration01 returned in LFE-Colour-Atlas-Iteration01.zip and imported unchanged. artifacts/iteration01-review.md records preservation of all 95 original records, six literal additions, numeric checks and the actual wide preview. One refinement executed; disposition returned for review, not accepted. No next assignment issued.
