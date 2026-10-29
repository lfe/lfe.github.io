---
project: project01-colour-atlas
status: active
depends-on: []
blocks: []
related:
  - https://github.com/lfe/lfe.github.io
  - ai-engineering/project11-unified-design-practice
---

# LFE colour atlas and incremental design trials

Revision 1.8 — 2026-10-29.

## Intent and brief

Duncan wants a strong practical design workflow using the visual-design-system
guides, OpenDesign and GPT, reducing his dependence on Anthropic. Begin with
small tasks and grow them only after using/reviewing real results. His assessment
of Claude Design is a personal reference for desired design quality, not an
independently measured product ranking or proof of comparative performance.

First task: a faithful HTML colour/value atlas of today's LFE website.
The design should make the palette and lightness relationships useful to a
designer. OpenDesign owns its composition and visual judgment within that brief;
do not pre-solve its layout here. Preserve current paints and distinguish
existing aliases, inferred roles and unknowns. Do not improve colours yet.

This project is the LFE colour-atlas/concept/adoption sequence, not a mandate
to build a general competitor, run broad vendor benchmarks or maintain a new
evaluation platform. First-run evidence informs the next prompt and possible
design-skill changes, not a general claim that the guides caused the result.

## Homes and authority

Site root/input: `/Users/oubiwann/lab/lfe/lfe.github.io` (`./` in OpenDesign).
Planning: `./.worktrees/planning/`, orphan `planning` branch.
Trial artifacts: current slice `artifacts/`, including actual HTML comps.
No site source output, route or Cobalt integration in the atlas trials.
Source integration is a later, separately issued assignment after design approval.
The unused clean `codex/lfe-colour-atlas` worktree is held for that possibility.

Workflow: Two Contributors with Operator relay. Sofie plans, writes prompts,
reviews artifacts/source fidelity and records acceptance here. OpenDesign is
the executing CC through its Codex CLI; the Operator reports it is configured
for GPT 6 Astra. Do not silently substitute the earlier Luna proposal or change
OpenDesign's model/settings. Actual runtime identity/settings are recorded only
if available; configured identity is not proof of backend identity.
Duncan launches each run, supplies visual judgment and decides the next pass.
No Autonomous Mode, internal implementation agent or background monitor has
been authorized. The prior Sol/Luna Autonomous selector was not approved.

CC has read access to site inputs. The intended canonical write home is the
issued slice artifacts/proposed closing report; the first run could only write
inside OpenDesign's own project. Operator-authorized export/manual import is
therefore the demonstrated return route; do not assume broader write access. CDC owns plans, ledgers, prompt issues
and Git integration. Sequence access; review after a completed run. Do not
edit an artifact concurrently with its author. Independent self-acceptance is
not allowed. Project acceptance remains the Operator's decision.
Local scoped planning commits are allowed; no push, merge, deployment, source
migration, skill change, model/profile change or automatic next-run submission.

## Current evidence and scope

Site baseline: 036e231b09c48966da26d2227bccb4fa3b295df9.
Current layouts load `src/css/site.css` from `tailwind/site.css`, plus
`src/css/common.css` and `src/css/highlight.css`. They already include OKLCH
gold/Tailwind values, component classes and Pagefind aliases.
Legacy green/cream Sass and earlier isolated LFE design-skill specimens are
not the active theme baseline. The original checkout's unrelated dirty
Makefile/Rust files and untracked verification brief remain untouched.

Installed visual-design-system 2.0.0: all 21 installed files matched
`design-2.0` / main 53625abb368a86ce6a36f190a99fcc7d3b6b51df during setup.
Installed collaboration-framework: 3.0.1. First task-relevant guide input is
fixed for the first pass; a future change must be recorded, not mixed in silently.
Normal source reading and checks serve this delivery. No separate source
acquisition, artwork/pixel extraction, visitor study or broad research is
commissioned. Lightness, luminance, contrast and HSV V remain distinct.

## Roadmap

| Arc | Usable result and home | Dependency | Status |
|---|---|---|---|
| Arc01 as-is atlas | Self-contained palette/value comp, then roles/states, in slice artifacts | Current active CSS and installed guides | Closed; both slices accepted, assembled atlas reviewed |
| Arc02 OpenDesign concepts | Conservative/exploratory alternatives in their owning slice artifacts | Atlas review and Operator brief | Roadmap only; not yet issued |
| Arc03 chosen integration | Approved design translated into Cobalt-friendly source/templates with a checked first consumer and rollback | Explicit design/integration approval | Roadmap only; not yet issued |

Keep later arc detail late. Baseline conversion of site CSS to OKHSL/OKHSV or
semantic tokens is not a prerequisite. Readouts need a verified algorithm;
production migration needs separate approval.

## Learning loop

For each slice: agree a small brief and observable review criteria; issue a
preserved OpenDesign prompt; Operator runs it; inspect actual comp and a concise
run record; Duncan provides visual feedback; record one concrete keep/change/
next-test decision. A second pass gets cc-prompt-iteration01.md, preserving the
first output via a scoped Git commit before revision. No extra archive tree.

First pass is a developmental trial, not a controlled A/B test. Review source
fidelity, information hierarchy/optical balance, practical usefulness,
interaction/legibility, and operator effort. Use concrete observations, not a
universal beauty score. A valid trial may reveal a poor design.
Stop after one initial pass for review; do not ask OpenDesign to iterate until
it can declare itself successful. Record changed prompt, guide, model or app
conditions in later passes. Larger tasks follow only as Operator-selected work.
Do not force a new project for every prompt correction.

Feedback is split into design critique, prompt problem, tool limitation or
possible guide gap. A guide gap needs inspection and a separately scoped
ai-engineering change before editing the released skill. No automatic attribution
of a bad output to the model or guide. Future vendor comparisons require
matched briefs/inputs and actual comparable artifacts; none is commissioned now.

## Visibility

Direct filesystem/Git review of outputs is available. Live Codex transcript
access is not established: OpenDesign owns its harness/session exposure.
Ask its first run to report an available session identifier and saved transcript
path, or state unavailable. Do not fabricate a shared session or install a
monitor. Bounded read-only inspection can be chosen once actual evidence exists.

## Version history

- 1.0 — 2026-10-28: Prepared faithful-atlas-first site-source plan and draft
  prompt; no source implementation or dispatch occurred.
- 1.1 — 2026-10-28: Operator clarified the real experiment: OpenDesign/GPT 6
  Astra executes, Sofie plans/reviews, incremental trials, comps in slice
  artifacts and later explicit Cobalt integration. Replaced the unissued draft,
  removed Autonomous/Luna hold and production build/output requirements, and
  added a small qualitative learning loop and honest visibility boundary.

- 1.2 — 2026-10-28: Operator supplied first ZIP and two screenshots. Imported unchanged results, reproduced 52 token/43 syntax memberships and recorded gaps/host check limits without acceptance or source changes. Future prompts should support project-local export/manual import.

- 1.3 — 2026-10-28: Operator approved a follow-up atlas prompt for missing component literals and actual dark-homepage colour relationships. Issued Slice01 Iteration01 with source preservation and project-local ZIP return; no implementation, acceptance or next-arc launch here.

- 1.4 — 2026-10-29: Imported Operator-returned Iteration01 ZIP unchanged. Initial checks preserve all 95 records, add six literal identities and reproduce numeric data; actual wide preview inspected. Positive Operator visual feedback recorded at its stated scope; no acceptance or new execution.

- 1.5 — 2026-10-29: Narrow/keyboard review reproduced a Reset failure while
  layout, filters, sort, pagination and empty state passed their checked scope.
  Issued a two-substitution Slice01 repair. Operator's closure/next-slice
  instruction is conditional on passing; no closure or successor opening yet.


- 1.6 — 2026-10-29: Arc01 Slice01 accepted after CDC's four-case Reset
  regression, exact preservation checks and Duncan's direct-file observation.
  Opened the planned remaining mode/state/search extension as Slice02; no
  redesign, source integration or automatic OpenDesign submission authorized.


- 1.7 — 2026-10-29: Arc01 Slice02 candidate returned and independently checked.
  Technical scope passes; current direct-file and visual/usefulness gates await
  Duncan. No source change, next-arc opening or publication authorized here.


- 1.8 — 2026-10-29: Arc01 closed after both slice acceptances, fresh assembled
  walkthrough and Duncan's explicit close instruction. The accepted as-is
  atlas is the next arc's baseline. Manual ZIP transport, preserved data/script
  and early interaction checks remain the practical workflow. Source cascade/
  vendor limitations become inputs to the concept brief, not automatic fixes
  or new research work. Agree Arc02's small design brief before issuing it.
