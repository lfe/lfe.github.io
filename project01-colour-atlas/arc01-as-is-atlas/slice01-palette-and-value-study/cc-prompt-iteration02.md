# OpenDesign / Codex — Repair the atlas Reset control

Project01 / Arc01 / Slice01, Iteration02. Issued 2026-10-29.
Predecessor: cc-prompt-iteration01.md. Execute this repair and return to
Duncan and Sofie / CDC. Keep the existing GPT 6 Astra OpenDesign configuration.

## Goal and boundaries

Fix one reproduced interaction defect, F01 in cdc-verification.md, affecting
S01-03. This is a mechanical repair to the existing atlas, with no redesign,
colour change, new feature, new slice or site integration.

Website root (read only): /Users/oubiwann/lab/lfe/lfe.github.io/
Planning root (read only): /Users/oubiwann/lab/lfe/lfe.github.io/.worktrees/planning/
Slice root:
project01-colour-atlas/arc01-as-is-atlas/slice01-palette-and-value-study/

Use absolute paths for reads: OpenDesign may run in its own project directory.
Write only project-local atlas.html, batch-record.md and closing-report.md,
then supply the normal ZIP export. Sofie imports them into the canonical slice.
Do not request wider access, edit repository/planning/skill files, commit,
build the site, install tools, push, publish, run agents or change settings.

## Required reading and intake, before editing

Required-full, in order: planning-root AGENTS.md; this prompt; the current
slice-plan.md and ledger.md; cdc-verification.md. Read complete contents,
recover truncated output, then record a short contract readback in the
project-local batch-record.md. Other plans and prior prompts are reference-only
for this narrow repair; this prompt repeats the binding preservation boundary.

Required-section: installed visual-design-system/guides/interaction-and-content.md,
"Turn tasks into actions and transitions" and "Assign state, semantics and focus".
Installed guide root:
/Users/oubiwann/Library/Application Support/ChatGPT-Profiles/personal/codex-home/skills/
Keep the existing design guidance input. These sections support restoring
the actual reset transition and focus, without adding a second state owner.

Required source: canonical artifacts/atlas.html at planning 56eb5e1,
SHA-256 d89a87a580aebd674ea11afb2e6a23d6adc3be15c4204dbd7097c94d19df09a9.
Read its comparison-controls markup and complete executable script (:148–238).
Inspect the two ID references and the reset/default/page/render/focus flow.
The embedded palette-data is required-data: preserve its complete bytes,
101 identities/values, including 100 opaque and one translucent record.
No source CSS or palette investigation is needed. If the canonical atlas hash
differs, report the mismatch before edits; do not start from an older export.

## Reproduced defect and prescribed repair

The form has a button id="reset" (line 143) and its click handler retrieves it
at line 223, then calls form.reset() at line 224. That control name masks the
native method. Actual Enter/click activation throws:
TypeError: form.reset is not a function.
The query, family, order, page and focus consequently do not reset.

Binding implementation: make exactly these two substitutions in atlas.html:
1. id="reset" -> id="atlas-reset" on the existing Reset button.
2. getElementById('reset') -> getElementById('atlas-reset') in its listener.

The author confirmed one occurrence of each and no #reset CSS selector.
Keep button type="button", its visible label, styling, event type and handler
body unchanged. Preserve form.reset(); page = 0; render(); query.focus();
in that order. Do not bypass the exception with a catch/no-op, hide the
control, add another reset pathway or alter the default values.

All other atlas bytes should remain unchanged. Reversing just these two
substitutions must reproduce the pinned input hash. This preserves the
101 records, eight context regions, original gradients and weak pairs,
catalogues, alpha caveat, lightness calculations and static fallback.
No new test infrastructure, dependency or generic checker deliverable.

## Verification and host limitations

Use available permitted prewrite checks to confirm exact replacement scope.
Obey OpenDesign's actual host rules, including any prohibition on
post-generation testing. Report unrun checks honestly; do not bypass host
restrictions or claim the displayed comp was inspected. CDC performs the
browser checks below after import. These are acceptance oracles, not permission
to break a host rule.

Reset oracle, at the returned revision:
- Set Component literals, Dark-to-light, query=#eee: two distinct rows.
  Activate Reset by Enter: empty query, Gold, Source order, page 1,
  11 gold records, focus on the query, no reset exception.
- Set All opaque colours, empty query, go to page 2 of 5. Activate Reset
  with Space: the same defaults, page 1 of 1 and 11 gold records.
- From an unmatched query/empty state, click Reset: defaults/results return.
- Repeat Reset in the default state; it remains usable without error.
The old artifact fails the first sequence. Checking only initial Gold rows
would miss the defect. Recheck keyboard focus and phone wrapping if the
returned markup differs beyond the prescribed changes.
Retain earlier data/layout/pagination checks only if their inputs are unchanged;
do not call retained results fresh. Direct-file/no-JS and final row closure
remain CDC review gates.

## Return and stop

Export the three project-local files. Keep prior batch/closing observations as
history and append a concise Iteration02 change/check/limits section, including
this prompt identity, input/output hashes when available and preservation
results. Distinguish proposed satisfaction of S01-03 from CDC acceptance.
Account for all six rows without changing their canonical statuses.
One repair attempt, then stop for review. No Slice02 opening or execution.
