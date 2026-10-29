# Slice01 — CDC verification

Reviewer: Sofie / CDC. 2026-10-29. Two-Contributor Workflow.
Current verdict: **Iteration02 accepted; Slice01 closed**, 2026-10-29.
See [Iteration02 acceptance](#iteration02-acceptance).

## Historical Iteration01 review

Historical verdict: **return for Iteration02; do not close or advance**.

## Candidate and retained evidence

Reviewed imported Iteration01 at planning
56eb5e11788318fafaadf00ecd4d629f10ce015f.
Atlas SHA-256:
d89a87a580aebd674ea11afb2e6a23d6adc3be15c4204dbd7097c94d19df09a9.
Source remains 036e231b09c48966da26d2227bccb4fa3b295df9.
The atlas was not edited during review. Retain the complete-population numeric,
source-preservation and wide-preview evidence in
[the focused review](artifacts/iteration01-review.md) at those unchanged bytes.
These checks do not constitute full slice acceptance.

## Fresh browser observations

Actual in-app browser preview: http://127.0.0.1:55841/atlas.html.
At 375 x 812 and 320 x 800 CSS pixels, document width equalled viewport width;
the visible-element bounding-box check found no horizontal overflow. Actual
viewport screenshots showed readable stacked controls, wrapped content and
visible focus. The first context disclosure opened/closed with Enter and had
a 3px solid focus outline. In-use/Lightness anchor navigation worked.
At 375px:
- Tab reached Family then Order; reverse Tab returned to the search field.
  Native keyboard selection changed Family to Component literals (six records)
  and Order to Dark-to-light, with black first at L = 0.
- Typing #eee returned two separate video/book foreground identities.
- Selecting all opaque records showed 100 records, 24 on page 1 of 5.
  Next activated by Enter showed 25–48/page 2; Previous activated by Space
  restored 1–24/page 1.
- An unmatched query produced zero rows, a visible empty state, a hidden
  comparison table, and disabled Previous/Next. Clearing the query restored
  the filtered population.
- Reset failed with Enter and pointer activation; it did not restore fields,
  results or focus. This is a product defect, not a selector lookup failure.

Browser selector timeouts for a native select and role lookup for a summary
were resolved using observed IDs/native controls. A rescaled native-adapter
screenshot was replaced by the actual viewport screenshot; no layout defect
was inferred from that adapter output. The read-only DOM evaluator does not
expose all native form members, so it was not used as proof of method identity.
The viewport override was reset; preview left at #in-use with default fields
restored manually. This manual restoration is not a successful Reset test.

## F01 — Reset cannot reset the comparison

Affected criterion: S01-03. At atlas.html:143 the form contains a button
id="reset"; at :223–224 its click handler calls form.reset().
That form-control name masks the native reset method. The actual browser
console reports: `TypeError: form.reset is not a function`, at :224:10.

Reproduction: Family=Component literals, Order=Dark-to-light, query=#eee;
two records are displayed. Focus Reset and press Enter, or click it. Fields
and count remain unchanged and the error is reported.

Repair: rename that button ID to atlas-reset and update its sole
getElementById lookup. There is one of each original occurrence and no #reset
CSS selector. Preserve the existing native form reset, page-zero/render/focus
sequence. No colour, data, layout or new interaction redesign is needed.
[Iteration02](cc-prompt-iteration02.md) carries the exact repair and regression
oracle. No authoring changes were made to the returned atlas here.

## Row accounting and remaining review

| Row | Disposition at this review |
|---|---|
| S01-01 | Open; retain reproduced 95-record preservation, six additions and source/scope evidence. |
| S01-02 | Open; retain full-population numeric/alpha proof and fresh duplicate/sort observations. |
| S01-03 | Open; narrow layout and listed keyboard flows pass, Reset fails F01. |
| S01-04 | Open; retain inline-resource/source-integrity evidence; direct-file and no-JS runtime observations remain unrun. |
| S01-05 | Open; CC records and focused critique retained; final return/acceptance remains pending. |
| S01-06 | Open; retain eight-region/source/gradient review; no palette repair authorized. |

All six canonical statuses remain open. No accessibility-conformance,
assistive-technology, other-engine/device, full-site or exhaustive rendered-pair
claim follows from these checks. Direct-file/no-JS runtime checks and the
remaining applicable acceptance walk stay with CDC after the repair.

The Operator authorized closure and next-slice opening **if everything passes**.
F01 means that condition is unmet. No closure or Slice02 opening occurs.
The known defect gets one mechanical feedback pass, not a new planning scale.


## Iteration02 acceptance

Assignment: cc-prompt-iteration02.md at planning bb9cfa0.
Returned bytes preserved in planning dc65d53; no source commit was made.
ZIP: LFE-Colour-Atlas-Iteration02.zip, 37,641 bytes, SHA-256
9fa105673a1cbc46bb5144948c60bc0650e02b369783520c1724351800e854b2.
Exactly three flat regular members were safely imported byte-for-byte:
- atlas.html -> artifacts/atlas.html, 148,669 bytes, SHA-256
  1c11f970a1c0b383719fddab164b8b092eb9ba51257882150ed0c42fc5199959.
- batch-record.md -> artifacts/batch-record.md, 16,231 bytes, SHA-256
  9ada3c5522b94985cdd46e2b4e15c2a334e44608c618a86995a71cf9c4a66cdf.
- closing-report.md -> slice root, 5,841 bytes before the attributed CDC
  close supplement, SHA-256
  2437e6d8c6be56e7e22da1815e4084cd10c9adad921e843243d92681a60ea11b.

### Fresh reproduction and retained evidence

Exact byte comparison independently confirms only the two prescribed ID
substitutions. Reversing them reproduces the prior atlas completely. The prior
95-record reconciliation, all 101 numeric/alpha bindings, 100 neutrals, eight
regions, gradients, CSS and static fallback are unaffected; retain their
independent evidence in artifacts/iteration01-review.md and the historical
review above, rather than claiming those calculations/layouts were rerun.

After reloading the actual HTTP preview, the new atlas-reset ID was present.
All four real regression sequences passed:
1. Component literals / Dark-to-light / #eee produced two rows; Enter on Reset
   restored empty query, Gold, Source order, page 1 of 1, 11 rows and query focus.
2. All opaque / page 2 of 5; Space on Reset restored those same defaults.
3. An unmatched query showed the empty state; pointer Reset restored defaults.
4. Repeated Reset at defaults remained usable. Fresh console error query: none.

Fresh DOM styles for the eight context regions and CTA resolve to the expected
source-bound slate/gray/gold values and gradient endpoints. Static HTML parsing
finds only fragment links, no external resource dependencies or fetch/storage;
the palette/context text and original catalogues exist outside executable
script, and controls/pagination start hidden with [hidden] display:none.
This is structural proof of the no-JS content path, not a JS-disabled browser
session. Eighteen named relevant source files match both their reported hashes
and the pinned 036e231 source blobs. An initial helper accidentally included
guide SKILL.md as a site-relative file; restricting it to src/ and tailwind/
corrected that helper scope, after which all 18 passed.

The attempted new phone override did not change the actual viewport (still
1822 x 1319); it is not new phone evidence. The override was reset. Retain the
previous reproduced 320/375px layout and keyboard observations because the
only changes are an ID and its listener lookup. Current Reset focus resolves
to the query with a visible 3px outline.

The in-app browser blocks file:// URLs. No alternative browser or workaround
was attempted. Duncan independently opened the exact canonical file and
reported: "the file opens ... filters and reset work"; the structured answer
also confirms direct-file opening and those controls. This Operator
observation closes the portability checkpoint, separately attributed from
CDC's HTTP/browser and source checks.

### Final rows and bubble-up

| ID | Final status | Evidence and acceptance |
|---|---|---|
| S01-01 | done | Reproduced complete source/data checks retained at byte-identical paints/data; exact two-ID diff; actual comp inspected. |
| S01-02 | done | Reproduced full numeric/alpha/neutral proof retained; original distinctions and duplicate identities unchanged. |
| S01-03 | done | Prior narrow/wide and keyboard observations retained, fresh four-case Reset regression passed, critique recorded; Duncan's positive design feedback and conditional close instruction apply to unchanged composition. |
| S01-04 | done | Fresh structural dependency/static-content and 18-file preservation checks; HTTP comp runs; independent Operator direct-file observation. |
| S01-05 | done | Appended CC intake/change/limits retained, exact return preserved, independent review and concrete next checks recorded. |
| S01-06 | done | Eight contexts and six literals preserved; retained source/cascade proof plus fresh rendered paint checks; scope/asset/semantic boundaries explicit. |

Six opening rows, six final rows; no deferral, no-op or silent drop.
F01 is closed by the prescribed repair and actual regression.
The artifact inventory and attributed close supplement in closing-report.md
are reconciled with the eight files under artifacts/. Issued prompts remain
unchanged; the current assignment is Iteration02, accepted after two executed
refinement passes. No source implementation, site build, publication, WCAG/AT,
full-site coverage, JS-disabled runtime or vendor-parity claim is made.

Bubble-up: Slice01 delivers Arc01's accepted palette/value/context comp,
artifact-only and usable locally. Source fields and observed states remain
distinct; full remaining mode/state/search study is still Slice02's work.
Manual ZIP import and CDC/Operator checks accommodate the host's write and
post-generation-test restrictions. Keep that route; no new archival/test system.
The arc plan is updated before the next slice is issued. A01-01 can close;
arc composition and final Operator review remain open.
