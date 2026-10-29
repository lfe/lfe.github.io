# Slice01 — CDC verification

Reviewer: Sofie / CDC. 2026-10-29. Two-Contributor Workflow.
Verdict: **return for Iteration02; do not close or advance**.

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
