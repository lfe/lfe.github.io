# Slice02 — CDC verification

Reviewer: Sofie / CDC, 2026-10-29. Two Contributors; OpenDesign is CC.
Assignment: cc-prompt.md at planning ad9132b.
Current verdict: **accepted; Slice02 closed**, 2026-10-29.
See [final acceptance](#final-acceptance).

## Historical initial review at 3dda6a8

Initial verdict: technical review passed; Operator gates were pending.
No required correction found. S02-01/02 accepted; S02-03/04 remain open for
the current extended comp's direct-file and visual/usefulness observations.
No slice or arc closure, successor opening or publication.

## Return identity and import

Operator supplied ~/Downloads/LFE-Colour-Atlas-Slice02.zip, 42,778 bytes.
ZIP SHA-256:
b8ddf54a48be2a8ddf228b34c391620d2e05941771f477b4f2ed5fa171d5d69d.
Exactly three flat regular, non-traversal/non-symlink members were imported
unchanged into this slice:
- artifacts/atlas.html: 192,292 bytes, SHA-256
  d39acba479bf74e9c960adc1821524785ea2f65e3322fef5a2cdc3b26fd7b9d0.
- artifacts/batch-record.md: 14,382 bytes, SHA-256
  57af3e1e02921326da7819b10bd5760a0f69ea5da30e92374f6d4ab1452fd77b.
- closing-report.md: 3,989 bytes, SHA-256
  e6c69c76df779b60322a4d6ae3c1233ecd2cbf884b003d6ac2c3abf4c3caec84.
Predecessor Slice01 remains unchanged and accepted. Its atlas hash remains
1c11f970a1c0b383719fddab164b8b092eb9ba51257882150ed0c42fc5199959.
Source remains 036e231b09c48966da26d2227bccb4fa3b295df9.

## Independent preservation and source checks

- All script bodies, including the complete 101-record palette payload and
  executable comparison script, are byte-equal to the accepted predecessor.
- Removing the added CSS, new modes/states section (including its inserted
  newline separator), and navigation link reconstructs the complete predecessor
  exactly. Existing style, catalogue, source notes, eight context specimens,
  alpha distinctions, values and algorithms are preserved.
- Parsed all five new Pagefind tables and independently compared each complete
  alias/value sequence with tailwind/site.css. Exact 36/36 match in groups
  3/11/7/4/11. Repeated scopes/values and the three search-page consumers remain
  distinct. Record scope and source locators were inspected against the source.
- All eight required site input files are byte-equal to the pinned source blobs.
  The dirty Makefile/Rust work and verification brief remain outside this task.
- Read the five-group content and all added CSS. New selectors/variables are
  scoped to s02; no old palette variable or comparison script was replaced.
  The source distinguishes existing aliases/classes, inferred roles, source
  reasoning and unobserved vendor internals.
- Checked the compiled navigation/download and focus rules, layer order and
  consumer classes. Current-light navigation loses gold by specificity; current-
  dark hover resolves to gray-600 in its hover-capable case; social text
  utilities outrank the component hover declarations. CTA utility focus retains
  currentColor ring with white/light or gray-800/dark offset. Development's
  blue-500 ring is in nav.top-nav .download:focus, not a standalone utility.
- Static resource/ID inspection finds only local fragment hrefs, no duplicate
  IDs or unresolved fragments, no external asset/fetch/storage dependency.
  Catalogue, mode/state text and alias values are HTML outside executable
  script; native details do not require script. This is structural static-
  fallback proof, not a JS-disabled browser session.

Helper recovery: the first insertion-reversal comparison left an extra separator
newline; removing the entire actual insertion including that separator passed.
An optional LF/EOF assertion rejected the HTML's missing terminal newline;
the accepted predecessor also has no terminal newline, so no formatting repair
was imposed. The imported bytes are preserved; both Markdown files use LF/EOF.
These were helper assumptions, not product defects.

## Actual browser checks

Served the standalone Slice02 artifact on loopback port 55842 and reviewed it
at http://127.0.0.1:55842/atlas.html#modes-states in the in-app browser.
No site build or production route is involved.

Wide view (1822 CSS pixels): five groups appear as labelled light/dark
comparisons; source details stay in native disclosures. Actual screenshots
show CTA/focus, navigation/tabs, development and Pagefind content. The page is
long but the local group navigation makes each comparison reachable.
This is CDC's task-fit visual assessment, not Duncan's approval or user research.

Narrow checks, with actual viewport verified after reload:
- 375 x 812: width/scrollWidth 375; CTA panels stack at 343px; no visible
  element in the new study exceeds viewport bounds.
- Keyboard Enter reaches Modes & states and group anchors. Root aliases open
  with Enter; Tab reaches the next disclosure; focus outline is 3px.
- Opened all five alias tables by keyboard: all 36 rows present and no
  horizontal overflow at 375px, including long scoped consumers.
- 320 x 800: opened the 11-row dark search table; width/scrollWidth 320 with
  no out-of-bounds study elements. Normal viewport restored afterward.

Fresh four-case comparison regression at 375px:
1. Component literals / Dark-to-light / #eee -> two identities; Enter Reset
   restores empty query, Gold, Source order, page 1 of 1, 11 rows, query focus.
2. All opaque / page 2 of 5 -> Space Reset restores those defaults.
3. Unmatched query -> visible empty state; pointer Reset restores defaults.
4. Repeated default Reset remains usable.
Fresh browser error-log query returned no errors.

Rendered specimen witnesses match their declared source model:
- Navigation's 12 foregrounds match the light/dark ordinary/current/social
  sequence, including gray-600 for current-dark hover.
- CTA static focus shadows use gold-950 outer rings, white/light or gray-800/
  dark 2px offsets, with outer extent 4px.
- Pagefind search-page dark surface resolves to rgb(30,41,59), its hover alias
  to rgb(51,65,85); the exact literals remain #1e293b and #334155.
- Subnav keeps black alpha 0.25 over oklch(0.373 0.044 257.287), and white
  alpha 0.6 over oklch(0.869 0.022 252.894).
These observations verify this atlas's static references, not the site's real
state transitions, Pagefind internals, browser-native outlines or conformance.

## Row dispositions and remaining gates

| Row | CDC disposition | Evidence / next owner |
|---|---|---|
| S02-01 | done | Reproduced exact data/script/old-content preservation, five-group source mappings and actual comp. |
| S02-02 | done | Reproduced all 36 scoped paints, cascade/alpha distinctions and bounded rendered witnesses; missing vendor values stay explicit. |
| S02-03 | open | Technical layout/keyboard/static/dependency/source checks pass; Duncan's direct-file check of this current extension remains pending. |
| S02-04 | open | CC records, inventory, critique and next-brief questions supplied and reviewed; Duncan's visual/usefulness verdict remains pending. |

Asked Duncan to review the actual current five-group study and separately open
the canonical Slice02 HTML in his browser. The in-app browser's previously
established file:// restriction remains; no blocked URL or workaround retried.
Slice01's older local-file/design observations are not silently reused as
approval of this added content. No WCAG/AT, forced-colour, enlarged text,
other-engine/device, live-search or comparative-vendor acceptance is claimed.

## Critique and provisional arc handoff

Keep: the paired references and pale labels make source conflicts readable
without changing the site. The new content exposes real semantic ownership,
rather than inventing tokens merely to complete a table.
Remaining design concern: page length; use the local group links/disclosures
and have Duncan judge whether the source detail is useful at this density.
Next check: current direct-file and visual review, then Arc01 composition.

All four opening rows remain accounted for. No scope was dropped and no repair
assignment is needed on the evidence reviewed. The original catalogue and
current/hover/focus distinctions must survive any later design proposal.
Missing vendor internals stay a named limitation of this local-alias study,
not an acquisition project or a fabricated default. No next-arc work is issued.


## Final acceptance

Duncan's responses for this exact extended artifact:
- Visual/usefulness review: "Suitable for this as-is study".
- Direct-file check: "Direct file and controls work".

These independently supplied Operator observations close the two remaining
gates. The atlas remains SHA-256
d39acba479bf74e9c960adc1821524785ea2f65e3322fef5a2cdc3b26fd7b9d0;
all previously reproduced technical evidence remains applicable, not rerun.
No change to the HTML, source inputs, batch record or issued prompt.

| ID | Final status | Evidence |
|---|---|---|
| S02-01 | done | Retained exact predecessor/data/script preservation and five-group review. |
| S02-02 | done | Retained complete 36-declaration/source comparison and rendered witnesses. |
| S02-03 | done | Retained responsive, keyboard, Reset, static-content and source checks plus Duncan's current direct-file observation. |
| S02-04 | done | Reviewed records, critique and handoff plus Duncan's current visual/usefulness verdict. |

Four opening rows, four done; no deferred/no-op row or silent drop.
The preserved CC closing report receives a clearly attributed CDC supplement.
Artifact inventory reconciles to two files under artifacts/ (atlas.html and
batch-record.md) and the first-class closing report in the slice root; the ZIP
is retained external transport in Downloads. No new capture/archival artifact.

Bubble-up verified: Slice02 delivers the remaining bounded light/dark/state/
search-alias study in the same atlas. Missing vendor internals remain explicit
scope limits, not a hidden deferral. Arc01 can compose the accepted two slices
and use the source-supported design questions when briefing a later concept
trial. No redesign, integration or publication is authorized by slice closure.
