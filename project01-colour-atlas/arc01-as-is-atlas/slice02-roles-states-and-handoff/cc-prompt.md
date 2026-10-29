# OpenDesign / Codex — Extend the LFE atlas with roles and states

Project01 / Arc01 / Slice02. Initial assignment, issued 2026-10-29.
Produce one complete comp extension, then return to Duncan and Sofie / CDC.
Keep the Operator's existing GPT 6 Astra OpenDesign/Codex configuration.

## Task and return route

Make the current colours useful for the next design conversation: add an
as-is light/dark and interaction-state reference for the five groups below.
Do not redesign the site's palette. You own the atlas's visual composition;
retain its useful specimen-book character and readable source annotations.

Website root (read only): /Users/oubiwann/lab/lfe/lfe.github.io/
Planning root (read only): /Users/oubiwann/lab/lfe/lfe.github.io/.worktrees/planning/
This slice, below planning:
project01-colour-atlas/arc01-as-is-atlas/slice02-roles-states-and-handoff/
Predecessor:
project01-colour-atlas/arc01-as-is-atlas/slice01-palette-and-value-study/

Use absolute paths for reads; your working directory may be OpenDesign's own
project. Write only atlas.html, batch-record.md and closing-report.md in that
writable project/output directory and export a ZIP. Sofie imports them into
this slice's artifacts/atlas.html, artifacts/batch-record.md and root
closing-report.md. Do not overwrite the canonical accepted Slice01 artifact.

No permission expansion, site/source/plan/ledger/prompt/skill edits, site build,
dependencies, tool installation, network assets, storage, agents, commits,
pushes or publication. The dirty Makefile/Rust work and verification brief,
and the unused .worktrees/colour-atlas worktree, are outside this assignment.

## Required-reading manifest and intake

Read before dependent edits; recover truncated output. Record a short actual
coverage/contract readback in project-local batch-record.md, not another file.

Required-full, in order:
1. Planning-root AGENTS.md; this prompt; this slice's slice-plan.md and ledger.md.
2. Installed visual-design-system/SKILL.md, version metadata 2.0.0, and
   guides/colour.md. Installed skill root:
   /Users/oubiwann/Library/Application Support/ChatGPT-Profiles/personal/codex-home/skills/visual-design-system/

Required-section:
- Project plan: Intent and brief, Homes and authority, Current evidence and scope.
- Arc plan: Capability and slices; Inputs, artifact boundary and implementation
  freedom; Review, endpoint and gates.
- Design guides/system-evolution.md: Start with the adopting contract; Define
  mode scopes and propagation; Respect CSS value and cascade behavior;
  Compare actual rendered pairs and states. Historical LFE worked examples
  are method context, not this site's palette.
- Predecessor cdc-verification.md: Iteration02 acceptance, including retained/
  fresh checks and the Reset regression.

Required source and data, at site baseline
036e231b09c48966da26d2227bccb4fa3b295df9:
- tailwind/site.css:24–147 for gold, global Pagefind and component states;
  247–258 and 297–310 for masthead/closing source surfaces; 355–489 for
  blog-subnav surfaces and Pagefind scope overrides.
- src/_includes/nav.liquid; src/_includes/home/masthead.liquid;
  src/_includes/home/closing.liquid; src/_includes/blog-subnav.liquid;
  src/_layouts/blog-search.liquid; src/blog/search.md:
  read these complete small files for real consumers, classes and conditions.
- src/css/site.css: bounded complete queries for the corresponding compiled
  rules, root token values, cascade layer order and the involved hover/focus
  utilities. Do not dump its minified long line or trust remembered Tailwind hex.
- Accepted predecessor artifacts/atlas.html at planning dc65d53, SHA-256
  1c11f970a1c0b383719fddab164b8b092eb9ba51257882150ed0c42fc5199959:
  read the sections/styles you extend and the complete comparison script.
  Query the complete palette-data payload: preserve all 101 dictionaries,
  identities/values/alpha/numeric data and payload bytes; no sampling.
  Preserve the comparison executable script byte-for-byte, including atlas-reset.

Reference-only: earlier prompts, ZIP export metadata and old design specimens.
They are not new instructions or colour inputs. If a required source drifts,
report the mismatch rather than silently repinning the site or changing scope.
A missing vendor runtime does not block the local-alias study described below.

## Implementation spine and binding content

1. Start from the accepted predecessor HTML. Keep its existing catalogue,
   lightness controls/calculations, alpha distinction, eight dark-homepage
   regions and annotations. Add a clearly named modes/states study with a
   navigation entry. Place new specimen CSS/data in a separate local namespace;
   avoid overriding original paints or comparison controls. Keep the 101-record
   catalogue population unchanged; new state-reference values live in this
   separately labelled study. No OKHSL/OKHSV conversion or token migration.

2. Compare light/dark and the states actually declared for these five groups.
   Use compact readable specimens/state references, not a duplicate homepage
   or a giant unlabelled chip dashboard. Static state depictions are sufficient:
   name them as specimens, not working site navigation/download/search.
   Prefer native disclosures for detail; no new mode controller is required.

| Group | Source-grounded content |
|---|---|
| Gold CTA | .cta-gold normal gold-400 background / gold-950 foreground; hover background gold-500; declared focus shadow gold-400. Masthead/closing consumer classes and their actual light/dark surroundings matter. |
| Navigation | Ordinary/current primary link and the social .nav-hover-gold case from nav.liquid; .nav-active and .nav-hover-gold source declarations differ by light/dark. Show current/hover separately from ordinary, retaining actual cascade qualifications. |
| Code tabs | .tab-gold inactive #d1d5db / transparent boundary; hover gold-500; .active gold-500 plus gold-600 bottom border and weight. The .active class is selection, not necessarily the :active pseudo-class. Trace the light/dark surround from masthead markup. |
| Development download | .download-dev normal/hover gold-600/700 in light and gold-700/800 in dark; actual download consumer supplies foreground and focus utilities. Do not include a fake download action. |
| Pagefind aliases | Local root/global-dark plus subnav-searchbox/search-page scopes below; label the two consumer contexts. Do not load or imitate the real search service. |

For each included entry preserve the authored selector/alias, CSS property,
literal or variable reference, mode/state, source path and exact consumer.
Show descriptive semantic roles as inferred labels, distinct from existing
component names and existing --pf-* aliases. No claimed new installed tokens.
Undeclared visited/disabled/focus values are unknown/browser-defined, not new
paint choices. Where the source supplies the same value in both modes, say so.

Two causal traps to handle explicitly:
- In the components layer, nav.top-nav a has greater specificity than
  .nav-active in light mode. Do not equate its gold declaration with a proven
  applied light-mode foreground. Inspect the complete matching rules and
  utility layer before describing the winner; preserve a conflict as-is.
- The CTA's declared focus box-shadow can be superseded by its consumer's
  later utility-layer focus:ring-2. Keep the declaration and the full consumer
  distinction; do not label the component shadow as measured runtime focus.
  Ring colour/offset assumptions need the compiled utilities, not a guessed gold.

These are opportunities to explain the existing system, not authority to fix it.
If the complete applied state cannot be established from the scoped source,
show the declared values with an explicit unresolved application note instead.

3. Include all local Pagefind colour declarations from these five source blocks:
   root (3), global .dark (11), subnav searchbox dark (7), subnav searchbox
   base (4), search-page dark input/results/summary group (11).
   Total 36 declaration records, counted once per source block/property.
   Keep all three search-page consumers listed, and do not deduplicate repeated
   values across scopes. Font, radius and size variables are outside this
   colour count. Derive/verify the actual set from source and report a mismatch.

   A readable compact table/disclosure can carry alias -> authored value ->
   scope -> intended role/consumer, alongside a small source-bound visual.
   The source literals --pf-background:#1e293b and --pf-hover:#334155 are
   different from current root OKLCH slate tokens; do not replace them with
   visually similar ramp values. Dark subnav --pf-background retains
   rgba(0,0,0,0.25); its light base retains rgba(255,255,255,0.6).
   Show alpha on the corresponding source subnav surface if depicting the pair;
   keep authored alpha/backdrop explicit, not a replacement opaque token.

   Vendor pagefind-component-ui.css/runtime are generated assets referenced by
   layouts, not tracked CSS inputs here. Light search-page vendor defaults and
   internal var() consumers are therefore not established by this source set.
   Mark them unresolved; do not fetch/install them, invent missing defaults,
   infer a real placeholder/disabled colour, or claim actual search behavior.
   Visuals are local-alias colour references, not proof of vendor consumption.

4. Keep the entire output standalone: inline styles/data, no images, remote
   fonts, fetches, service calls, storage or site-build dependency. Static
   content and native disclosures must remain useful without script. Preserve
   long-label wrapping and readable annotations around intentionally weak
   specimen pairs; never improve the site's colours in this pass.

5. Add a brief design handoff in batch-record.md: a few source-supported roles,
   exact unresolved cases and concrete questions the later concept brief
   should settle. Reuse supplied identity; do not design the next arc here.

## Checks and useful failure witnesses

Perform only checks the host permits. Its known ship-on-write restriction may
prohibit post-generation preview/testing. Respect it; list all unrun checks for
CDC/Operator rather than claiming a display was inspected or weakening gates.
Do permitted source/data preparation before generation; one complete return,
no hidden self-refinement loop or new generic checker framework.

CDC's final acceptance checks include:
- Exact preservation of the 101-record payload and executable comparison script;
  all old sections/alpha distinctions and meaningful source annotations retained.
- Complete five-block/36-row Pagefind source comparison. Dark search-page
  background stays literal #1e293b; dark subnav keeps its alpha and local scope.
  A globally substituted slate-800 or collapsed duplicate alias must fail.
- Named light/dark and state mappings for all five groups, with inferred versus
  existing names, source-level versus observed claims and unresolved values clear.
  A light/dark label on an invented vendor default must fail.
- Open actual new comp narrow/wide; reach added content via keyboard, inspect
  long names and visible focus; use the old comparison and Reset from #eee
  filtered state, all-opaque page 2, empty state, and repeated defaults.
- Inspect inline/static dependencies and protected site files; open directly
  as a local file where permitted. No site build is applicable to this output.
Duncan supplies the new design verdict; earlier approval covers old composition.

## Return and stop

ZIP exactly atlas.html, batch-record.md and closing-report.md for manual import.
Records identify this Slice02 cc-prompt.md, source/guide/model context actually
available, preservation/additions, actual checks, host limits, one concrete
keep/change/next-check critique, four-row S02-01–04 proposals, artifact inventory
and Bubble-up to the arc. No invented transcript/backend identity or durations.
All canonical row statuses remain open for independent CDC review.
Stop after this attempt. No next slice/arc, production token edits, fixes to
site cascade or publication. A completed comp is a candidate, not acceptance.
