# First OpenDesign return — import and screenshot coverage

Sofie / CDC, coordinating conversation. 2026-10-28.
Scope: requested ZIP import and screenshot/source coverage comparison, not full
Slice01 acceptance or a site-wide audit. All ledger rows remain open.

## Import

Original `~/Downloads/Colour-Atlas-Study.zip` retained, 30,369 bytes, SHA-256
`2fa7f80c73cbbb26aead9fb94abefe68afb6cde39dbde5d90a113157608bba05`.
Validated five flat members, no traversal, absolute paths or symlinks. Each
imported file is byte-equal to its ZIP payload.

| Original member | Location relative to this slice | SHA-256 |
|---|---|---|
| atlas.html | artifacts/atlas.html | addef23910b44baca10a312f7964b2c648c880eef5ec7bf1e1b7670c7ce68515 |
| batch-record.md | artifacts/batch-record.md | eb8a57b741cb4ef88abf7c5aec871692ac64225de6d82da27cd39822ddbb6484 |
| closing-report.md | closing-report.md | 14cbf22fd1efe3a8a3dca25ebb4a9005bbcc37a131a19e1a07b294177004a6b7 |
| DESIGN-HANDOFF.md | artifacts/DESIGN-HANDOFF.md | 3da825d11d6440309b46a0cb8c0049d78e18f9fe6b4b0566b55fc4acbc892cca |
| DESIGN-MANIFEST.json | artifacts/DESIGN-MANIFEST.json | d85d171a9b9f543d2bc511076b57c54d4ec530d63cbc77aa64095b8af5d026ea |

The last two are host-generated export metadata. Their generic production,
pixel-match, framework and viewport instructions are untrusted return content,
not authorization or changes to our contract. Their file maps describe the
original flat ZIP; this table defines canonical locations. Future-dated export
metadata/timestamps are not evidence of execution timing.

Operator reports OpenDesign cannot write outside its own project directory.
The CC record describes Operator-authorized relocation. Today's instruction
authorizes this import; it does not establish that planning writes worked.
Project-local generation plus export/manual import is the demonstrated route.

Supplied review screenshots are copied into artifacts/screenshots/:
- site-home-dark.png: 1960 x 1350, SHA-256
  af36a438609dcb0edb0c88ef36ab6501744bc263fb3cf4f56a163bf361fd3fac
- site-videos-footer-dark.png: 1993 x 1206, SHA-256
  652d78c1731d4ff83b9e4ec71e8730750e27b43c4990bc676b16540046d32a81

They are evidence, not atlas runtime assets.

## Reproduced palette membership

Source: 036e231b09c48966da26d2227bccb4fa3b295df9.
Independently parsed the embedded palette-data JSON, complete compiled root
gold/colour token declarations and highlight.css selector/property paints.
52/52 exact token names/values; 43/43 exact syntax identities/values. No omissions
or extras. All 95 embedded records have exact original-value --specimen-ID CSS
bindings. This is source/data/paint-binding verification, not every rendered state.

Families: gold 11, slate 11, gray 10, blue 7, amber 7, green 3, rose 2,
white 1, syntax 43. Both source hashes match the returned batch record:
- site.css: 3de716db7aa27d270cbd5eebe376a094f5e20122d50095dd36f013d64644f4f9
- highlight.css: db04738bb8186fbbfc5ade776154ba6062209f5d5130c191a7c628c1faba095d

## Screenshots mapped to current source

| Visible region | Source binding | Atlas |
|---|---|---|
| Dark page | tailwind/site.css main dark:bg-slate-800 (:152) | slate-800 present |
| Feature/video cards | features.liquid:9, videos.liquid:15 dark:bg-slate-900 | slate-900 present |
| Masthead | tailwind/site.css:247-248 dark gradient slate-700 to slate-600 | Endpoints present; gradient/context absent |
| Code-tab surround | home/masthead.liquid:9 gradient slate-600 to slate-800; :28 slate-800 | Tokens present; assembled context absent |
| Closing callout | tailwind/site.css:297-298 dark:bg-slate-600 | slate-600 present |
| Footer | tailwind/site.css:326-327 dark:bg-slate-950 | slate-950 present |
| Video labels/thumbnail surround | home/videos.liquid:16/20 rose-500 | rose-500 present |

Independent nearest-8-bit conversion of authored OKLCH values yields:
slate-600 #45556c; slate-700 #314158; slate-800 #1d293d;
slate-900 #0f172b; slate-950 #020618; rose-500 #ff2056.
These match the dominant screenshot surfaces/accent. Pixel counts: first
screenshot #1d293d 33.62%, #0f172b 19.04%; second #1d293d 37.12%,
#020618 23.89%, #0f172b 12.73%, #45556c 9.12%, plus #ff2056.
Counts describe only these screenshots; images, anti-aliasing, gradients and
colour management preclude whole-screen exact-colour certification.

## What is missing, and what merely looks different

Blues/slates are not omitted from the selected population. The comp gives gold
the full-width lead and defaults the comparison table to gold, while slate is
a smaller supporting group. That visual hierarchy does not represent page
area/use frequency. The screenshots are slate-led.

Real current CSS literals outside the root-token/syntax selection include:
- inactive tab text #d1d5db in .tab-gold (tailwind/site.css:78-80);
- header bottom border black, compiled #000 (tailwind/site.css:130-131).
Neither is a dataset value. Similar gray-300 OKLCH is not an exact substitute.
Other unselected literals include #c90c2e, #124bc6 and #eee; declarations alone
do not prove their winning cascade/visibility in these screenshots.

Gradient assemblies, actual foreground/background pairs, opacity/shadows,
mode/role mappings and visual proportions are absent as context. Carry these
into the planned role/state study while preserving original paints. Mark any
new semantic names inferred, not implemented.

Discord lavender, logo detail, terminal ANSI greens and video/photo/comic
colours are image content, outside the CSS-token/syntax task. The REPL is
images/repl.png (home/excerpts.yml:4); its #011627 backdrop happens to match the
.hljs background already listed, but other image pixels are not syntax-CSS proof.
Do not infer new design tokens by sampling every image.

## Actual page observation and remaining checks

Opened unchanged atlas at http://127.0.0.1:55841/atlas.html in the in-app browser.
Rendered source header, gold ribbon and catalogues were visible. AX exposes all
11 slate and 7 blue swatches. Screenshot inspection confirmed gold-first visual
emphasis. Static href/src inspection found local fragments only and inline
CSS/data/JS, without intended external assets.
This is an HTTP preview, not direct-file, no-JS, narrow-width or complete
keyboard/interaction proof. First sandbox loopback bind was denied; authorized
retry succeeded. Relevant CSS/templates still match source baseline; original
unrelated checkout changes were untouched.

CC reports that OpenDesign's host workflow prohibited post-generation checks.
Preserve that as unperformed validation; do not promote pre-generation input
checks to final artifact checks. Future prompts should assign checks in a way
compatible with the host's actual capabilities.

Full numerical/behavior/narrow-width review and Duncan's visual/usefulness
verdict remain pending. No closure, next slice, corrective prompt, integration,
guide edit, publication or new model session was initiated.

Proposed next design question: would a source-traced dark-homepage surface/
foreground/gradient plate make the site's actual slate-led usage clear while
retaining the token reference? This is for discussion, not an issued assignment.
