# LFE site palette

This file documents the source-owned Tailwind palette in `site.css`. The machine-readable coordinates, origins, gamut reductions, semantic map and pair checks are in [`palette.json`](palette.json). The accepted 30° orange/plum study input is preserved in the planning worktree at commit `e599ba13124683bbaab965cc113a1817b9b0fe69`; this adoption uses its precomputed grid30 values without browser-time shade generation.

## Primitive ramps

All values are explicit six-digit sRGB literals. Numbered stops are a naming convention, not a promise of contrast or perceptual uniformity. Slate keeps the accepted Slice04 per-stop +15° hue treatment and original stop paints; the two added neutral anchors preserve accepted muted and on-action paints.

| Family | Stop | Primitive | Hex | Quantized OKLCH (L, C, h°) |
|---|---:|---|---|---|
| slate | 50 | `--lfe-slate-50` | `#f9fafc` | `0.984906783, 0.002860766, 264.542142471` |
| slate | 100 | `--lfe-slate-100` | `#f2f5f9` | `0.969011588, 0.006236061, 255.474901701` |
| slate | 200 | `--lfe-slate-200` | `#e4e7f0` | `0.928290812, 0.012574011, 271.279765428` |
| slate | 300 | `--lfe-slate-300` | `#ced4e1` | `0.869217263, 0.019078330, 265.969083404` |
| slate | 400 | `--lfe-slate-400` | `#9aa1b8` | `0.710993192, 0.034425378, 272.095465772` |
| slate | 500 | `--lfe-slate-500` | `#6a728b` | `0.554526493, 0.040084928, 271.135803415` |
| slate | 600 | `--lfe-slate-600` | `#4d5369` | `0.445613039, 0.036967364, 272.915664852` |
| slate | 700 | `--lfe-slate-700` | `#393f55` | `0.371587857, 0.038748832, 272.578856347` |
| slate | 800 | `--lfe-slate-800` | `#23273b` | `0.278461037, 0.037437412, 275.022353629` |
| slate | 900 | `--lfe-slate-900` | `#15152a` | `0.207737641, 0.041177900, 282.213340782` |
| slate | 950 | `--lfe-slate-950` | `#050517` | `0.129612244, 0.040975399, 279.029243275` |
| orange | 50 | `--lfe-orange-50` | `#fff9f3` | `0.985092279, 0.010229840, 67.707194853` |
| orange | 100 | `--lfe-orange-100` | `#feefe1` | `0.960229538, 0.024710343, 65.585849606` |
| orange | 200 | `--lfe-orange-200` | `#f7dcc3` | `0.910260856, 0.044773390, 65.032791386` |
| orange | 300 | `--lfe-orange-300` | `#ebc29b` | `0.840359562, 0.069835838, 65.215079418` |
| orange | 400 | `--lfe-orange-400` | `#d6a270` | `0.749664194, 0.090259162, 64.611391250` |
| orange | 500 | `--lfe-orange-500` | `#bb8145` | `0.649916160, 0.104871312, 65.005765941` |
| orange | 600 | `--lfe-orange-600` | `#9c672e` | `0.559445253, 0.099403300, 65.044382783` |
| orange | 700 | `--lfe-orange-700` | `#7b5021` | `0.470012134, 0.084593084, 65.302591266` |
| orange | 800 | `--lfe-orange-800` | `#5c3a15` | `0.381111952, 0.069979330, 64.907825144` |
| orange | 900 | `--lfe-orange-900` | `#3e2509` | `0.290426550, 0.055224064, 65.115222492` |
| orange | 950 | `--lfe-orange-950` | `#231506` | `0.210412063, 0.034666883, 66.839573281` |
| gold | 50 | `--lfe-gold-50` | `#fcfaf1` | `0.984178335, 0.012041430, 96.418916896` |
| gold | 100 | `--lfe-gold-100` | `#f7f2e0` | `0.960215660, 0.024321525, 94.068723597` |
| gold | 200 | `--lfe-gold-200` | `#eae2c0` | `0.910390242, 0.045642087, 96.220202905` |
| gold | 300 | `--lfe-gold-300` | `#d9cb97` | `0.841023474, 0.069946368, 94.827765265` |
| gold | 400 | `--lfe-gold-400` | `#c0ae6a` | `0.750992567, 0.090364834, 95.024711269` |
| gold | 500 | `--lfe-gold-500` | `#a38e3c` | `0.649302731, 0.104592902, 94.934648253` |
| gold | 600 | `--lfe-gold-600` | `#877323` | `0.559302756, 0.099933131, 94.648788971` |
| gold | 700 | `--lfe-gold-700` | `#6a5a18` | `0.470519288, 0.085163312, 94.982186671` |
| gold | 800 | `--lfe-gold-800` | `#4e420d` | `0.380924093, 0.070562589, 95.721778929` |
| gold | 900 | `--lfe-gold-900` | `#342b04` | `0.290822411, 0.055717468, 95.572531009` |
| gold | 950 | `--lfe-gold-950` | `#1d1804` | `0.209494026, 0.035572925, 96.093524081` |
| green | 50 | `--lfe-green-50` | `#f4fdf7` | `0.985667335, 0.012309944, 157.009247092` |
| green | 100 | `--lfe-green-100` | `#e6f7eb` | `0.960260484, 0.023919134, 155.152444525` |
| green | 200 | `--lfe-green-200` | `#cbebd4` | `0.910613522, 0.045720459, 154.155028819` |
| green | 300 | `--lfe-green-300` | `#a7d9b7` | `0.840982261, 0.070257091, 154.912729070` |
| green | 400 | `--lfe-green-400` | `#7fbf95` | `0.749756462, 0.089518501, 154.901428332` |
| green | 500 | `--lfe-green-500` | `#55a273` | `0.649757584, 0.104796598, 155.278318489` |
| green | 600 | `--lfe-green-600` | `#3d865a` | `0.560999890, 0.100739220, 154.850112626` |
| green | 700 | `--lfe-green-700` | `#2d6945` | `0.470501411, 0.086036760, 154.810287202` |
| green | 800 | `--lfe-green-800` | `#1f4d32` | `0.379730121, 0.068911157, 155.328088096` |
| green | 900 | `--lfe-green-900` | `#11331f` | `0.289427833, 0.054231805, 155.025786338` |
| green | 950 | `--lfe-green-950` | `#0a1d11` | `0.210247025, 0.034754856, 154.373983909` |
| blue | 50 | `--lfe-blue-50` | `#f6fbff` | `0.985459396, 0.007534265, 241.660712604` |
| blue | 100 | `--lfe-blue-100` | `#e7f4ff` | `0.960697540, 0.020259424, 243.414415320` |
| blue | 200 | `--lfe-blue-200` | `#c9e5fe` | `0.909368060, 0.045383682, 245.266943636` |
| blue | 300 | `--lfe-blue-300` | `#a5d0f6` | `0.840045218, 0.070117526, 245.442518910` |
| blue | 400 | `--lfe-blue-400` | `#7db4e4` | `0.749934596, 0.090426466, 245.728231917` |
| blue | 500 | `--lfe-blue-500` | `#5495cb` | `0.649285000, 0.105197087, 245.471210361` |
| blue | 600 | `--lfe-blue-600` | `#3c7aab` | `0.560372503, 0.099682996, 244.822688100` |
| blue | 700 | `--lfe-blue-700` | `#2d5f87` | `0.469731108, 0.084574802, 245.170096545` |
| blue | 800 | `--lfe-blue-800` | `#1e4665` | `0.380840774, 0.069648831, 244.569438991` |
| blue | 900 | `--lfe-blue-900` | `#102e45` | `0.290898007, 0.055170541, 244.602348656` |
| blue | 950 | `--lfe-blue-950` | `#091a27` | `0.210321850, 0.034533043, 243.555413976` |
| red | 50 | `--lfe-red-50` | `#fff8f7` | `0.984247610, 0.007627780, 27.227312586` |
| red | 100 | `--lfe-red-100` | `#ffede9` | `0.959298164, 0.020362149, 32.530422133` |
| red | 200 | `--lfe-red-200` | `#fed7cd` | `0.909407131, 0.045952603, 34.726912395` |
| red | 300 | `--lfe-red-300` | `#f4bbac` | `0.839762017, 0.069581395, 35.219970756` |
| red | 400 | `--lfe-red-400` | `#e19a87` | `0.751279320, 0.090164432, 35.619748720` |
| red | 500 | `--lfe-red-500` | `#c67763` | `0.649958484, 0.104196525, 35.030180601` |
| red | 600 | `--lfe-red-600` | `#a75d4b` | `0.559773018, 0.101141092, 34.626835412` |
| red | 700 | `--lfe-red-700` | `#834839` | `0.469239885, 0.084516984, 35.191568258` |
| red | 800 | `--lfe-red-800` | `#623428` | `0.380692559, 0.069553603, 35.543026285` |
| red | 900 | `--lfe-red-900` | `#422018` | `0.288456224, 0.054735400, 34.274247917` |
| red | 950 | `--lfe-red-950` | `#26120d` | `0.209638081, 0.034988944, 34.899263832` |
| plum | 50 | `--lfe-plum-50` | `#fff8fd` | `0.985923395, 0.009895033, 335.469813676` |
| plum | 100 | `--lfe-plum-100` | `#fdecf8` | `0.959959309, 0.024111992, 336.088516231` |
| plum | 200 | `--lfe-plum-200` | `#f5d7ed` | `0.910391979, 0.043903801, 335.348354934` |
| plum | 300 | `--lfe-plum-300` | `#e8badd` | `0.840102242, 0.069812942, 334.762950054` |
| plum | 400 | `--lfe-plum-400` | `#d298c5` | `0.749263276, 0.091095220, 334.702110309` |
| plum | 500 | `--lfe-plum-500` | `#b776a9` | `0.650574660, 0.105582935, 334.899626239` |
| plum | 600 | `--lfe-plum-600` | `#985d8c` | `0.559891884, 0.099817277, 334.601595399` |
| plum | 700 | `--lfe-plum-700` | `#78486e` | `0.470954406, 0.084532592, 334.830831435` |
| plum | 800 | `--lfe-plum-800` | `#593351` | `0.379378493, 0.070438482, 335.009910990` |
| plum | 900 | `--lfe-plum-900` | `#3c2036` | `0.289723144, 0.055323279, 335.246897445` |
| plum | 950 | `--lfe-plum-950` | `#22121f` | `0.210170347, 0.034994899, 333.775204880` |

## Exact selected anchors

These 36 named anchors are the approved grid30 action, link, tagline and five-family status values. Keep their selected hex values exact; do not replace them with the nearest standard ramp stop. Two extra dark neutral anchors are listed separately.

| Anchor | Primitive | Hex | Source role |
|---|---|---|---|
| `orange-action-light` | `--lfe-orange-action-light` | `#875315` | `grid30.modes.light.accents.action` |
| `orange-hover-light` | `--lfe-orange-hover-light` | `#734300` | `grid30.modes.light.accents.hover` |
| `plum-tagline-light` | `--lfe-plum-tagline-light` | `#704968` | `grid30.modes.light.accents.plum` |
| `blue-ink-light` | `--lfe-blue-ink-light` | `#2c5374` | `grid30.modes.light.statuses.info.roles.ink` |
| `blue-tint-light` | `--lfe-blue-tint-light` | `#eaf5ff` | `grid30.modes.light.statuses.info.roles.tint` |
| `blue-border-light` | `--lfe-blue-border-light` | `#6284a2` | `grid30.modes.light.statuses.info.roles.border` |
| `green-ink-light` | `--lfe-green-ink-light` | `#2d5b3f` | `grid30.modes.light.statuses.success.roles.ink` |
| `green-tint-light` | `--lfe-green-tint-light` | `#ebf7ee` | `grid30.modes.light.statuses.success.roles.tint` |
| `green-border-light` | `--lfe-green-border-light` | `#638b71` | `grid30.modes.light.statuses.success.roles.border` |
| `gold-ink-light` | `--lfe-gold-ink-light` | `#5c4f1d` | `grid30.modes.light.statuses.warning.roles.ink` |
| `gold-tint-light` | `--lfe-gold-tint-light` | `#f7f4e6` | `grid30.modes.light.statuses.warning.roles.tint` |
| `gold-border-light` | `--lfe-gold-border-light` | `#8b8057` | `grid30.modes.light.statuses.warning.roles.border` |
| `red-ink-light` | `--lfe-red-ink-light` | `#714135` | `grid30.modes.light.statuses.error.roles.ink` |
| `red-tint-light` | `--lfe-red-tint-light` | `#fff0ec` | `grid30.modes.light.statuses.error.roles.tint` |
| `red-border-light` | `--lfe-red-border-light` | `#a17468` | `grid30.modes.light.statuses.error.roles.border` |
| `plum-ink-light` | `--lfe-plum-ink-light` | `#68415f` | `grid30.modes.light.statuses.plum.roles.ink` |
| `plum-tint-light` | `--lfe-plum-tint-light` | `#fceff8` | `grid30.modes.light.statuses.plum.roles.tint` |
| `plum-border-light` | `--lfe-plum-border-light` | `#97738f` | `grid30.modes.light.statuses.plum.roles.border` |
| `orange-action-dark` | `--lfe-orange-action-dark` | `#f5ba81` | `grid30.modes.dark.accents.action` |
| `orange-hover-dark` | `--lfe-orange-hover-dark` | `#fed1a7` | `grid30.modes.dark.accents.hover` |
| `plum-tagline-dark` | `--lfe-plum-tagline-dark` | `#dbadd0` | `grid30.modes.dark.accents.plum` |
| `blue-ink-dark` | `--lfe-blue-ink-dark` | `#a8d0f3` | `grid30.modes.dark.statuses.info.roles.ink` |
| `blue-tint-dark` | `--lfe-blue-tint-dark` | `#1c242b` | `grid30.modes.dark.statuses.info.roles.tint` |
| `blue-border-dark` | `--lfe-blue-border-dark` | `#6a8aa6` | `grid30.modes.dark.statuses.info.roles.border` |
| `green-ink-dark` | `--lfe-green-ink-dark` | `#aad8b8` | `grid30.modes.dark.statuses.success.roles.ink` |
| `green-tint-dark` | `--lfe-green-tint-dark` | `#1c251f` | `grid30.modes.dark.statuses.success.roles.tint` |
| `green-border-dark` | `--lfe-green-border-dark` | `#6c9078` | `grid30.modes.dark.statuses.success.roles.border` |
| `gold-ink-dark` | `--lfe-gold-ink-dark` | `#d8cb9b` | `grid30.modes.dark.statuses.warning.roles.ink` |
| `gold-tint-dark` | `--lfe-gold-tint-dark` | `#252319` | `grid30.modes.dark.statuses.warning.roles.tint` |
| `gold-border-dark` | `--lfe-gold-border-dark` | `#908660` | `grid30.modes.dark.statuses.warning.roles.border` |
| `red-ink-dark` | `--lfe-red-ink-dark` | `#f2bcae` | `grid30.modes.dark.statuses.error.roles.ink` |
| `red-tint-dark` | `--lfe-red-tint-dark` | `#2b201d` | `grid30.modes.dark.statuses.error.roles.tint` |
| `red-border-dark` | `--lfe-red-border-dark` | `#a47b70` | `grid30.modes.dark.statuses.error.roles.border` |
| `plum-ink-dark` | `--lfe-plum-ink-dark` | `#e6bbdc` | `grid30.modes.dark.statuses.plum.roles.ink` |
| `plum-tint-dark` | `--lfe-plum-tint-dark` | `#282026` | `grid30.modes.dark.statuses.plum.roles.tint` |
| `plum-border-dark` | `--lfe-plum-border-dark` | `#9c7a93` | `grid30.modes.dark.statuses.plum.roles.border` |
| `slate-muted-dark` | `--lfe-slate-muted-dark` | `#bcc3d6` | `accepted Slice04 nonstandard muted ink` |
| `slate-on-action-dark` | `--lfe-slate-on-action-dark` | `#1d1e33` | `accepted Slice04 nonstandard on-action ink` |

## Semantic aliases by mode

Each semantic alias is defined in both `:root` and `.dark`. Keep the definitions mode-local; do not let an alias inherit a reference whose value was already resolved in another scope.

| Semantic role | Light definition | Dark definition |
|---|---|---|
| `--lfe-action` | `var(--lfe-orange-action-light)` | `var(--lfe-orange-action-dark)` |
| `--lfe-action-hover` | `var(--lfe-orange-hover-light)` | `var(--lfe-orange-hover-dark)` |
| `--lfe-action-quiet` | `var(--lfe-slate-600)` | `var(--lfe-slate-700)` |
| `--lfe-action-quiet-hover` | `var(--lfe-slate-700)` | `var(--lfe-slate-600)` |
| `--lfe-action-quiet-text` | `#ffffff` | `var(--lfe-slate-200)` |
| `--lfe-action-secondary` | `var(--lfe-slate-700)` | `var(--lfe-slate-600)` |
| `--lfe-action-secondary-hover` | `var(--lfe-slate-600)` | `var(--lfe-slate-700)` |
| `--lfe-action-secondary-text` | `#ffffff` | `var(--lfe-slate-200)` |
| `--lfe-boundary` | `var(--lfe-slate-500)` | `var(--lfe-slate-400)` |
| `--lfe-brand-ink` | `var(--lfe-slate-800)` | `var(--lfe-slate-800)` |
| `--lfe-brand-plate` | `var(--lfe-slate-50)` | `var(--lfe-slate-50)` |
| `--lfe-callout-title` | `var(--lfe-orange-action-light)` | `var(--lfe-orange-action-dark)` |
| `--lfe-canvas` | `var(--lfe-slate-50)` | `var(--lfe-slate-900)` |
| `--lfe-error-boundary` | `var(--lfe-red-border-light)` | `var(--lfe-red-border-dark)` |
| `--lfe-error-surface` | `var(--lfe-red-tint-light)` | `var(--lfe-red-tint-dark)` |
| `--lfe-error-text` | `var(--lfe-red-ink-light)` | `var(--lfe-red-ink-dark)` |
| `--lfe-focus` | `var(--lfe-orange-action-light)` | `var(--lfe-orange-action-dark)` |
| `--lfe-footer` | `var(--lfe-slate-200)` | `var(--lfe-slate-200)` |
| `--lfe-footer-focus` | `var(--lfe-orange-action-dark)` | `var(--lfe-orange-action-dark)` |
| `--lfe-footer-hover` | `var(--lfe-orange-action-dark)` | `var(--lfe-orange-action-dark)` |
| `--lfe-footer-surface` | `var(--lfe-slate-800)` | `var(--lfe-slate-900)` |
| `--lfe-footer-text` | `var(--lfe-slate-200)` | `var(--lfe-slate-200)` |
| `--lfe-header` | `var(--lfe-slate-200)` | `var(--lfe-slate-900)` |
| `--lfe-hover` | `var(--lfe-slate-300)` | `var(--lfe-slate-600)` |
| `--lfe-info-boundary` | `var(--lfe-blue-border-light)` | `var(--lfe-blue-border-dark)` |
| `--lfe-info-surface` | `var(--lfe-blue-tint-light)` | `var(--lfe-blue-tint-dark)` |
| `--lfe-info-text` | `var(--lfe-blue-ink-light)` | `var(--lfe-blue-ink-dark)` |
| `--lfe-interactive-accent` | `var(--lfe-orange-action-light)` | `var(--lfe-orange-action-dark)` |
| `--lfe-link` | `var(--lfe-orange-action-light)` | `var(--lfe-orange-action-dark)` |
| `--lfe-link-hover` | `var(--lfe-orange-hover-light)` | `var(--lfe-orange-hover-dark)` |
| `--lfe-link-visited` | `var(--lfe-plum-tagline-light)` | `var(--lfe-plum-tagline-dark)` |
| `--lfe-masthead` | `var(--lfe-slate-50)` | `var(--lfe-slate-800)` |
| `--lfe-nav` | `var(--lfe-slate-700)` | `var(--lfe-slate-300)` |
| `--lfe-nav-accent` | `var(--lfe-orange-action-light)` | `var(--lfe-orange-action-dark)` |
| `--lfe-notice-boundary` | `var(--lfe-plum-border-light)` | `var(--lfe-plum-border-dark)` |
| `--lfe-notice-surface` | `var(--lfe-plum-tint-light)` | `var(--lfe-plum-tint-dark)` |
| `--lfe-notice-text` | `var(--lfe-plum-ink-light)` | `var(--lfe-plum-ink-dark)` |
| `--lfe-on-action` | `#ffffff` | `var(--lfe-slate-on-action-dark)` |
| `--lfe-panel` | `#ffffff` | `var(--lfe-slate-800)` |
| `--lfe-raised` | `var(--lfe-slate-200)` | `var(--lfe-slate-700)` |
| `--lfe-success-boundary` | `var(--lfe-green-border-light)` | `var(--lfe-green-border-dark)` |
| `--lfe-success-surface` | `var(--lfe-green-tint-light)` | `var(--lfe-green-tint-dark)` |
| `--lfe-success-text` | `var(--lfe-green-ink-light)` | `var(--lfe-green-ink-dark)` |
| `--lfe-tagline` | `var(--lfe-plum-tagline-light)` | `var(--lfe-plum-tagline-dark)` |
| `--lfe-text-muted` | `var(--lfe-slate-600)` | `var(--lfe-slate-muted-dark)` |
| `--lfe-text-primary` | `var(--lfe-slate-800)` | `var(--lfe-slate-200)` |
| `--lfe-text-secondary` | `var(--lfe-slate-700)` | `var(--lfe-slate-300)` |
| `--lfe-warning-boundary` | `var(--lfe-gold-border-light)` | `var(--lfe-gold-border-dark)` |
| `--lfe-warning-surface` | `var(--lfe-gold-tint-light)` | `var(--lfe-gold-tint-dark)` |
| `--lfe-warning-text` | `var(--lfe-gold-ink-light)` | `var(--lfe-gold-ink-dark)` |

## Exceptions and usage

- The existing `--gold-50` through `--gold-950` names remain compatibility aliases to `--lfe-gold-*`; new code should use the namespaced primitives or semantic roles.
- `--lfe-footer-hover` and `--lfe-footer-focus` intentionally use the selected **dark-mode** orange action in both site modes, preserving the footer’s inverse-surface contract.
- `--lfe-callout-title` is mode-local orange action emphasis for the existing home closing callout. Its canonical Markdown input uses this semantic alias; the build regenerates derived `title_html`.
- Pagefind’s four `--pf-error-*` values use the real error family. Its ordinary link, raised surface, selected mark and focus continue to use their semantic roles.
- `--lfe-brand-ink` is fixed to slate 800 in both modes for the category-count plate.
- Actual code cells, syntax highlighting, REPL/terminal imagery and `--lfe-on-art` retain their component-owned values. The widget frame, tab strip/tab states and description use ordinary semantic UI roles and adopt this palette; the legacy `--lfe-code-tab*` definitions stay preserved.

Typical consumers:

```css
.site-link { color: var(--lfe-link); }
.site-link:hover { color: var(--lfe-link-hover); }
.success-message {
  color: var(--lfe-success-text);
  background: var(--lfe-success-surface);
  border-color: var(--lfe-success-boundary);
}
```

## Reproduction and provenance

- Source baseline: `08f2e5e3e2fabdba81323dd07b1df50abafc8763` on `codex/lfe-design-deploy`.
- Selected comparison artifact: `project01-colour-atlas/arc03-design-deploy/slice06-accent-comparison/artifacts/palette-spec.json` at `e599ba13124683bbaab965cc113a1817b9b0fe69`.
- Standard ramps: 7 families × 11 stops = 77 literals; exact role anchors: 38; total unique primitives: 115.
- Conversion: fixed L/h;50 binary-search C reductions when needed, inward C margin0.000001; nearest8bit sRGB; no clipping; full coordinates and all 56 affected context-pair checks are in `palette.json`.
- Slate provenance: Tailwind3.4.17 source hex ramp -> per-stop OKLCH hue+15, L/C unchanged before quantisation; existing accepted stop hex preserved ([upstream source](https://raw.githubusercontent.com/tailwindlabs/tailwindcss/v3.4.17/src/public/colors.js)). OKLCH conversion reference: [https://www.w3.org/TR/css-color-4/#color-conversion-code](https://www.w3.org/TR/css-color-4/#color-conversion-code).
