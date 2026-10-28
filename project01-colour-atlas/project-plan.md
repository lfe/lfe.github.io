---
project: project01-colour-atlas
status: active
depends-on: []
blocks: []
related:
  - https://github.com/lfe/lfe.github.io
  - ai-engineering/project11-unified-design-practice
---

# LFE colour atlas and design exploration

Revision 1.0 — 2026-10-28.

## Brief and definition of done

Give Duncan a faithful HTML colour/value atlas of today's LFE site, then use
that baseline in a small OpenDesign experiment and a chosen reversible
semantic-token migration. The first useful result is the actual local palette
and lightness page. Intended audience: Duncan and his OpenDesign session;
this is supplied intent, not visitor research.

Preserve current paint, gradients, opacity, state differences and exceptions
through the baseline work. Distinguish existing aliases from inferred roles.
Show ambiguities rather than inventing implemented tokens or correcting colours.
Deliver source under `src/design/colour-atlas/`, without public navigation edits.

## Current evidence and boundaries

Site baseline: 036e231b09c48966da26d2227bccb4fa3b295df9.
Current layouts load `src/css/site.css` from `tailwind/site.css`, plus
`src/css/common.css` and `src/css/highlight.css`. They already include OKLCH
gold/Tailwind variables, component classes and Pagefind aliases. Earlier
green/cream `sass/lfe/_variables.scss` and `src/lfe.css` are legacy material;
earlier design-skill LFE specimens are separate example worktrees.
Neither is today's production palette.

The installed visual-design-system is 2.0.0: all 21 installed files matched
main and `design-2.0` at 53625abb368a86ce6a36f190a99fcc7d3b6b51df.
The active collaboration framework is 3.0.1.

Source inspection and checks needed to deliver this page are within scope.
No separate research, source acquisition, framework comparison, complete
artwork/pixel palette, visitor study or site-wide accessibility certification.
Do not claim a model's lightness is WCAG luminance, HSV value or exact visual
equality. OKHSL/OKHSV readouts need a verified algorithm and round-trip checks;
conversion of production CSS is not a prerequisite for a faithful atlas.

## Roadmap

| Arc | Usable delivery/home | Required input | Status |
|---|---|---|---|
| Arc01 as-is atlas | Palette/value page, roles/states and local tool handoff in `src/design/colour-atlas/` | Current active CSS/layouts | Open; execution awaits workflow selection |
| Arc02 OpenDesign concepts | Conservative and exploratory alternatives against the same baseline/task | Accepted atlas plus Operator tool/session decisions | Roadmap only |
| Arc03 semantic adoption | Chosen mapping with first rendered consumer and rollback | Operator-selected direction and migration contract | Roadmap only |

Later arcs are not authorized to execute yet; detail them when near.

## Workflow, ownership and authority

Planning: orphan `planning`, `.worktrees/planning`.
Source: `codex/lfe-colour-atlas`, `.worktrees/colour-atlas`, at baseline above.
Original main checkout's Makefile/Rust changes and
`CC-verification-brief-otp-tutorials.md` are protected and outside the task.

CDC: Sofie in the coordinating conversation. Default is CDC + CC. A pending
question proposes Autonomous Mode for Arc01 only: OpenAI gpt-6.1-sol/high for
CDC/review and gpt-6-luna/high for CC. This is a proposal, not approval.
Hold contributor dispatch and source implementation until the Operator chooses.
If normal relay is selected, provide the preserved CC prompt path. No CRC.
Slice acceptance requires independent CDC review; arc composition needs a fresh
independent context or Operator verdict. Operator owns project acceptance.

After issue, CC owns source mutations/index and CDC owns planning. Sequence
build/generation and review handbacks. An implementer cannot independently
accept its repairs. Record actual context identities when assigned.

Local scoped commits and loopback preview are allowed. No push, merge, public
deployment, OpenDesign upload/submission, new runtime/dependency installation
or profile/browser-preference changes. Preserve unrelated work and do not
install tools merely for validation.

## Version history

- 1.0 — 2026-10-28: Operator approved faithful-atlas-first delivery. Created
  planning/source worktrees and corrected the preliminary legacy-Sass assessment
  using actual stylesheet links. Awaiting workflow selection; no source edits.
