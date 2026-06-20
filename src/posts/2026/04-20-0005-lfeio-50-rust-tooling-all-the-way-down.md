---
layout: post.liquid
title: "lfe.io 5.0: Rust Tooling, All the Way Down"
description: ""
permalink: "/blog/tutorials/2026/04/20/0005-lfeio-50-rust-tooling-all-the-way-down"
categories: ["announcements"]
tags: [lfe, cobalt, rust, infrastructure, release]
published_date: 2026-04-20 00:05:00 +0000
is_draft: false
data:
  author: duncan-mcgreggor
  written_for:
    lfe: "2.2.1"
    erlang: "28"
  last_validated: null
  cover_image: "/images/newsroom/LFE_Newsroom_00303_.png"
  cover_alt: "Vigdís — LFE news, a busy rotating space-station newsroom"
  math: false
---

If you've ever tried to contribute to `lfe.io` -- and bless you if you
have (for we have sinned) -- you know the toolchain was a bit of mess. Zola for the SSG (that one wasn't _too_ bad). TailwindCSS, which meant npm. Preline, which
meant npm. Yarn, which meant npm-but-also-yarn. Docker, because the CI
needed a particular Zola pinned next to a particular Node pinned. A
`Makefile` that called `docker run` to call `npx` to call... well, you
get the idea.

This is the LFE community. We don't _do_ npm. We don't _do_ Node sitting
on `node_modules/` like a hen on a clutch of malicious eggs. We use
Erlang. We use LFE. We use, when the occasion calls for it, Rust. (In fact, we've been publishing books using Rust's `mdbook` for almost a decade, now.) So
when I sat down a few weeks ago to fix the site's perennial "the
Tailwind output is stale" problem for the third or fourth time, I
decided that what I actually wanted was to never have to type `npm` again
to ship a tweak to the front page.

Today I'm landing **lfe.io 5.0**, which does exactly that.

## What changed

The site still renders the same. Same content, same layout, same dark
mode, same code-excerpt tabs on the masthead. If you visit `lfe.io`
right now you won't see a single new pixel. Under the hood, though:

- **Zola → [Cobalt](https://cobalt-org.github.io/).** Same Rust-ecosystem
  static site generator family, different design choices. Liquid
  templates instead of Tera. YAML data files instead of TOML
  buried-in-front-matter. Out of the box it's a little less featureful
  than Zola, but it gave us a cleaner surface to build on top of.
- **A new build tool: `lfesite`.** A small Rust CLI living at
  `tools/lfesite/` in the repo. It handles data pre-rendering, SCSS
  compilation (via the [`grass`](https://crates.io/crates/grass) crate),
  Tailwind, and Cobalt orchestration. Six subcommands, ~50 tests.
- **The npm toolchain is gone.** Tailwind is the standalone CLI binary
  now -- `lfesite` auto-downloads and caches it on first run. Preline's
  Tailwind plugin variants are inlined as Tailwind v4 `@variant`
  declarations in `tailwind/site.css`, so we don't need the JS plugin
  either. No `package.json`. No `node_modules/`. No Docker.
- **The home page content lives in `_data/home/*.yml` now.** It used to
  live in a 579-line TOML front-matter block on `content/_index.md`,
  which... yeah. Editing the features cards meant editing TOML tables
  inside a markdown file's front-matter and praying. Now each widget
  has its own YAML file: `features.yml`, `books.yml`, `videos.yml`,
  `excerpts.yml`, and so on. Markdown fields use a `_md` → `_html`
  convention; `lfesite prerender` handles the rest.

## The contributor experience

This is the part I'm most excited about. To build the site now, you
need exactly one thing: Rust. That's it. From a fresh clone:

```shell
make build
```

That'll install `lfesite`, install `cobalt`, download the Tailwind
binary, compile your SCSS, run Tailwind, build the site, and generate
the sitemap. No npm install. No Docker pull. No "but it works on my
machine."

`make serve` gives you a dev server with file watching. `make help`
gives you a colorful menu of every other target. `make check-tools`
tells you what's missing.

## Numbers

Because it's fun:

| | 4.2.0 | 5.0.0 |
|---|---|---|
| SSG | Zola | Cobalt |
| Templates | 22 Tera | 17 Liquid |
| Home page content | 579 lines of TOML | 9 YAML files |
| Required tools | Rust, Node, npm, Docker | Rust |
| Automated tests | 0 | 50 |
| Root directory items | ~45 | 10 |

The diff against the old Zola-built output came down to 74 identical
files plus a handful of cosmetic differences (meta tag formatting and
minified-CSS whitespace). Not bad for a ground-up rebuild.

## Bug fixes that snuck in along the way

The Download dropdown menu has been broken for about two years.
Surprise! It works again. The dark/light theme toggle wasn't actually
doing anything (the `hs-dark-mode-active` variant wasn't registered).
Fixed. The tab highlighting on the code-excerpt widget didn't visibly
indicate which tab was active. Fixed. A stray `</div>` in the closing
section was prematurely closing a parent container. Gone. The
`&nbsp;&nbsp;&nbsp;` hack in `books.md` and `community.md` that nobody
remembers writing? Deleted.

## What's next

The site is on Cobalt. The blog is still on its old stack. Cobalt is
_made_ for blogs -- collections, RSS, archives, tags, pagination, the
works. The next big project is bringing `blog.lfe.io` over too.

Come find us on [Discord](https://discord.gg/Uf3PszVHtF) or
[Mastodon](https://fosstodon.org/web/@lfe) if you want to help. The new
contributor story is finally one I can say with a straight face: clone
the repo, install Rust, type `make build`. That's the whole onboarding.

No npm. No tears.
