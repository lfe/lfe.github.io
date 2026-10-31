---
name: lfe-website
description: >
  Use when working in the lfe.github.io repo — building or serving the LFE
  website/blog, adding or editing blog posts, fixing post URLs/permalinks or
  front-page placements, or changing the `lfesite` Rust build tool. Covers the
  Cobalt + lfesite + Tailwind/Pagefind pipeline and the permalink model.
---

# LFE Website & Blog (lfe.github.io)

## Overview

The LFE project website and blog (domain `lfe.io`, see `CNAME`). Static site
built by **Cobalt** (a Rust SSG) and driven by a **custom Rust tool, `lfesite`**
(in `tools/lfesite/`), which wraps the whole pipeline. Templates are **Liquid**;
styling is **Tailwind**; search is **Pagefind**.

**Canonical insight:** a post's identity is its `permalink`. The date/time/slug
is duplicated in three places that must agree — **filename**, **`published_date`**,
and **`permalink`** — and everything downstream (URLs, front-page placement)
keys off `permalink`. They're written consistently at creation; if you *edit*
one later, you must edit all three. (See Gotchas.)

## Where things live

| Path | What |
|------|------|
| `_cobalt.yml` | Cobalt config: `source: src`, `destination: ./site`, default permalink template |
| `src/` | All site source (pages, posts, layouts, data, assets) |
| `src/posts/<YEAR>/MM-DD-HHMM-slug.md` | Blog posts, **by-year layout** (migrated from a flat layout) |
| `src/_layouts/*.liquid`, `src/_includes/` | Liquid templates |
| `src/_data/` | Data files: `blog.yml`, `authors.yml`, `site.yml`, `home/`, … |
| `src/_data/blog_resolved.yml` | **Generated** by the build (gitignored) — do not hand-edit |
| `src/images/{newsroom,tutorials,default}/` | Cover-image pools (auto-assigned) |
| `tools/lfesite/` | The Rust build tool (cargo workspace member) |
| `site/` | **Generated** build output (gitignored; **not** auto-cleaned) |
| `Makefile` | Entry point for every task |

## Commands

Everything runs through `make` (which builds and calls this checkout’s `lfesite` binary by default):

| Command | Does |
|---------|------|
| `make build` | Full build → `site/` |
| `make serve` | Dev server + file watch (port 5093 by default) |
| `make clean` | Remove `site/`, `_site/`, and branch-local `target/*/lfesite` outputs; does not uninstall a shared binary |
| `make install` | `cargo install` the `lfesite` binary **and** `cobalt-bin` |
| `make validate` | Sanity-check data files, front-matter, layouts |
| `make test` / `make check` | `lfesite` tests / build+lint+test+validate |

`lfesite` subcommands (also runnable directly): `build`, `serve`, `prerender`,
`validate`, `new-post`, `publish-post`, `draft-post`, `migrate`, `blog-migrate`.

`make build` runs six stages in `tools/lfesite/src/cmd/build.rs`: **prerender** (render `*_md` fields), **blog-resolve** (editorial slots and cover images), **Tailwind** (from `tailwind/site.css` to generated `src/css/site.css`), **Cobalt**, **sitemap.xml**, and **Pagefind**.

By default Make builds `tools/lfesite` into this checkout’s `target/debug/lfesite` (or `$CARGO_TARGET_DIR/debug/lfesite`) before build, serve, prerender, validate or migrate. Set `LFESITE=/path/to/lfesite` to use an existing executable; an override is not built automatically. `make install` remains the explicit global install. `cobalt` must be installed with `make install`; `tailwindcss` and `pagefind` auto-download standalone binaries to `~/.cache/lfesite/` if not on `PATH`. `src/fontawesome*.css` and `src/webfonts/` are tracked vendor assets loaded directly by `src/_includes/css.liquid`; no Sass compiler or Sass source is used.

## Working on blog posts

**Create:** `lfesite new-post "Title" --category tutorials --tags a,b [--draft] [--slug foo] [--date "YYYY-MM-DD HH:MM"]`.
It writes `src/posts/<year>/MM-DD-HHMM-slug.md` and generates the filename,
`permalink` (`/blog/<category>/<year>/<month>/<day>/<hhmm>-<slug>`), and
`published_date` from one timestamp — so they start consistent. Author is your
git `user.name`, slugified, and must exist in `src/_data/authors.yml`.

**No spaces in tags or categories** — use hyphens (`lfe-friday`, not `lfe
friday`). A space becomes an ugly, router-breaking tag URL like
`/blog/tags/lfe%20friday/`. Enforced by `make check-tags-cats` (wired into
`make lint` and `make check`).

**Cover images:** posts with `cover_image: null` get one auto-assigned by the
blog-resolve step, deterministically, from a category-routed pool
(`announcements`/`news`→newsroom, `tutorials`→tutorials except `lfe-friday`,
else default). It writes the choice back into front-matter (idempotent).

## Front-page editorial config (`src/_data/blog.yml`)

Hand-curated front-page placement. Slots — `centrepiece`, `lede_1`, `lede_2`,
`spotlight` — each take a **post `permalink`, copied verbatim** from the post's
front-matter. `river_count` sets how many chronological items follow. Empty/
missing slot → falls back to the next chronological post.

The blog-resolve step matches slots to posts by **exact permalink string** and
writes `src/_data/blog_resolved.yml` for the templates. A mismatch logs
`warning: <slot> permalink '…' not found, using fallback` and silently
substitutes the wrong post — check build output for that warning.

## Gotchas (all seen in practice)

| Symptom | Cause / Fix |
|---------|-------------|
| Renamed/re-dated a post but it keeps its **old URL** | The explicit `permalink` in front-matter wasn't updated. Cobalt uses it verbatim (it overrides the `_cobalt.yml` default template). Update filename **+** `published_date` **+** `permalink` together. |
| Old URL still 200s after a permalink change | `site/` is **not** auto-cleaned — stale output lingers. `make clean` (or delete the stale `site/.../old-path/` dir) and rebuild. |
| Front-page slot shows the **wrong post** | `blog.yml` slot value doesn't exactly match the post's `permalink`. Match the exact string (mind trailing slashes). Watch for the "using fallback" warning. |
| Edited `tools/lfesite/` but the build behaves unchanged | Make builds and invokes `target/debug/lfesite` from this checkout by default (or `$CARGO_TARGET_DIR/debug/lfesite`). Check `LFESITE` for an explicit executable override. |
| A "the builder is broken" report | Usually it isn't — it faithfully uses front-matter/data. The migration-gap tell is **something reconstructing or hardcoding a date/slug instead of reading `permalink`**. Trace to the stale source value. |

## Verify before claiming done

After changes: `make check` for the repository-native tool checks, then `make build` (watch for permalink fallback warnings) and inspect `site/`. `site/` and
`src/_data/blog_resolved.yml` are gitignored generated artifacts — commit
sources, not output.
