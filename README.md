# lfe.io

*Repository for the main LFE (Lisp Flavoured Erlang) Language site*

Visit <a href="http://lfe.io/">lfe.io</a>!

## Contributing

Feel free to [open a ticket](https://github.com/lfe/lfe.github.io/issues/new) or fork the site and submit a [pull request](https://github.com/lfe/lfe.github.io/pulls).

### Prerequisites

The only prerequisite is [Rust](https://www.rust-lang.org/tools/install). Everything else is installed automatically:

```shell
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```

### Quick Start

```shell
make build    # Build the site with this checkout’s lfesite
make serve    # Start dev server with file watching
```

Each normal build first compiles this checkout’s `lfesite` into `target/debug/lfesite` (or `$CARGO_TARGET_DIR/debug/lfesite`) and invokes that exact binary. Cobalt must be installed separately with `make install`; Tailwind CSS and Pagefind download standalone binaries to `~/.cache/lfesite/` when missing. You can set `LFESITE=/path/to/lfesite` to select an existing executable; overrides are not built automatically. No npm, no node, no Docker required.

Run `make help` to see all available targets, or `make check-tools` to verify your setup.

### Build pipeline

`lfesite build` runs prerender → blog-resolve → Tailwind → Cobalt → sitemap → Pagefind. The Tailwind source is `tailwind/site.css`; generated site CSS is `src/css/site.css`. FontAwesome CSS and webfonts remain tracked vendor assets. `make clean` removes `site/`, `_site/`, and branch-local `target/*/lfesite` build outputs; it does not uninstall a shared Cargo binary.

### Project Structure

```
src/                  # Site content and assets
  ├── _data/home/     #   Home page data (YAML — excerpts, features, books, etc.)
  ├── _data/site.yml  #   Site-wide config (download URLs, versions)
  ├── _includes/      #   Liquid template partials
  ├── _layouts/       #   Liquid page layouts
  ├── *.md            #   Content pages (about, learn, use, etc.)
  ├── css/, js/       #   Stylesheets and scripts
  ├── images/         #   Image assets
  └── ...             #   Other static files (favicon, fonts, papers, etc.)
  ├── fontawesome*.css # Tracked FontAwesome compiled stylesheets
  ├── webfonts/        # Tracked FontAwesome fonts
tailwind/             # Tailwind CSS source (site.css)
tools/lfesite/        # Build tool (Rust)
```

### Editing Content

- **Page content**: Edit the `.md` files in `src/` (e.g., `src/about.md`, `src/learn.md`)
- **Home page widgets**: Edit YAML files in `src/_data/home/` (excerpts, features, books, videos, etc.)
- **Site config** (download versions): Edit `src/_data/site.yml`
- **Templates**: Edit Liquid files in `src/_layouts/` and `src/_includes/`
- **Site CSS**: Edit `tailwind/site.css` (Tailwind v4 CSS-first config); the build generates `src/css/site.css`. FontAwesome stylesheets and fonts are tracked vendor assets loaded directly by the CSS include.

Content data files use a `_md` / `_html` convention: fields ending in `_md` contain markdown source, and `lfesite prerender` generates the corresponding `_html` fields automatically during build.

### Spell Checking

Before submitting a PR we recommend running the spell checker:

```shell
make spell-check
```

(requires [aspell](http://aspell.net/))

## Dev Server

```shell
make serve
```

This starts a dev server on port 5093 with file watching. Edits to content, templates, data or `tailwind/site.css` rebuild the site; generated `src/css/site.css` and `src/_data/blog_resolved.yml` are ignored by the watcher.

## Publishing

GitHub Actions automatically rebuilds and deploys the site for all successful merges to `main`.
