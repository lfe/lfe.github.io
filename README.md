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
make build    # Build the site (installs tools on first run)
make serve    # Start dev server with live reload
```

The first `make build` will automatically install `lfesite` (the build tool), `cobalt` (the static site generator), and download the Tailwind CSS standalone binary. No npm, no node, no Docker required.

Run `make help` to see all available targets, or `make check-tools` to verify your setup.

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
sass/                 # FontAwesome SCSS source
tailwind/             # Tailwind CSS source (site.css)
tools/lfesite/        # Build tool (Rust)
```

### Editing Content

- **Page content**: Edit the `.md` files in `src/` (e.g., `src/about.md`, `src/learn.md`)
- **Home page widgets**: Edit YAML files in `src/_data/home/` (excerpts, features, books, videos, etc.)
- **Site config** (download versions): Edit `src/_data/site.yml`
- **Templates**: Edit Liquid files in `src/_layouts/` and `src/_includes/`
- **CSS**: Edit `tailwind/site.css` (Tailwind v4 CSS-first config with `@variant` declarations)

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

This starts a dev server on port 5093 with live reload. Edit content or templates and the site rebuilds automatically.

## Publishing

GitHub Actions automatically rebuilds and deploys the site for all successful merges to `main`.
