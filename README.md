# lfe.io

*Repository for the main LFE (Lisp Flavoured Erlang) Language site*

Visit <a href="http://lfe.io/">lfe.io</a>!

## Contributing

Feel free to [open a ticket](https://github.com/lfe/lfe.github.io/issues/new) or fork the site and submit a [pull request](https://github.com/lfe/lfe.github.io/pulls).

Note that the entire site content is driven with Markdown files -- *including* the metadata sections of those files!

The front page's content is in TOML variables between the `+++` metadata markers in `./content/_index.md`.

If you're intersted in updating the CSS, everything you need to tweak should be in `./sass/lfe/_variables.scss`.

More about the static site generator software:

* [zola docs](https://www.getzola.org/documentation/getting-started/installation/)

The templating language used by zola and which drives the layout/design of the LFE site:

* [tera](https://tera.netlify.app/docs#templates)

## Dev Server

If you would like to run the site locally, you may do so with this command:

```bash
zola serve
```

This will start the site on a local dev server running on port 1111.

If you want to run on a different port, simply set the `zola` port option to
your liking:

```bash
zola serve -p 5099
```

## Publishing

To re-deploy the site with changes made to the above-mentioned files, simply do the following:

```shell
make publish
```

You may want to check the spelling first, though:

```shell
make spell-check
```

## Publishing to Staging

The staging site is hosted in [another LFE repo](), and thus requires a custom remote URL to be added to your local git clone:

```shell
git remote add staging git@github.com:lfe/site-staging.git
```

With that in place, you can push your branch to that repo with:

```shell
git push staging <your branch name>
```

If you are pushing to a branch that is not the current default in staging, you'll want to change the default to your branch here:
* <https://github.com/lfe/site-staging/settings>

The Github Actions for deploying the site is set to only trigger on the `main` branch, so you'll want to manually kick off a deploy, when you're ready (the "Run workflow" drop-down at the top):
* <https://github.com/lfe/site-staging/actions/workflows/deploy-site.yml>
