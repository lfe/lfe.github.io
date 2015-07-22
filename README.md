# lfe.io

*Repository for the main LFE (Lisp Flavoured Erlang) Language site*

Visit <a href="http://lfe.io/">lfe.io</a>!

## Contributing

Feel free to <a href="https://github.com/lfe/lfe.github.io/issues/new">open a ticket</a>
or fork the site and submit a
<a href="https://github.com/lfe/lfe.github.io/pulls">pull request</a>.

## Dev Server

If you would like to run the site locally, you may do so with this command:

```bash
$ make dev
```

This will start the site on a local dev server running on port 4000. If you
want to run on a different port, simply set the ``PORT`` environment variable
to your liking:

```bash
$ PORT=5099 make dev
```

This ``make`` target assumes that ``lfetool`` has cloned the lfe repository
(and compiled it) to ``~/.lfe/lfe``.

You can also connect to your running server in another terminal using the
following ``make`` target:

```bash
$ make connect
```
