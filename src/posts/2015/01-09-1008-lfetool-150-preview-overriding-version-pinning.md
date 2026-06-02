---
layout: post.liquid
title: "lfetool 1.5.0 Preview: Overriding Version Pinning"
description: "Looking for volunteers to test new lfetool feature"
permalink: "/blog/announcements/2015/01/09/1008-lfetool-150-preview-overriding-version-pinning"
categories: ["announcements"]
tags: ["tools", "utilities", "libraries", "qa", "testing"]
published_date: 2015-01-09 10:08:00 +0000
is_draft: false
data:
  author: duncan-mcgreggor
  written_for: null
  last_validated: null
  cover_image: "/images/default/LFE_00253_.png"
  cover_alt: "Vigdís — LFE, retro-futurist digital painting"
  math: false
---
<a href="/blog/assets/images/posts/lfetool-logo-large-grey.png"><img class="right smallplus" src="/blog/assets/images/posts/lfetool-logo-large-grey.png" /></a>The next version of lfetool will provide users with the flexibility
to override dependencies with pinned versions in their project's
``rebar.config`` file. This post is aimed at folks on the LFE mail list who
may be interested in using this right away or helping to do QA. Of course,
if others find themselves compelled to use and test the pre-release code,
they are quite welcome (with open arms, even)!

lfetool 1.5.0 will do this via:

 * An ``lfe.config`` file
 * Bootstrapping LFE
 * Bootstrapping dependent libraries

Here's how you can try this new feature out ...


## Override Dependencies

lfetool 1.5.0 will let you override the ``rebar.config``-declared dependency
versions with either ``HEAD`` or a named branch.

### What you need

 * Install [rebar](https://github.com/rebar/rebar#downloading)
 * Make sure git is installed on your system
 * Get the development version (1.x) of lfetool:

   ```bash
$ curl -L -o \
    ./lfetool https://raw.github.com/lfe/lfetool/dev-v1/lfetool
$ bash ./lfetool install
   ```
 * An LFE project (if you don't have one, you can create one with the command
   ``lfetool new library my-lib``)


### Default Pinning

To ensure greater functional longevity of projects, ``lfetool`` generates
a ``rebar.config`` with specific releases. As such, if you have a recent LFE
project, your ``rebar.config`` file has a ``deps`` section that probably looks
something like this:

```erlang
{deps, [
   {lfe, ".*", {git, "git://github.com/rvirding/lfe.git", {tag, "v0.9.0"}}},
   {lutil, ".*", {git, "https://github.com/lfex/lutil.git", {tag, "0.6.0"}}},
   {ltest, ".*", {git, "git://github.com/lfex/ltest.git", {tag, "0.4.2"}}}
  ]}.
```

Unfortunately, due to
[an old bug in rebar](https://github.com/rebar/rebar/issues/170),
it can be quite painful to use a different version of say, LFE, in your project
if another dependency in your project wants to use a *different* version of
LFE.

A common request for the lfetool project has been allowing users to use the
latest version of LFE in either the ``develop`` or ``master`` branches. We
can use this new feature to do that now.

Note that in order for the following to work, you need to make sure that the
*app version* for all of your dependencies is not set to an explicit version,
but rather to ``".*"``. For instance, if you have the following in your
``rebar.config``:

```erlang
{lutil, "0.4.0", {git, "https://github.com/lfex/lutil.git", {tag, "0.4.0"}}},
```

you will need to change it to this:

```erlang
{lutil, ".*", {git, "https://github.com/lfex/lutil.git", {tag, "0.4.0"}}},
```


### Overriding

``lfetool`` provides for overriding the defaults through an LFE config file,
``lfe.config``, and a new command:

```bash
$ lfetool download deps
```

If your project is using the latest release of LFE, 0.9.0, but you want to use
the ``develop`` branch instead, create a file named ``lfe.config`` in your
project directory with the following content:

```lfe
#(project (#(deps (#("rvirding/lfe" "develop")))))
```

Then do this:

```bash
$ rm -rf deps/lfe
$ lfetool download deps
$ make compile
$ make repl-no-deps
```
```
> (lutil:get-versions)
(#(erlang "17")
 #(emulator "6.2")
 #(driver-version "3.1")
 #(lfe "0.10.0-dev")
 #(lutil "0.6.0"))
```

Voi la! The LFE version is that of the develop branch, not the 0.9.0 release :-)

You can make one more change for convenience: in your project, update
the ``resources/make/common.mk``, changing the ``get-deps`` target from this:

```make
get-deps:
    @echo "Getting dependencies ..."
    @lfetool download deps || \
    (which rebar.cmd >/dev/null 2>&1 && rebar.cmd get-deps || rebar get-deps)
```

to this:

```make
get-deps:
    @echo "Getting dependencies ..."
    @PATH=$(SCRIPT_PATH) ERL_LIBS=$(ERL_LIBS) $(LFETOOL) download deps
```

(Be sure to use tabs!)

If you run across any issue, please
[file a ticket](https://github.com/lfe/lfetool/issues/new) :-) Thanks!


## Some More Background

As mentioned before, this feature had been requested several times in the past,
both in public venues and in private conversations. It was consistently
back-burnered due to the anticipated amount of work required to make it happen.

However, as more requests were made over time (and with Robert Virding
casting his vote as well), it became clear that this could not be put off
any longer. This came up most recently in the
[lfetool 0.9.0 update ticket](https://github.com/lfe/lfetool/issues/111) (see
[this comment](https://github.com/lfe/lfetool/issues/111#issuecomment-68649788)).
A few points where made, and then the conversation was moved to
[this ticket](https://github.com/lfe/lfetool/issues/135). In some ways there
was less work than expected, and in others, more. You know, the usual ;-)

That last ticket is fairly long, so we'd like to point to you
[this comment](https://github.com/lfe/lfetool/issues/135#issuecomment-68797570),
which details the testing plan undertaken on multiple Docker containers. This
section provides a set-by-step approach for most of what you need to ensure
things are working on Linux.

Also, if you do find issues, that is a great place to leave some comments.
