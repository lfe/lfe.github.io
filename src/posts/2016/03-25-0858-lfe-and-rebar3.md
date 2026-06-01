---
layout: post.liquid
title: "LFE and rebar3"
description: ""
permalink: "/blog/tutorials/2016/03/25/0858-lfe-and-rebar3"
categories: [tutorials]
tags: [howto, libraries, tools, utilities, rebar3]
published_date: 2016-03-25 08:58:00 +0000
is_draft: false
data:
  author: duncan-mcgreggor
  written_for:
    lfe: "1.2"
    erlang: "18"
  last_validated: null
  cover_image: null
  cover_alt: null
  math: false
---

<a href="/blog/assets/images/posts/lfe-rebar3-logo.png"><img class="left thumb" src="/blog/assets/images/posts/lfe-rebar3-logo.png" /></a>
This post provides a quick introduction to using ``rebar3`` in LFE projects or
in Erlang+LFE projects. After this quick read, you'll be able to jump right in
to LFE development in projects managed with ``rebar3`` :-)
<br /> <br />

With the support of namespaces landing in ``rebar3`` last year, the LFE
community started making slow but steady progress toward adopting ``rebar3`` as
its preferred tool for building projects, running tests, creating releases,
examining dependency trees, generating docs, and creating generally useful,
project-oriented command-line tools.

As a community, we've still got a ways to go before we completely reproduce all
the functionality that has been provided by ``lfetool``, but every new LFE
plugin brings us closer to that goal.

Note that this post assumes that you already have Erlang installed on your
system.


## Installing ``rebar3``

There are several ways to get ``rebar3`` (including building it yourself), but
most of us use the readily available option provided on the project's
[README](https://github.com/erlang/rebar3):

```bash
$ wget https://s3.amazonaws.com/rebar3/rebar3 && chmod +x rebar3
$ sudo mv rebar3 /usr/local/bin
```

This is the nightly build. I have been very happy with the nightlies and have
used them consistently in both development and production without any issues.
If you'd like to read more about installing ``rebar3``, be sure to check out
the [getting started page](http://www.rebar3.org/docs/getting-started).


## Compiling LFE Files

With ``rebar3`` installed, you're ready to rock and roll: bringing LFE into an
Erlang project is as simple as updating your ``rebar.config`` file. (Note that
if you are moving from ``rebar`` to ``rebar3``, it is indeed a *move*:
for the most part, ``rebar3`` configuration options are not backwards
compatible with ``rebar``).

If you've got an Erlang project and you want to support adding LFE files to it,
here's all you need to do to the project's ``rebar.config``:

```erlang
{plugins, [
   {'lfe-compile',
     {git, "https://github.com/lfe-rebar3/compile.git", {tag, "0.3.0"}}}
  ]}.

{provider_hooks, [{pre, [{compile, {lfe, compile}}]}]}.
```

Then, add ``.lfe`` source files to your heart's content. When done, simply
compile:

```bash
$ rebar3 compile
```

This will pull down all the dependencies that the LFE-rebar3 plugin has
(including LFE itself), compile all your ``.erl`` files that ``rebar3``
normally does, and then compile any ``.lfe`` files you have added.

Note that if you only want to (re-)compile ``.lfe`` files, you may explicitly
call the plugin's compile command:

```bash
$ rebar3 lfe compile
```

If your project is LFE-only (no ``.erl`` files), the set up is exactly the
same.

## Bringing in LFE Libraries

If you'd like to add some of the [community LFE
libraries](https://github.com/lfex) to your project, or pull in libraries from
somewhere else, simply add them to the ``deps`` section in the
``rebar.config``. Here's how you add the LFE library which allows you to use
some Clojure idioms in your LFE code (e.g., the ``->`` and ``->>`` thrushing
macros):

```erlang
{deps, [
   {clj, ".*", {git, "git://github.com/lfex/clj.git", {tag, "0.4.0"}}}
  ]}.
```

Running ``rebar3 compile`` again will pull down this library and all of its
dependencies, making all of their modules and include files available for use
in your LFE code. (Note that if you want to use the thrushing macros, you'll
need to put ``(include-lib "clj/include/compose.lfe")`` after your
``defmodule``.)


## LFE Plugins

The LFE compile plugin is written in Erlang. However, all the other LFE-rebar3
plugins currently under development are written in LFE itself. There are a few
[small utility plugins](https://github.com/lfe-rebar3) that we've been
experimenting with, but perhaps the most useful plugin right now is the
[lodox](https://github.com/lfe-rebar3/lodox) documentation-generator (inspired
by Clojure's [Codox](https://github.com/weavejester/codox)).

If you'd like to use this plugin, simply add it to your other ``rebar3``
plugins:

```erlang
{plugins, [
   {'lfe-compile',
     {git, "https://github.com/lfe-rebar3/compile.git", {tag, "0.3.0"}}},
   {lodox,
     {git, "https://github.com/lfe-rebar3/lodox.git", {tag, "0.12.10"}}}
  ]}.
```

And then you are ready to use it to generate HTML docs for your LFE source
code:

```bash
$ rebar3 lfe lodox
```


## ``rebar3`` Features

``rebar3`` is a fantastic tool with a whole suite of incredibly useful
features. If you run the following command:

```bash
$ rebar3 --help
```

part of the output will show the standard commands that come with the tool
(many of which also take their own subcommands; try out ``rebar new --help``):

```
as                Higher order provider for running multiple tasks in ...
clean             Remove compiled beam files from apps.
compile           Compile apps .app.src and .erl files.
cover             Perform coverage analysis.
ct                Run Common Tests.
deps              List dependencies
dialyzer          Run the Dialyzer analyzer on the project.
do                Higher order provider for running multiple tasks in ...
edoc              Generate documentation using edoc.
escriptize        Generate escript archive.
eunit             Run EUnit Tests.
help              Display a list of tasks or help for a given task or subtask.
new               Create new project from templates.
path              Print paths to build dirs in current profile.
pkgs              List available packages.
release           Build release of project.
relup             Create relup of releases.
report            Provide a crash report to be sent to the rebar3 issues page.
shell             Run shell with project apps and deps in path.
tar               Tar archive of release built of project.
tree              Print dependency tree.
unlock            Unlock dependencies.
update            Update package index.
upgrade           Upgrade dependencies.
version           Print version for rebar and current Erlang.
xref              Run cross reference analysis.
```

Note that if you have a bunch of LFE plugins configured for use in your
project, you'll also see those displayed as part of the output of
``rebar3 help``:

```
lfe <task>:
  clean          The LFE rebar3 clean plugin.
  compile        The LFE rebar3 compiler plugin
  lodox          Generate documentation from LFE source files.
  repl           The LFE rebar3 LFE REPL plugin.
  test           The LFE rebar3 test plugin.
  version        The LFE rebar3 version plugin.
```

I've found the ``tree`` command quite helpful to track down explicit
dependencies and their versions when debugging issues between projects:

```bash
$ rebar3 tree
```
```
===> Verifying dependencies...
└─ lsci─0.0.2 (project app)
   ├─ encurses─0.4.1 (git repo)
   └─ py─0.0.5 (git repo)
      ├─ erlport─0.9.8 (git repo)
      ├─ logjam─0.4.0 (git repo)
      │  ├─ color─0.2.0 (git repo)
      │  ├─ lager─3.1.0 (git repo)
      │  │  └─ goldrush─0.1.8 (git repo)
      │  └─ lcfg─ (git repo)
      │     └─ ltest─0.8.0 (git repo)
      └─ lutil─0.8.0 (git repo)
         ├─ clj─0.4.0 (git repo)
         │  └─ kla─0.6.0 (git repo)
         └─ lfe─1.0 (git repo)
```


## Profiles

This ``rebar3`` feature is important enough for its own section :-) You can
read about ``rebar3`` profiles [here](https://www.rebar3.org/docs/profiles) but
in short, they allow you to split up potentially very large ``rebar.config``
files into sections that will only get run if a particular profile is active.

Practically speaking, this means that you can do things like the following:

* allow your users to only download the minimum dependencies when compiling
  your library
* for example, only downloading and compiling the testing framework when
  running tests, or
* only downloading doc tools when running the "docs" profile
* providing developer tools only run running the "dev" profile
* set explicit dependencies on a per-profile basis
* avoid some cyclic dependency issues


This drastically reduced download and compile times for several of our more
dependency-heavy libraries. [Here's an
example](https://github.com/lfex/exemplar/blob/master/rebar.config) of an LFE
project's ``rebar.config`` file making good use of the profiles feature. In
that example, you can run the following commands to perform the per-profile
actions:

Run the unit tests:

```bash
$ rebar3 as test eunit
```

Get detailed version info:

```bash
$ rebar3 as dev lfe version
```

Build the API reference for the project:

```bash
$ rebar3 as docs lfe lodox
```

We've only just started using ``rebar3`` profiles in some of the LFE libraries,
but we're already pretty big fans.


## Community Goals

Our ultimate goal with rebar3 is to provide a suite of genuinely useful LFE
plugins (written in LFE) that devs adore using. One of the things we've got
slated for up-coming hack time is templates for LFE projects: not only
providing LFE versions of the Erlang ones that ship with ``rebar3`` but also
YAWS, elli, and LFE Dragon templates.

