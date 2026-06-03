---
layout: post.liquid
title: "Usability Update: IErlang Notebook (a la IPython)"
description: "Updated project source for IErlang makes it easier to use"
permalink: "/blog/announcements/2014/12/06/1244-ierlang-notebook-a-la-ipython"
categories: ["announcements"]
tags: ["erlang", "tools", "web", "shells"]
published_date: 2014-12-06 12:44:00 +0000
is_draft: false
data:
  author: duncan-mcgreggor
  written_for: null
  last_validated: null
  cover_image: "/images/newsroom/LFE_Newsroom_00305_.png"
  cover_alt: "Vigdís — LFE news, a busy rotating space-station newsroom"
  math: false
---
<a href="/blog/assets/images/posts/screencapture-ierlang_demo.png"><img class="left thumb" src="/blog/assets/images/posts/screencapture-ierlang_demo.png" /></a>Back in April, the Erlang community
was stunned to hear that we had been given an answer to
[IPython](http://ipython.org/), IHaskell, and IJulia --
[IErlang](http://robl.co/ierlang-featured-on-hacker-news/). However, as
[Robbie](https://twitter.com/lynchrobbie) noted in the list of outstanding
issues at the end of the IErlang demo notebook, not a lot of time had been set
aside to develop a more standard project structure. Furthermore, the setup and
installation of IErlang to get to the point where you could try it out was a
rather arduous process. All of that has now changed ...


## Running the New Demo

The IErlang project has a series of
[open PRs](https://github.com/robbielynch/ierlang/pulls) that address several
of the organizational issues Robbie had lamented, but you don't have to wait
until they get merged; you can
[get it now](https://github.com/oubiwann/ierlang):

```bash
$ git clone git@github.com:oubiwann/ierlang.git
$ cd ierlang
$ make demo
```

Yup. That's it.

This will do the following for you:

 * Set up a Python virtual environment in the project's working directory (in
   ``./.venv-py2``)
 * Install IPython and its dependencies into that virtual environment
 * Patch IPython to support Erlang
 * Download the Erlang project dependencies
 * Compile the Erlang dependency modules
 * Compile the ``ierl_*`` modules
 * Start up the demo notebook in your browser


## Requirements

As you might have guessed, this requires that you have the following
installed in your ``$PATH``:

 * ``erl`` and ``erlscript``
 * ``rebar``
 * Python 2.7 (Python 3.4 support is in the works; see
   [the ticket](https://github.com/robbielynch/ierlang/issues/6))


## Outstanding Issues

As you can see at the bottom of the IErlang notebook demo, many of the issues
that Robbie identified earlier this year still remain. In particular, I've
added another one to that list: high CPU usage for both ``console`` and
``notebook`` mode.

A minor issue I didn't add, but which I may take up (being the code janitor
that I am) is function-level organization in the modules: most functions are
too long and entail too much logic. They need to be split out (which will
also make it easier for folks to contribute).

As referenced above, currently only Python 2.7 is supported. Python 3.4 support
is in-progress, but requires cleaning up ``str`` and ``bytes`` handling.


## Future Work

The next logical step is to generalize this work (through refactoring!) so that
[LFE](http://lfe.io/),
[Elixir](http://elixir-lang.org/),
[Joxa](http://joxa.org/),
[Luerl](https://github.com/rvirding/luerl),
[Erlog](https://github.com/rvirding/erlog),
[Haskerl](https://github.com/etnt/Haskerl),
and others may also be used with IErlang :-)

