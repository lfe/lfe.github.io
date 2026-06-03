---
layout: post.liquid
title: "lfetool v1.2 Is Out!"
description: "Notes on the latest interim release of lfetool"
permalink: "/blog/announcements/2014/08/31/2024-lfetool-v12-is-out"
categories: ["announcements"]
tags: ["releases", "updates"]
published_date: 2014-08-31 20:24:00 +0000
is_draft: false
data:
  author: duncan-mcgreggor
  written_for: null
  last_validated: null
  cover_image: "/images/newsroom/LFE_Newsroom_00298_.png"
  cover_alt: "Vigdís — LFE news, a busy rotating space-station newsroom"
  math: false
---

We've just pushed out the latest version of lfetool in the v1.x series. This
was done as a result of several bug reports and conversations made on IRC
(#erlang-lisp, Freenode).

You can get the latest here:

* [https://github.com/lfe/lfetool/releases/tag/1.2.0](https://github.com/lfe/lfetool/releases/tag/1.2.0)
* [https://github.com/lfe/lfetool/tree/milestone-v1.2](https://github.com/lfe/lfetool/tree/milestone-v1.2)

The [LFE Quick Start](http://docs.lfe.io/quick-start/1.html) has also been
updated so that newcomers start off right with the latest stable release of
lfetool :-)

The issues addressed in the 1.2 release can be found here:

* [Milestone 1.2 Issues](https://github.com/lfe/lfetool/issues?q=milestone%3A%22Version+1.2%22+is%3Aclosed)

Most of the bug reports actually boiled down to issues with conflicting
versions of dependency libraries in project rebar.config files. There was a
recent switch to lutil (away from lfe-utils) and ltest (away from lfeunit and
lunit), and older versions of lfetool couldn't handle these. Also, some changes
made to lfeutils and or lfeunit just prior to the switch caused some cyclic
dependencies in rebar.

Do keep in mind that there's a lot of work going into the v2 rewrite of lfetool
(code is being converted from Bash to LFE), and this release isn't a
distraction from this effort, but rather an attempt at an improved user
experience for current users :-)
