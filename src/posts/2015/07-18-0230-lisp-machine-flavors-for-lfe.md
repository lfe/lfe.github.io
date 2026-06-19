---
layout: post.liquid
title: "Lisp Machine Flavors for LFE"
description: ""
permalink: "/blog/tutorials/2015/07/18/0230-lisp-machine-flavors-for-lfe"
categories: ["tutorials"]
tags: ["lisp-machine", "flavors"]
published_date: 2015-07-18 02:30:00 +0000
is_draft: false
data:
  author: robert-virding
  written_for: null
  last_validated: null
  cover_image: "/images/tutorials/LFE_Tutorial_00392_.png"
  cover_alt: "Vigdís — LFE tutorial, a retro-futurist study aboard a spaceship"
  math: false
---
<a href="/blog/assets/images/posts/lm_logo.png"><img class="right tiny" src="/blog/assets/images/posts/lm_logo.png" /></a>Long, long ago I did an implementation of MIT Lisp Machine Flavors for Portable Standard Lisp. This was a very interesting project but it is now all lost in antiquity.

When I mentioned to Duncan that I had done this he asked why not do it for LFE? After thinking it over I decided that this was definitely worth doing for many reasons:

* It was in itself a very interesting and powerful example of an OO
  system with many, many features.

* In many ways the Erlang/LFE system is very OS like in its nature and
  so resembles the Lisp Machine in being more like an implementation
  of a system with a language rather than just an implementation of a
  language. Of course Erlang is not lisp but with LFE we are
  definitely closer.

* It would definitely be a fun project.

So, LM flavors it is. There are many things from the LM lisp we can't
do in LFE, for example mutable data and global variables, or are very
dependent on the internals of the LM so it won't be an exact
implementation, but I will do what I can.

One very interesting question when it starts working is how deeply we
should try and integrate this into LFE. If we do make it part of the
core LFE system it will influence a lot of other libraries and
tools. Also there maybe other things from the LM we might want to
include as well. We might end up with an LFE machine.

A link to the [LM documentation](http://www.bitsavers.org/pdf/mit/cadr/chinual_6thEd_Jan84/) and some other interesting links [CADR lisp machine](http://www.unlambda.com/cadr/index.html) and [Lisp machines](http://c2.com/cgi/wiki?LispMachine)

Robert

P.S. Defstruct is calling me.
