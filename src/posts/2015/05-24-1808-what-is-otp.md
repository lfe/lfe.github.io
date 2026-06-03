---
layout: post.liquid
title: "What is OTP?"
description: "A quick examination of OTP"
permalink: "/blog/tutorials/2015/05/24/1808-what-is-otp"
categories: [tutorials]
tags: [otp, erlang]
published_date: 2015-05-24 18:08:00 +0000
is_draft: false
data:
  author: duncan-mcgreggor
  written_for:
    lfe: "0.10"
    erlang: "18"
  last_validated: null
  cover_image: "/images/tutorials/LFE_Tutorial_00379_.png"
  cover_alt: "Vigdís — LFE tutorial, a retro-futurist study aboard a spaceship"
  math: false
---

<a href="/blog/assets/images/posts/LFE-signal.jpg"><img class="right thumb" src="/blog/assets/images/posts/LFE-signal.jpg" /></a>This post is a prelude to the LFE OTP tutorial series which attempts to clarify the role of OTP as something above and beyond a programming language, a set of libraries, or a framework.


## LFE OTP Tutorial Series

* [Introducing the LFE OTP Tutorials](/blog/tutorials/2015/05/23/1720-new-series-lfe-otp-tutorials/)
* [What is OTP?](/blog/tutorials/2015/05/24/1808-what-is-otp/)
* [Prelude to OTP](/blog/tutorials/2015/05/25/0929-prelude-to-otp/)
* [Creating LFE Servers with OTP, Part I](/blog/tutorials/2015/05/26/1112-creating-servers-with-the-gen_server-behaviour/)
* [Creating LFE Servers with OTP, Part II](/blog/tutorials/2015/05/28/1008-creating-servers-with-the-gen_server-behaviour-ii/)
* [Distributed LFE](/blog/tutorials/2015/09/18/1604-distributed-lfe/)

You can leave feedback for the LFE OTP tutorials
[here](https://github.com/lfe/blog/issues/7).


## Pattern Languages

The term *pattern language* was coined by architect
[Christopher Alexander](http://en.wikipedia.org/wiki/Christopher_Alexander),
covered in detail in his 1977 book
[A Pattern Language](http://www.amazon.com/dp/0195019199),
where he wove together themes of architecture, urban design, and community
livability. The idea of a *pattern language* was applied to software and
entered the collective consciousness of the programming world the in the book
[Design Patterns: Elements of Reusable Object-Oriented Software](http://www.amazon.com/dp/0201633612).

A pattern language is a method of describing good design practices within a
field of expertise.  What brings a sense of wholeness, spirit, or grace to this
field? What precise and empirically verifiable generalizations can be made with
regard to recurring themes in this field? A pattern language is an attempt to
express the deeper wisdom underlying the answers to these questions through a
set of interconnected expressions arising from that wisdom.


## OTP as a Pattern Language

In essence, OTP is a pattern language: one for building fault-tolerant,
distributed systems. That's easy enough to say, and many languages or
frameworks say similar things about their own work. But what does this really
mean? What is *wholeness* from the perspective of OTP? What is the *spirit* of
OTP?  What makes an OTP application *graceful*? What *wisdom* has been
extracted from these? What *interconnected expression* arose from that wisdom?

Though we could point to various behaviours, ways of managing state, excellent
uses of pattern matching and LFE records, macros, etc., or share stories of the
accumulated hard-won experiences of Erlang engineers -- all in an effort to
answer these questions, it would actually be more meaningful for you to
discover this yourself in the coming posts.

Note that we are not shirking our tutorial responsibilities in making that
statement. Quite the contrary! In the course of working through these
tutorials, you will gain the actual joys of OTP's elegance. By the time you
have finished these LFE exercises, giving them due time and consideration, you
will be able to answer all of these questions from your own direct experience.

Lest we leave you adrift in too much poetry and promises, we will say the
following right now: OTP as a pattern language attempts to provide encoded
wisdom for the programmer who wishes to create stable, highly-concurrent,
fault-tolerant distributed systems. The OTP pattern language is a combination
of such things as:

 * **Behaviours** - these are the core patterns in the OTP pattern language,
   embodying lessons-learned in building concurrent systems
 * **Programming Style** - common idioms shared across a community of
   developers
 * **Best practices** - how to connect one or many behaviour implementations in
   one or more deployable applications.

OTP behaviours include the following:

 * **Workers** (``gen_server``, ``gen_fsm``, ``gen_event``)
 * **Supervisors** (used for building supervision trees)
 * **Applications** (collections of supervisors and workers and their
   subsequent supervision trees in combination with the means to effectively
   manage them)

We will cover programming style and best practices in each LFE example we go
over.


## Up Next

The next post will whet your OTP appetite with some example code some simple
servers created in the LFE edition of
[Casting SPELs in Lisp](http://lfe.gitbooks.io/casting-spels/content/book/part7/README.html).
Each example is cumbersome in its own right and will help motivate finding a
better way to create long-running processes.

