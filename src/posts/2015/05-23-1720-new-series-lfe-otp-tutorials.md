---
layout: post.liquid
title: "New Series: LFE OTP Tutorials"
description: "Kicking off a series of OTP tutorials for LFE hackers"
permalink: "/blog/tutorials/2015/05/23/1720-new-series-lfe-otp-tutorials"
categories: ["tutorials"]
tags: ["otp", "erlang"]
published_date: 2015-05-23 17:20:00 +0000
is_draft: false
data:
  author: duncan-mcgreggor
  written_for: null
  last_validated: null
  cover_image: "/images/tutorials/LFE_Tutorial_00365_.png"
  cover_alt: "Vigdís — LFE tutorial, a retro-futurist study aboard a spaceship"
  math: false
---
<a href="/blog/assets/images/posts/LFE-signal.jpg"><img class="right tiny" src="/blog/assets/images/posts/LFE-signal.jpg" /></a>This post introduces an new series of tutorials on the LFE blog which will step LFE users through OTP in the style of Lisp Flavoured Erlang.

## LFE OTP Tutorial Series

* [Introducing the LFE OTP Tutorials](/blog/tutorials/2015/05/23/1720-new-series-lfe-otp-tutorials/)
* [What is OTP?](/blog/tutorials/2015/05/24/1808-what-is-otp/)
* [Prelude to OTP](/blog/tutorials/2015/05/25/0929-prelude-to-otp/)
* [Creating LFE Servers with OTP, Part I](/blog/tutorials/2015/05/26/1112-creating-servers-with-the-gen_server-behaviour/)
* [Creating LFE Servers with OTP, Part II](/blog/tutorials/2015/05/28/1008-creating-servers-with-the-gen_server-behaviour-ii/)
* [Distributed LFE](/blog/tutorials/2015/09/18/1604-distributed-lfe/)

There aren't a lot of OTP tutorials online. One really good one
([starts here](http://learnyousomeerlang.com/what-is-otp#its-the-open-telecom-platform))
is in Fred's most excellent book
[Learn You Some Erlang for great good!](http://learnyousomeerlang.com/). Other
resources include:

* Joe Armstrong's [Programming Erlang](https://pragprog.com/book/jaerlang2/programming-erlang)
  including the chapters "Introducing OTP" and "Making a System with OTP"
* Francesco and Simon's [Erlang Programming](http://shop.oreilly.com/product/9780596518189.do),
  Chapter 12
* All of Francesco and Steve's [Designing for Scalability with Erlang/OTP](http://shop.oreilly.com/product/0636920024149.do)
* Martin, Eric, and Richard's [Erlang and OTP in Action](http://www.manning.com/logan/),
  including Chapter 4 and Chapter 8 (among others)

There are bits and pieces in blog posts, documentation, etc., but nothing that
really gives a complete picture, is easy to grasp for newcomers, and which
keeps the code very clean and clear.

And there is absolutely nothing for LFE.

This series will attempt to alter this landscape somewhat, if only to address
the last point. The idea will be to refer readers to these excellent books for
in-depth looks into the topics that we cover, but to provide a clear howto for
basic concepts and getting started.

We do want to make this an amazing OTP tutorial series, so
please [leave feedback](https://github.com/lfe/blog/issues/7) if you have ideas
on how to do that.

The plan as it stands right now is to cover the following, with super-explicit
code (no "magical" implicit parameters):

* Creating services with the ``gen_server`` behaviour
* How to use the ``gen_fsm`` behaviour
* Working with ``gen_event`` and notifications
* Using ``supervisor`` to create supervision trees
* Creating applications with the ``application`` behaviour
* Making a "release" with ``relx``

Depending upon content covered and the size of any give post's draft, these may
get split into more than just those six. Watch the skies! Wait for the signal!
LFE OTP tutorials are on their way!


