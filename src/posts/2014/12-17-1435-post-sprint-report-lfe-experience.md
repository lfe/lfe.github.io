---
layout: post.liquid
title: "Post Sprint Report: LFE Experience"
description: "A first-encounter with LFE from an Erlang appreciator with a Lisp background."
permalink: "/blog/reports/2014/12/17/1435-post-sprint-report-lfe-experience"
categories: ["reports"]
tags: ["community", "engineering-teams", "lfe-in-the-wild", "ux"]
published_date: 2014-12-17 14:35:00 +0000
is_draft: false
data:
  author: anurag-mendhekar
  written_for: null
  last_validated: null
  cover_image: "/images/default/LFE_00218_.png"
  cover_alt: "Vigdís — LFE, retro-futurist digital painting"
  math: false
---
<a href="/blog/assets/images/posts/LispFlavoredErlang-medium-square.png"><img class="right small" src="/blog/assets/images/posts/LispFlavoredErlang-medium-square.png" /></a>My
team and I have just finished our first major sprint using LFE and I thought
this would be a good time to report on our experience. Our stack is built around
[YAWS](http://yaws.hyber.org/) and we are using MySQL as the database. We're not
using any templating language, but instead relying on an API-based architecture
with all front-end interactions in JavaScript + HTML.


## Introduction

We are a small team of developers who are trying to get a pretty major web
application launched in 12 weeks. This is a challenge in its own right, but
as we were evaluating web platforms, we ended up rejecting most others in
favor of Erlang. The choice was made collectively by all the devs who
evaluated all of our different options (the others being PHP, node.js,
Clojure, Scala, Haskell).  We picked Erlang primarily for the scalability,
reliability and ecosystem support.

Of the team, I am the only experienced Lisper (Scheme, Racket, CL). I was
naturally attracted to LFE because of the philosophy of staying close to
Erlang, but still being a Lisp. I further made the decision that at least
some people would use LFE for development, perhaps mixed with Erlang. So we
now have a team that is learning Erlang and LFE at the same time. The devs
are all very competent, but this learning curve is our biggest risk at this
point. It is my hope that LFE will help us transcend the learning curve.
There are some indications that this is happening, but I will know more
for sure in a few more days.

## The Good

1. I am impressed at the correctness of the implementation. I have not
   seen unpredictable behavior yet and have unearthed no major correctness
   issues in the language implementation. Kudos to the team for that!

1. I like the closeness to Erlang. Being able to mindlessly call Erlang
   modules is a huge plus. Including .hrl files and having the records
   available in the defrecord form is very convenient. While I am no Erlang
   expert, I find converting cut and pasted Erlang code to LFE quite easy.
   So far, I only had a little bit of trouble with Bit comprehensions on
   that front. But, I suspect that is more because I am new to Erlang.

1. The compiler seems fast enough, but I'll know more as the number of
   source files grow.


## The Moderately Good

1. **Documentation**: ... It exists, but things are hard to find. I realize
   this is a work in progress and hopefully we will be able to help in some
   ways.

1. **The LFE REPL**: I was thankful for it for understanding behavior which
   was not in the documentation or if I was too lazy to look it up.

1. **Unit Test Framework**: Useful for many cases, but it took me a while to
   get it working correctly. I had trouble understanding test outputs,
   and the inability to isolate runs to specific tests was a little painful.


## The Frustrations

Please bear with me on this. Not all of this relates to LFE entirely, but I
feel it better to list it here in case LFE can have better solutions. The
section is bigger because I'm trying to give all the gory details.


### The Macro System

About 80% of the code I wrote in the past 10 days is for macros. It was a
painful experience.

* The biggest problem is that the macro system does not report errors properly.
  If an expansion encounters an error, all that the compiler reports is
  something like "could not expand form". Or, even more frustratingly
  "bad application". No other information is provided about what the error
  was and where it arose. This was true in the LFE REPL as well. It would
  REALLY *REALLY* help if the underlying error is reported.

* I got around these issues by writing helper functions which I could debug
  in the repl, but it took me a while to settle on this methodology. It
  makes my macros a trivial shell over a helper function, which makes for
  uglier code.

Note, however, the macro system works correctly once the macros are debugged. There are no issues with correctness.

### Strings/Binary Complexity

My ideal situation here would be that LFE provide a mechanism for letting
developers choose to always use Erlang binaries. We are
[currently discussing exactly this](https://groups.google.com/d/msg/lisp-flavoured-erlang/pF4retTadzw/bwmz6a6BNZYJ)
on the mail list.

### Erlang's Formatted I/O

Given my long exposure to Common Lisp's format, I feel that ``"~p"`` is woefully
inadequate. One of my resolutions is to contribute a CL compatible format
function to LFE. I would love to collaborate with anyone who is interested
in this.


### One Right Way

One other thing on my wish-list would be that LFE be a little more opinionated
on syntax choices. Allowing multiple ways to define functions etc., while
helpful to lazy old goats like me, probably complicates the language more
than necessary. It may be best left to user-defined macros to provide
alternatives.


## Closing Thoughts

Anyhow, that is all for now. More as things develop. Congratulations on
getting LFE this far.

I am working on a document called "LFE for Erlang programmers", which I will
publish to this group as soon as it reaches some level of stability.

