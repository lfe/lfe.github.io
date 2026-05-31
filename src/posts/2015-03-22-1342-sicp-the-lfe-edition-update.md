---
layout: post.liquid
title: "SICP: the LFE Edition - Update"
description: "An update on the LFE edition of SICP and a note about the new preface"
permalink: "/blog/update/2015/03/22/1342-sicp-the-lfe-edition-update"
categories: ["update"]
tags: ["books", "news", "docs", "education", "sicp"]
published_date: 2015-03-22 13:42:00 +0000
is_draft: false
data:
  author: duncan-mcgreggor
  written_for: null
  last_validated: null
  cover_image: null
  cover_alt: null
  math: false
---
<a href="/blog/assets/images/posts/sicp.jpg"><img class="right small" src="/blog/assets/images/posts/sicp.jpg" /></a>
The LFE Edition of the historic *Structure and Interpretation of Computer
Programs* ([book](http://lfe.gitbooks.io/sicp/content/index.html),
[source](https://github.com/lfe/sicp)) continues to make steady progress,
including getting a new preface. This post is a teaser for additional posts
which will appear here in the coming days. But first, here's how things stand
so far:

 * Chapter 1 is completed
 * Chapter 2 is about half-way done
 * There is a new (additional) preface for the LFE edition (more on that in
   the next post)
 * To date, there have been 17,100 views of LFE SICP
 * There have been 3,300 full reads of the book
 * There have been 500 downloads of the eBook versions [^1]

As an LFE programmer, the most amazing thing to see is how extraordinarily
well-suited LFE is to the task of teaching the material in SICP -- more often
than not, the LFE versions of the sample programs are more elegant, succinct,
and expressive.[^2] For the most post, though, the text remains very much as
it was in beauty of the original.

One significant exception we've made to this is the addition of a third
preface. Though explicitly for the third (LFE) edition, it only discusses
Erlang and LFE briefly. Instead, we have taken the opportunity to provide a
personal touch to the history of Lisp's development via short biographies of a
selection of mathematicians who made significant contributions to such
developments as number and function theory, logic, the philosophy of
mathematics, the λ&#8209;calculus, and the Lisp itself.

Over the coming days, these will be posted in serial form on the LFE blog to
share some of this information with a wider audience, and for the motivated and
interested reader, to [receive feedback](https://github.com/lfe/sicp/issues/6)
on how we might make improvements to the LFE preface.

More is on its way ...

----

[^1]: We don't actually recommend the GitBook eBook downloads for SICP yet,
      as they do not have good support for LaTeX in the eBook versions; the
      web version, though, presents all of the LaTeX in its eye-popping,
      mathematical glory.

[^2]: The one exception of that, is of course, the fact that LFE is a Lisp-2
      and not a Lisp-1 like Scheme; as such, function and lambda application is
      not as elegant in LFE as it is in the code from the first and second
      editions of SICP.
