---
layout: post.liquid
title: "The Hidden Origins of Lisp: Introduction"
description: ""
permalink: "/blog/excerpts/2015/03/22/1445-the-hidden-origins-of-lisp-introduction"
categories: ["excerpts"]
tags: ["books", "documentation", "education", "history", "lisp", "mathematics", "sicp"]
published_date: 2015-03-22 14:45:00 +0000
is_draft: false
data:
  author: duncan-mcgreggor
  written_for: null
  last_validated: null
  cover_image: null
  cover_alt: null
  math: false
---
<a href="/blog/assets/images/posts/sicp.jpg"><img class="right medium" src="/blog/assets/images/posts/sicp.jpg" /></a>
As [mentioned earlier](https://lfe.io/blog/update/2015/03/22/1342-sicp-the-lfe-edition-update/),
this post kicks off a blog series highlighting the new preface for the LFE
edition of *Structure and Interpretation of Computer Programs*.
([book](http://lfe.gitbooks.io/sicp/content/index.html),
[source](https://github.com/lfe/sicp)) The posts will be comprised of the
preface introduction and then a collection of short biographies as pertains to
the foundation (laid by the given individual) which ended up being crucial to
the development of Lisp, concluding with a section regarding Lisp's important
role in the future of computer science and related industries:

 * [Introduction](/excerpts/2015/03/22/1445-the-hidden-origins-of-lisp-introduction/)
 * [Giuseppe Peano](/excerpts/2015/03/23/1008-the-hidden-origins-of-lisp-peano/)
 * [Bertrand Russell](/excerpts/2015/03/24/0111-the-hidden-origins-of-lisp-russell/)
 * [Alonzo Church](/excerpts/2015/03/25/1108-the-hidden-origins-of-lisp-church/)
 * [John McCarthy](/excerpts/2015/03/26/1111-the-hidden-origins-of-lisp-mccarthy/)
 * [The Place of Lisp in the 21st Century](/excerpts/2015/03/27/1101-the-hidden-origins-of-lisp-future)

The LFE preface in SICP also has a quick summary of the origins of Erlang
and of LFE itself, discusses changes from the 2nd edition of SICP, then
closes with instructions on how to obtain the source for the original, for the
LFE edition, and for the code used in the LFE edition's chapters. Those bits
are available in the book and won't be presented in this series of the LFE
blog.

If you find any issues or have questions, concerns, etc., you may provide
feedback about the preface in the
[dedicated ticket](https://github.com/lfe/sicp/issues/6) created for tracking
such things.

Today we start with the preface introduction; look for the remaining sections
in the coming days.


## The Hidden Origins of Lisp: Introduction

Beginnings are important. They may not fully dictate the trajectory of their
antecedents, yet it does seem they do have a profound impact on the character
of their effects. For the human observer, beginnings are also a source of
inspiration: good beginnings lend a strength of purpose, the possibility of
greater good. The story of Lisp has a good beginning -- several of them, in
fact -- closely tied to the theories of numbers, mathematical logic, functions
and types as well as that of computing itself.

At their root, the histories of programming languages spring from, on one hand,
the practical considerations of engineering and developer experience, and on
the other hand, the principle of computability. This, in turn, ultimately
traces its beginnings to the fundamental concepts of arithmetic and
mathematical logic: what are numbers and how to we define them rigorously?
These questions were asked and considered -- sometimes from a fairly vague
philosophical perspective -- by great minds such as Leibniz (later 1600s;
drafts published posthumously), Boole (1847), Grassmann (1861), Pierce (1881),
Frege (1884), and Dedekind (1888). It was the Italian mathematician Giuseppe
Peano, though, who in 1889 finally identified and distilled the essence of
these explorations in terms that were more precisely formulated than those of
his peers or intellectual fore bearers. These were subsequently elaborated by
successive generations of mathematicians prior to the advent of "high-level"
programming languages in the 1950s.

Histories are complicated; complete ones are impossible and readable ones are
necessarily limited and lacking in details. In our particular case, there is a
complex lineage of mathematics leading to Lisp. However, for the sake of
clarity and due to this being a preface and not a book in its own right, the
mathematical and computational history leading to Lisp has been greatly
simplified in this preface. The four dominant historical figures discussed
provide distinct insights and represent corresponding themes as mathematics
evolved unwittingly toward a support for computing. Due to the limitation of
scope, however, it might be better to view these as archetypes of mathematical
discovery rather than historical figures one might come to know when reading a
full history. Of the many themes one could discern and extract from these great
minds, we focus on the following:

* Understanding and defining the underpinnings of arithmetic and logic ("What are
  numbers? What is counting?")
* Attempting to formally unify all of mathematics in a consistent framework of
  logic ("Can I express all of math in discrete logical assertions and
  statements?")
* Formally defining algorithms and computability ("Is there a procedure that can
  take any precise mathematical statement and decide whether the statement is
  true or false?")
* Creating the means by which symbolic computation and artificial reasoning could
  be made manifest ("Can we make machines solve problems that are usually
  considered to require intelligence?") [^1]

Each major topic above depended -- in one form or another -- upon the preceding
topic, and the four famous mathematicians listed in the subsequent sections
embodied each of these themes. Small excerpts from their lives and work are
shared as believed to have impacted the course of events that lead to Lisp's
inception.


----

[^1]: This is an almost word-for-word quote from John McCarthy's January 1962
      submission in the quarterly progress report for MIT's RLE, titled
      [XXI. ARTIFICIAL INTELLIGENCE](http://dspace.mit.edu/bitstream/handle/1721.1/53661/RLE_QPR_064_XXI.pdf),
      page 189 on the original hard copy. The table of contents for the
      original is available
      [here](http://dspace.mit.edu/bitstream/handle/1721.1/53645/RLE_QPR_064_TOC.pdf).
