---
layout: post.liquid
title: "spell1 - LL(1) parser generator update"
description: ""
permalink: "/blog/tutorials/2015/06/23/0353-spell1---ll1-parser-generator-update"
categories: ["tutorials"]
tags: ["spell1", "lfe", "erlang", "tools"]
published_date: 2015-06-23 03:53:00 +0000
is_draft: false
data:
  author: robert-virding
  written_for: null
  last_validated: null
  cover_image: null
  cover_alt: null
  math: false
---
<a href="/blog/assets/images/posts/lfe-tooling-leonardo-gears-2.png"><img class="right small" src="/blog/assets/images/posts/lfe-tooling-leonardo-gears-2.png" /></a> Work has been proceeding with spell1 and we now have something useable. There are now two front-ends for the language of the grammar files and output files, one for handling Erlang, and the other for handling LFE.

The way the spell1 code is split makes it quite straight-forward to add front-ends for other languages. Even to have the grammar file in one syntax and the output file in another.

Work is now being done to automatically handle left-recursion and multiple rules with the same prefix.

Comments welcome.

Robert
