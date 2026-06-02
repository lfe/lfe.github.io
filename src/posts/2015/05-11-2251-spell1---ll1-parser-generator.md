---
layout: post.liquid
title: "spell1 - LL(1) parser generator"
description: ""
permalink: "/blog/design/2015/05/11/2251-spell1---ll1-parser-generator"
categories: ["design"]
tags: ["spell1", "lfe", "erlang", "tools"]
published_date: 2015-05-11 22:51:00 +0000
is_draft: false
data:
  author: robert-virding
  written_for: null
  last_validated: null
  cover_image: "/images/default/LFE_00255_.png"
  cover_alt: "Vigdís — LFE, retro-futurist digital painting"
  math: false
---
<a href="/blog/assets/images/posts/lfe-tooling-leonardo-gears-2.png"><img class="right small" src="/blog/assets/images/posts/lfe-tooling-leonardo-gears-2.png" /></a>I have been working on an LL(1) grammar parser generator for Erlang/LFE. While we have yecc for LALR(1) grammars this isn't suitable for everything. I think there are 2 main problems:

- The generated yecc parsers must be given the exact number of tokens, neither too many nor too few. While this is no problem with Erlang code because of the ``.`` it is difficult with LFE. This could be fixed by writing a new yecc include file.

- You tend to need some *extra* end token, usually written as ``$``, after the ones needed for parsing to drive the actual parsing. This would make giving too many tokens problematic.

You don't have these problems with an LL(1) parser. The current LFE has been handwritten following the same rules as would a parser generator. So now I have taken the final step to finish a generator I started long-ago.

It works but needs a bit of cleaning up to be generally useful. It can now handle the full LFE syntax, which isn't complex, and is almost a useful tool. While it is written in Erlang it will be easy to fix it so that it uses either Erlang or LFE for both the grammar file and the output file.

The hardest part has been coming up with a good name, suggestions welcome.

Robert
