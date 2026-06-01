---
layout: post.liquid
title: "LFE and dialyzer"
description: "How we will be able to run dialyzer on LFE code"
permalink: "/blog/announcements/2015/04/04/1538-lfe-and-dialyzer"
categories: ["announcements"]
tags: ["lfe", "dialyzer"]
published_date: 2015-04-04 15:38:00 +0000
is_draft: false
data:
  author: robert-virding
  written_for: null
  last_validated: null
  cover_image: null
  cover_alt: null
  math: false
---
<a href="/blog/assets/images/posts/ldialyzer.png"><img class="left medium" src="/blog/assets/images/posts/ldialyzer.png" /></a>
Dialyzer can be a useful tool but its implementation has a few idiosyncrasies which make it difficult to directly use with LFE. It can only use .erl files or .beam files. Unfortunately the .beam files must be compiled from erlang files using the 'debug_info' option. This option includes the full Erlang AST in the .beam file and it is this which is used by dialyzer.[^1]

Unfortunately the LFE compiler does not generate an Erlang AST, it instead generates something called Core erlang which is used internally in the compiler. This is a much nicer language to compile to. Funnily enough dialyzer actually uses Core erlang internally, it just doesn't provide any direct way of inputting it.

I have been working on fixing this using some module loading trickery so that special versions of dialyzer files are used which can handle LFE and Core erlang. This does work but it needs a bit more work before it is released to an "unsuspecting public".

There will be a special dialyzer startup program, ldialyzer, which does the right thing.

Robert

[^1]: For this reason releasing a product with only .beam files but which have been compiled with the debug_info option is the same as including the source files.
