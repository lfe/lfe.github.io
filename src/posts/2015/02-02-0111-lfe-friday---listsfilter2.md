---
layout: post.liquid
title: "LFE Friday - lists:filter/2"
description: ""
permalink: "/blog/tutorials/2015/02/02/0111-lfe-friday---listsfilter2"
categories: ["tutorials"]
tags: ["lfe-friday", "lfe", "erlang"]
published_date: 2015-02-02 01:11:00 +0000
is_draft: false
data:
  author: robert-virding
  written_for:
    lfe: "0.9"
    erlang: "17"
  last_validated: null
  cover_image: "/images/fridays/LFE_Friday_00278_.png"
  cover_alt: "Vigdís — LFE Friday, LiffyBot studying Erlang modules in a Mead-style command room"
  math: false
---
<a href="/blog/assets/images/posts/LispFlavoredErlang-medium-square.png"><img class="left tiny" src="/blog/assets/images/posts/LispFlavoredErlang-medium-square.png" /></a>This week's LFE Friday was translated with permission from the
[Erlang Thursday](http://www.proctor-it.com/category/erlang/erlang-thursday/)
series by [Steven Proctor](https://twitter.com/stevenproctor).
*This week's translator*: Robert Virding.

Today’s LFE Friday is on [lists:filter/2](http://www.erlang.org/doc/man/lists.html#filter-2).

``lists:filter/2`` takes two arguments, a predicate function and a list to iterate over. The return value is a list of items for which the predicate function returns ``true`` for that item.

```lfe
> (lists:filter (lambda (x) (=:= (rem x 2) 1)) '(1 2 3 4 5))
(1 3 5)
> (lists:filter #'erlang:is_atom/1 '(1 a 3 #(a b) World foo))
(a World foo)
> (lists:filter (lambda (x) (> x 0)) '(1 0 -3 foo -13 43))
(1 foo 43)
> (lists:filter (lambda (x) (> x 0)) ())
()
> (lists:filter (lambda (x) 'false) '(1 2 3 4 5))
()
> (lists:filter (lambda (x) 'true) '(1 2 3 4 5)) 
(1 2 3 4 5)
```

–Proctor, Robert
