---
layout: post.liquid
title: "LFE Friday - ordsets:subtract/2"
description: ""
permalink: "/blog/tutorials/2015/07/17/0213-lfe-friday---ordsetssubtract2"
categories: ["tutorials"]
tags: ["lfe friday", "lfe", "erlang"]
published_date: 2015-07-17 02:13:00 +0000
is_draft: false
data:
  author: robert-virding
  written_for: null
  last_validated: null
  cover_image: null
  cover_alt: null
  math: false
---
<a href="/blog/assets/images/posts/LispFlavoredErlang-medium-square.png"><img class="left tiny" src="/blog/assets/images/posts/LispFlavoredErlang-medium-square.png" /></a>This week's LFE Friday was translated with permission from the
[Erlang Thursday](http://www.proctor-it.com/category/erlang/erlang-thursday/)
series by [Steven Proctor](https://twitter.com/stevenproctor).
*This week's translator*: Robert Virding.

Today's LFE Friday comes from a sunny Gotland and is on [ordsets:subtract/2](http://erlang.org/doc/man/ordsets.html#subtract-2).

``ordsets:subtract/2`` takes two ordered sets as its arguments, and returns a ordered set containing the items of the first ordered set that are not in the second ordered set.

```lfe
> (set set-1 (ordsets:from_list '(5 4 3 2 1)))
(1 2 3 4 5)
> (set set-2 (ordsets:from_list '(1 1 2 3 5 8 13)))
(1 2 3 5 8 13)
> (set set-3 (ordsets:from_list '(2 -2 4 -4 16 -16)))
(-16 -4 -2 2 4 16)
> (set empty-set (ordsets:new))
()
> (ordsets:subtract set-1 set-2)
(4)
> (ordsets:subtract set-1 empty-set)
(1 2 3 4 5)
> (ordsets:subtract set-2 empty-set)
(1 2 3 5 8 13)
> (ordsets:subtract empty-set set-1)
()
> (ordsets:subtract set-2 set-3)
(1 3 5 8 13)
```

And note that ``ordsets:subtract/2`` is not commutative, unlike ``ordsets:union/2`` or ``ordsets:intersection/2``.

```lfe
> (ordsets:subtract set-1 set-3)
(1 3 5)
> (ordsets:subtract set-3 set-1)
(-16 -4 -2 16)
```

And again, your friendly reminder if you haven't been following along, just because Ordered Sets in LFE are represented as a List, doesn't mean that Lists are Ordered Sets.

-Proctor, Robert
