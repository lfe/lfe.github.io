---
layout: post.liquid
title: "LFE Friday - ordsets:is_disjoint/2"
description: ""
permalink: "/blog/tutorials/2015/03/22/2108-lfe-friday---ordsetsis_disjoint2"
categories: ["tutorials"]
tags: ["lfe friday", "lfe", "erlang"]
published_date: 2015-03-22 21:08:00 +0000
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

Today's LFE Friday covers [ordsets:is_disjoint/2](http://www.erlang.org/doc/man/ordsets.html#is_disjoint-2).

There are times in your in your coding day where you have problems where you need to know if given a list of items that none of those items appear in a secondary list.

Your first intuition might be to write out the code as described, like such:

```lfe
> (not (lists:any (lambda (item) (lists:member item '(1 3 5 7))) '(2 4 6)))
true
```

And while that is accurate, if you redefine your problem in more mathematical terms, you can start to think in sets.  When you start to think in terms of sets, you realize that you can check to see if the intersection of the two sets is the empty set.

```lfe
> (=:= (ordsets:intersection '(1 3 5 7) '(2 4 6)) ())
true
```

This is becoming not only more concise, but also more explicit about what you are trying to check.

We can do better still, by checking if the lists are disjoint sets.  Enter ``ordsets:is_disjoint/2``.

``ordsets:is_disjoint/2`` takes two lists, and returns ``true`` if no elements are in common.

```lfe
> (ordsets:is_disjoint '(1 3 5 7) '(2 4 6))
true
> (ordsets:is_disjoint '(1 2 3 5 7) '(2 4 6))
false
```

Because ``ordsets:is_disjoint/2`` operates against two lists, we do not have to make sure the elements are unique prior to calling ``ordsets:is_disjoint/2``.

```lfe
> (ordsets:is_disjoint '(1 1 3 5 7 5 3) '(2 4 2 2 6))
true
> (ordsets:is_disjoint '(1 2 3 5 7) '(2 4 2 2 6))
false
```

And if either list passed to ``ordsets:is_disjoint/2`` is an empty list, the result is that the lists are disjoint.

```lfe
> (ordsets:is_disjoint '(1 2 3 5 7) '())         
true
> (ordsets:is_disjoint '() '(2 4 6))                 
true
> (ordsets:is_disjoint '() '())     
true
```

And if you are curious, by running ``ordsets:is_disjoint/2`` through ``timer:tc/3``, we can see that as soon as Erlang knows that the sets are not disjoint, it returns ``false``.  And if you remember from the previous [LFE Friday on timer:tc/3](http://blog.lfe.io/blog/tutorials/2015/01/10/2201-lfe-friday---timertc3/), the return value is a tuple with the first element being the number of *micro*seconds it took to complete.

```lfe
> (timer:tc 'ordsets 'is_disjoint (list (lists:seq 1 1000000) (lists:seq 2000000 3000000)))         
#(13627 true)
> (timer:tc 'ordsets 'is_disjoint (list (lists:seq 1 1000000) (lists:seq 1 3000000)))      
#(1 false)
```

-Proctor, Robert
