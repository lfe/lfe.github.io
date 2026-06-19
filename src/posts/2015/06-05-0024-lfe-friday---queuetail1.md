---
layout: post.liquid
title: "LFE Friday - queue:tail/1"
description: ""
permalink: "/blog/tutorials/2015/06/05/0024-lfe-friday---queuetail1"
categories: ["tutorials"]
tags: ["lfe-friday", "lfe", "erlang"]
published_date: 2015-06-05 00:24:00 +0000
is_draft: false
data:
  author: robert-virding
  written_for: null
  last_validated: null
  cover_image: "/images/fridays/LFE_Friday_00278_.png"
  cover_alt: "Vigdís — LFE Friday, LiffyBot studying Erlang modules in a Mead-style command room"
  math: false
---
<a href="/blog/assets/images/posts/LispFlavoredErlang-medium-square.png"><img class="left tiny" src="/blog/assets/images/posts/LispFlavoredErlang-medium-square.png" /></a>This week's LFE Friday was translated with permission from the
[Erlang Thursday](http://www.proctor-it.com/category/erlang/erlang-thursday/)
series by [Steven Proctor](https://twitter.com/stevenproctor).
*This week's translator*: Robert Virding.

In today's LFE Friday we continue with the ``queue`` module's Okasaki API, and look at [queue:tail/1](http://erlang.org/doc/man/queue.html#tail-1).

``queue:tail/1`` takes a non-empty queue as its argument, and returns a new queue with the first element removed.

```lfe
> (set queue (queue:from_list '(1 2 3 4 5)))
#((5 4) (1 2 3))
> (set tail (queue:tail queue))             
#((5 4) (2 3))
> queue
#((5 4) (1 2 3))
> (queue:head tail)
2
> (queue:to_list tail)
(2 3 4 5)
```

We can see above that calling ``queue:tail/1`` is not a destructive operation as might happen in other languages, and does indeed leave the original queue intact.

As part of the Okasaki API, which treats a queue as a double ended, ``queue:tail/1`` has a counterpart function [queue:liat/1](http://erlang.org/doc/man/queue.html#liat-1) which will return a new queue with last item removed.  ``queue:liat/1`` also has an alias in the Okasaki API of [queue:init/1](http://erlang.org/doc/man/queue.html#init-1).

```lfe
> (queue:liat queue)
#((4) (1 2 3))
> (queue:init queue)
#((4) (1 2 3))
> queue
#((5 4) (1 2 3))
```

Note that the Erlang documentation also shows that there is an alias ``queue:lait/1`` which it points out ***should not*** be used because it is a misspelling.

And because we want to try to break things and see what we can learn, let's try to call the different tail functions we have covered so far with an empty queue to see what happens.

```lfe
> (set empty-queue (queue:new))
#(() ())
> (queue:tail empty-queue)
exception error: empty
  in (: queue drop #(() ()))

> (queue:liat empty-queue)
exception error: empty
  in (: queue drop_r #(() ()))

> (queue:init empty-queue)
exception error: empty
  in (: queue drop_r #(() ()))

```

Looks like we get exception errors in [queue:drop/1](http://erlang.org/doc/man/queue.html#drop-1) and [queue:drop_r/1](http://erlang.org/doc/man/queue.html#drop_r-1) when we call ``queue:tail/1`` and ``queue:liat/1`` respectively.

And when we look at the behavior of ``queue:drop/1`` and ``queue:drop_r/1`` with a queue with items in it, it looks like ``queue:tail/1`` is just an alias for ``queue:drop/1``, and ``queue:liat/1`` and ``queue:init/1`` are just aliases for ``queue:drop_r/1``.

```lfe
> (queue:drop queue)
#((5 4) (2 3))
> (queue:drop_r queue)
#((4) (1 2 3))
```

--Proctor, Robert
