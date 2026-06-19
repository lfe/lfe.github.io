---
layout: post.liquid
title: "LFE Friday - queue:peek/1"
description: ""
permalink: "/blog/tutorials/2015/06/14/2228-lfe-friday---queuepeek1"
categories: ["tutorials"]
tags: ["lfe-friday", "lfe", "erlang"]
published_date: 2015-06-14 22:28:00 +0000
is_draft: false
data:
  author: robert-virding
  written_for: null
  last_validated: null
  cover_image: "/images/fridays/LFE_Friday_00274_.png"
  cover_alt: "Vigdís — LFE Friday, LiffyBot studying Erlang modules in a Mead-style command room"
  math: false
---
<a href="/blog/assets/images/posts/LispFlavoredErlang-medium-square.png"><img class="left tiny" src="/blog/assets/images/posts/LispFlavoredErlang-medium-square.png" /></a>This week's LFE Friday was translated with permission from the
[Erlang Thursday](http://www.proctor-it.com/category/erlang/erlang-thursday/)
series by [Steven Proctor](https://twitter.com/stevenproctor).
*This week's translator*: Robert Virding.

For today's LFE Friday we continue looking at the ``queue`` module and look at [queue:peek/1](http://erlang.org/doc/man/queue.html#peek-1) from the Extended API.

``queue:peek/1`` takes a queue as it's argument and returns either the atom ``empty`` if the queue is empty, or ``#(value item)`` where ``item`` is the item at the head of the queue.

```lfe
> (set queue-one (queue:from_list '(1 2 3 4 5)))
#((5 4) (1 2 3))
> (queue:peek queue-one)
#(value 1)
> (set empty-queue (queue:new))
#(() ())
> (queue:peek empty-queue)     
empty
```

``queue:peek/1`` does not modify the existing queue at all either, so we can call it once as seen above, or multiple times as below, and the queue we peeked at doesn't change.

```lfe
> (set queue-two (queue:from_list '(a b c d e f)))
#((f e) (a b c d))
> (queue:peek queue-two)
#(value a)
> (queue:peek queue-two)
#(value a)
> (queue:peek queue-two)
#(value a)
> queue-two
#((f e) (a b c d))
```

And unlike we saw in the previous [LFE Friday on queue:head/1](https://lfe.io/blog/tutorials/2015/05/29/0345-lfe-friday---queuehead1/), we can safely peek at an empty queue instead of getting an exception.

```lfe
> (queue:head empty-queue)
exception error: empty
  in (: queue head #(() ()))

> (queue:peek empty-queue)
empty
```

Erlang's ``queue`` module also contains [queue:peek_r/1](http://erlang.org/doc/man/queue.html#peek_r-1) which will peek at the element at the rear of the queue.

```lfe
> (queue:peek_r empty-queue)                      
empty
> (queue:peek_r queue-one)      
#(value 5)
> (queue:peek_r queue-one)
#(value 5)
> (queue:peek_r queue-one)
#(value 5)
> (queue:peek_r queue-two)
#(value f)
> queue-two               
#((f e) (a b c d))
> queue-one
#((5 4) (1 2 3))
> empty-queue
#(() ())
```

-Proctor, Robert
