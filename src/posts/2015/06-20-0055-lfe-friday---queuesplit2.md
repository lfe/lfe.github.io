---
layout: post.liquid
title: "LFE Friday - queue:split/2"
description: ""
permalink: "/blog/tutorials/2015/06/20/0055-lfe-friday---queuesplit2"
categories: ["tutorials"]
tags: ["lfe-friday", "lfe", "erlang"]
published_date: 2015-06-20 00:55:00 +0000
is_draft: false
data:
  author: robert-virding
  written_for:
    lfe: "0.9"
    erlang: "17"
  last_validated: null
  cover_image: "/images/fridays/LFE_Friday_00276_.png"
  cover_alt: "Vigdís — LFE Friday, LiffyBot studying Erlang modules in a Mead-style command room"
  math: false
---
<a href="/blog/assets/images/posts/LispFlavoredErlang-medium-square.png"><img class="left tiny" src="/blog/assets/images/posts/LispFlavoredErlang-medium-square.png" /></a>This week's LFE Friday was translated with permission from the
[Erlang Thursday](http://www.proctor-it.com/category/erlang/erlang-thursday/)
series by [Steven Proctor](https://twitter.com/stevenproctor).
*This week's translator*: Robert Virding.

Today's LFE Friday from a Sweden celebrating midsummer is on [queue:split/2](http://erlang.org/doc/man/queue.html#split-2) from the ``queue`` modules Original API.

``queue:split/2`` takes two arguments. The first argument being a integer n from 0 to size, where size is number of items in the queue, and the second argument is the queue that we wish to split.  The return value is a two-tuple with the first element being a queue of the first n items, and the second element of the tuple is a queue of the rest of the items.

```lfe
> (set queue-one (queue:from_list '(a 1 b 2 c 3 4))) 
#((4 3 c) (a 1 b 2))
> (queue:split 4 queue-one)
#(#((2) (a 1 b)) #((4 3) (c)))
> (queue:split 0 queue-one)
#(#(() ()) #((4 3 c) (a 1 b 2)))
> (queue:split 1 queue-one)
#(#(() (a)) #((4 3 c) (1 b 2)))
> (queue:split 7 queue-one)
#(#((4 3 c) (a 1 b 2)) #(() ()))
> (queue:split 15 queue-one)
exception error: badarg
  in (: queue split 15 #((4 3 c) (a 1 b 2)))

> (set (tuple split-first split-second) (queue:split 3 queue-one))
#(#((b 1) (a)) #((4 3 c) (2)))
> split-first
#((b 1) (a))
> split-second
#((4 3 c) (2))
> (queue:peek split-first)
#(value a)
> (queue:peek split-second)
#(value 2)
```

Erlang also provides a [queue:join/2](http://erlang.org/doc/man/queue.html#join-2) function that takes two queues, and returns a new queue, with the queue that was passed as the second argument appended to the queue passed in as the first argument.

```lfe
> (queue:join split-first split-second)
#((4 3 c) (a 1 b 2))
> (queue:join split-second split-first)
#((b 1) (2 c 3 4 a))
> (queue:join (queue:new) split-first) 
#((b 1) (a))
> (queue:join (queue:new) (queue:new)) 
#(() ())
```

-Proctor, Robert
