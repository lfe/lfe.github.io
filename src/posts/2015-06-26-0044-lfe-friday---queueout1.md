---
layout: post.liquid
title: "LFE Friday - queue:out/1"
description: ""
permalink: "/blog/tutorials/2015/06/26/0044-lfe-friday---queueout1"
categories: ["tutorials"]
tags: ["lfe friday", "lfe", "erlang"]
published_date: 2015-06-26 00:44:00 +0000
is_draft: false
data:
  author: robert-virding
  written_for: null
  last_validated: null
  cover_image: "/images/fridays/LFE_Friday_00286_.png"
  cover_alt: "Vigdís — LFE Friday, LiffyBot studying Erlang modules in a Mead-style command room"
  math: false
---
<a href="/blog/assets/images/posts/LispFlavoredErlang-medium-square.png"><img class="left tiny" src="/blog/assets/images/posts/LispFlavoredErlang-medium-square.png" /></a>This week's LFE Friday was translated with permission from the
[Erlang Thursday](http://www.proctor-it.com/category/erlang/erlang-thursday/)
series by [Steven Proctor](https://twitter.com/stevenproctor).
*This week's translator*: Robert Virding.

Today's LFE Friday covers [queue:out/1](http://erlang.org/doc/man/queue.html#out-1) from the ``queue`` module's Original API.

``queue:out/1`` is one of my all time Queue functions, or methods, that I have seen, and that is across all the languages and libraries that I have encountered.

"What makes it so great?", I can hear you asking.

That would be it's combination of tuples, tagged tuples, immutability, forgivingness, and the fact that after seeing the result, it makes me wish more Queue implementations had an API like this.

First there have been many times in my past where either myself, or someone else, has forgotten to do a check to see if a queue is empty before trying to pop the first item from it, and that mistake has resulted in a not-so-nice runtime error.

``queue:out/1`` on the other hand, doesn't trigger an error when you try to call it on an empty queue.  Rather it returns a tagged tuple telling you that the queue you tried to call ``out`` on was empty, and the empty queue.

```lfe
> (queue:out (queue:new))
#(empty #(() ()))
```

If we do pass in a non-empty queue, ``queue:out/1`` returns a two tuple, with the first element being a tagged tuple that tells us we got a value out and the HEAD of the original queue, and for the second element, we get a new queue with the result of removing the first item.

```lfe
> (set queue (queue:from_list '(a b c d)))
#((d) (a b c))
> (queue:out queue)
#(#(value a) #((d) (b c)))
> queue
#((d) (a b c))
> (set (tuple (tuple 'value head) new-queue) (queue:out queue))
#(#(value a) #((d) (b c)))
> queue
#((d) (a b c))
> head
a
> new-queue
#((d) (b c))
> (queue:head new-queue)
b
```

The fourth query is a good case for using the backquote macro.

```lfe
> (set `#(#(value ,head) ,new-queue) (queue:out queue))
#(#(value a) #((d) (b c)))
```

When dealing with the abstract notion of a queue across any language, the concept of a "pop" does two things, returns the top item of the queue, and modifies the queue to have that item removed.

Since Erlang/LFE queues are immutable, after you think about it for a few minutes, it starts to make sense that ``queue:out/1`` handles both those behaviors of "pop" by returning both the item removed from the queue, and the new state of the queue with that item removed.

Erlang's ``queue`` module also provides a function [queue:out_r/1](http://erlang.org/doc/man/queue.html#out_r-1) which behaves the same as ``queue:out/1`` except it operates off the last item in the queue, instead of the first item.

```lfe
> (queue:out_r (queue:from_list '(a b c d)))
#(#(value d) #((c) (a b)))
> (queue:out_r (queue:new))                                    
#(empty #(() ()))
```

I hope you found ``queue:out/1`` as handy and as nice I have,

-Proctor, Robert
