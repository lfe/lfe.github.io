---
layout: post.liquid
title: "LFE Friday - timer:tc/3"
description: ""
permalink: "/blog/tutorials/2015/01/10/2201-lfe-friday---timertc3"
categories: ["tutorials"]
tags: ["lfe-friday", "lfe", "erlang"]
published_date: 2015-01-10 22:01:00 +0000
is_draft: false
data:
  author: robert-virding
  written_for: null
  last_validated: null
  cover_image: "/images/fridays/LFE_Friday_00282_.png"
  cover_alt: "Vigdís — LFE Friday, LiffyBot studying Erlang modules in a Mead-style command room"
  math: false
---
<a href="/blog/assets/images/posts/LispFlavoredErlang-medium-square.png"><img class="left tiny" src="/blog/assets/images/posts/LispFlavoredErlang-medium-square.png" /></a>This week's LFE Friday was translated with permission from the
[Erlang Thursday](http://www.proctor-it.com/category/erlang/erlang-thursday/)
series by [Steven Proctor](https://twitter.com/stevenproctor).
*This week's translator*: Robert Virding.

Today’s LFE Friday is on [timer:tc/3](http://www.erlang.org/doc/man/timer.html#tc-3).

I am sure we have all written some timing code where we capture the current time, do something, capture the current time again and then find the difference to find out how long something took to execute. In Erlang, that generally looks something like the following:


```lfe
> (set time1 (now))
#(1420 910649 803027)
> (timer:sleep 4000)    ;Do something
ok
> (set time2 (now))
#(1420 910653 804244)
> (timer:now_diff time2 time1)                                             
4001217
```

Note that we have to use [timer:now_diff/2](http://www.erlang.org/doc/man/timer.html#now_diff-2), since the ``now/0`` function returns the timestamp as a tuple, and we can’t just do normal subtraction on that tuple like we might be able to in other languages.

Of course as good "engineers", we know that since we need to do timings in various places of the app we can just create our own function to do that, and have that live in just one place.

The downside is: the wise people on the Erlang language team have done that for us already and provided it in the form of ``timer:tc/3``.

``timer:tc/3`` takes the module name, function name, and a list of the arguments to be passed to the function. And since we usually want the result of the function we are calling, in addition to the timing, the return value is a tuple of the time in microseconds, and the result of applying the function passed to ``timer:tc/3``.

```lfe
> (timer:tc 'timer 'sleep '(4000))
#(4000480 ok)
> (timer:tc 'lists 'foldl (list (lambda (x accum) (+ x accum)) 0 (lists:seq 1 2000000)))
#(3533603 2000001000000)
```

There is also ``timer:tc/1`` which takes just a function and applies it, and ``timer:tc/2`` which takes a function and applies it with the given arguments.

```lfe
> (timer:tc (lambda () (lists:foldl (lambda (x accum) (+ x accum)) 0 (lists:seq 1 2000000))))       
#(3693260 2000001000000)
> (timer:tc #'lists:foldl/3 (list (lambda (x accum) (+ x accum)) 0 (lists:seq 1 2000000)))
#(3529578 2000001000000)
```

–Proctor, Robert
