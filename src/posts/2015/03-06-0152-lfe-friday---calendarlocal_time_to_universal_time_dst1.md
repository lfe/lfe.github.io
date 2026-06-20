---
layout: post.liquid
title: "LFE Friday - calendar:local_time_to_universal_time_dst/1"
description: ""
permalink: "/blog/tutorials/2015/03/06/0152-lfe-friday---calendarlocal_time_to_universal_time_dst1"
categories: ["tutorials"]
tags: ["lfe-friday", "lfe", "erlang"]
published_date: 2015-03-06 01:52:00 +0000
is_draft: false
data:
  author: robert-virding
  written_for:
    lfe: "0.9"
    erlang: "17"
  last_validated: null
  cover_image: "/images/fridays/LFE_Friday_00279_.png"
  cover_alt: "Vigdís — LFE Friday, LiffyBot studying Erlang modules in a Mead-style command room"
  math: false
---
<a href="/blog/assets/images/posts/LispFlavoredErlang-medium-square.png"><img class="left tiny" src="/blog/assets/images/posts/LispFlavoredErlang-medium-square.png" /></a>This week's LFE Friday was translated with permission from the
[Erlang Thursday](http://www.proctor-it.com/category/erlang/erlang-thursday/)
series by [Steven Proctor](https://twitter.com/stevenproctor).
*This week's translator*: Robert Virding.

In honor of the time change this weekend, today's LFE Friday is on [calendar:local_time_to_universal_time_dst/1](http://www.erlang.org/doc/man/calendar.html#local_time_to_universal_time_dst-1)

We here in the EU will have to wait 3 more weeks for our time change, but we will do this anyway. :-)

To better understand what ``calendar:local_time_to_universal_time_dst/1`` is doing, it will be contrasted to [calendar:local_time_to_universal_time/1](http://www.erlang.org/doc/man/calendar.html#local_time_to_universal_time-1).

This coming Sunday is we move the clock forward an hour at 2 AM, so let us see what the time is in UTC right at 1:59 AM.

```lfe
> (calendar:local_time_to_universal_time #(#(2015 3 8) #(1 59 59)))
#(#(2015 3 8) #(7 59 59))
```

Now let's see what ``calendar:local_time_to_universal_time/1`` returns for 2 AM, which we never actually hit, since the time changes straight to 3 AM.

```lfe
> (calendar:local_time_to_universal_time #(#(2015 3 8) #(2 0 0)))  
#(#(2015 3 8) #(8 0 0))
```

But as we dig into the library we see that there is also ``calendar:local_time_to_universal_time_dst/1`` which returns a list of time tuples, and we start to see why this is important.

This time, let's call ``calendar:local_time_to_universal_time_dst/1`` with the 2 AM hour and see what is returned.

```lfe
> (calendar:local_time_to_universal_time_dst #(#(2015 3 8) #(2 0 0)))
()
```

An empty list!

This makes sense if you take a moment to think about it, as 2 AM doesn't ever happen so there is no UTC time that it would map to.

And if we look at what happens when we make the jump to 3 AM, we get 8 AM UTC, which is just a second later in UTC, then the time in UTC at 1:59:59 AM.

```lfe
> (calendar:local_time_to_universal_time_dst #(#(2015 3 8) #(3 0 0)))
(#(#(2015 3 8) #(8 0 0)))
```

That covers the case for this coming weekend, so let is check out what is going to happen later this year on November 1st when the clocks move back an hour.

To start with a baseline, let's see what time it is in UTC at 12:59:59 AM, as we know we only encounter that time once.

```lfe
> (calendar:local_time_to_universal_time #(#(2015 11 1) #(0 59 59)))    
#(#(2015 11 1) #(5 59 59))
> (calendar:local_time_to_universal_time_dst #(#(2015 11 1) #(0 59 59)))
(#(#(2015 11 1) #(5 59 59)))
```

And even though the time change happens a 2 AM, we still only encounter that once, as we move straight to 1 AM the first time.

```lfe
> (calendar:local_time_to_universal_time_dst #(#(2015 11 1) #(2 0 0)))
(#(#(2015 11 1) #(8 0 0)))
```

So at 12:59:59 AM, we are at 5:59:59 AM UTC, and at 2 AM we are at 8 AM UTC. Now comes the tricky part, 1 AM.

1 AM is an odd case, as we will live through 1 AM twice that night. So let's see what Erlang does with that.

First, we see what time it is in UTC when we call calendar:local_time_to_universal_time with 1 AM.

```lfe
> (calendar:local_time_to_universal_time #(#(2015 11 1) #(1 0 0)))    
#(#(2015 11 1) #(6 0 0))
```

We get 6 AM UTC. We move from 5:59:59 AM UTC to 6 AM UTC; and that makes sense, until we start to wonder about the second time we encounter 1 AM.

Now, let's see what time it is in UTC when we call ``calendar:local_time_to_universal_time_dst/1`` with 1 AM.

```lfe
> (calendar:local_time_to_universal_time_dst #(#(2015 11 1) #(1 0 0)))    
(#(#(2015 11 1) #(6 0 0)) #(#(2015 11 1) #(7 0 0)))
```

We get a list with two times in UTC, one for 6 AM UTC when we encounter 1 AM the first time, and one for 7 AM UTC for when we encounter 1 AM the second time around. This happens for every time in the 1 AM hour as well.

```lfe
> (calendar:local_time_to_universal_time_dst #(#(2015 11 1) #(1 59 59)))    
(#(#(2015 11 1) #(6 59 59)) #(#(2015 11 1) #(7 59 59)))
```

By returning a list of either no items, one item, or two items, Erlang gives you the most accurate information for you to use, and allow for you to make the informed decision for how you are expecting to handle the times, instead of making some of the decisions for you even if they don't align with your system's view of the world.

–Proctor, Robert
