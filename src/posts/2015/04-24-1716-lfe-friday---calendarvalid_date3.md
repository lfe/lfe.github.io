---
layout: post.liquid
title: "LFE Friday - calendar:valid_date/3"
description: ""
permalink: "/blog/tutorials/2015/04/24/1716-lfe-friday---calendarvalid_date3"
categories: ["tutorials"]
tags: ["lfe-friday", "lfe", "erlang"]
published_date: 2015-04-24 17:16:00 +0000
is_draft: false
data:
  author: robert-virding
  written_for: null
  last_validated: null
  cover_image: "/images/fridays/LFE_Friday_00277_.png"
  cover_alt: "Vigdís — LFE Friday, LiffyBot studying Erlang modules in a Mead-style command room"
  math: false
---
<a href="/blog/assets/images/posts/LispFlavoredErlang-medium-square.png"><img class="left tiny" src="/blog/assets/images/posts/LispFlavoredErlang-medium-square.png" /></a>This week's LFE Friday was translated with permission from the
[Erlang Thursday](http://www.proctor-it.com/category/erlang/erlang-thursday/)
series by [Steven Proctor](https://twitter.com/stevenproctor).
*This week's translator*: Robert Virding.

Today's LFE Friday is [calendar:valid_date/3](http://www.erlang.org/doc/man/calendar.html#valid_date-3).

Originally, I was thinking it was going to be ``calendar:time_difference/3``, but then I looked into the Erlang documentation for the [calendar module](http://www.erlang.org/doc/man/calendar.html) and saw that it was marked as obsolete, so today I present ``calendar:valid_date/3``.

The arguments to ``calendar:valid_date/3`` are an integer for the year, integer for the month, and an integer for the day.  ``calendar:valid_date/3`` returns the atom ``true`` if the day passed in is a valid date, and the atom ``false`` if it is not a valid date.

```lfe
> (calendar:valid_date 2015 04 31)
false
> (calendar:valid_date 2015 04 30)
true
> (calendar:valid_date 2015 02 29)
false
> (calendar:valid_date 2012 02 29)
true
> (calendar:valid_date 2015 11 31)
false
> (calendar:valid_date 2015 11 76)
false
> (calendar:valid_date 2015 17 13)
false
```

Just a quick check for our sanity that the day this post was published is a valid date as well.

```lfe
> (calendar:valid_date 2015 04 24)
true
```

Now let's try to break this a bit and test to see how it can handle `0`'s and negative integer values.

```lfe
> (calendar:valid_date -1 04 24)  
false
> (calendar:valid_date 2015 -7 21)
false
> (calendar:valid_date 2015 7 -13)
false
> (calendar:valid_date 0 0 0)     
false
```

As one might hope, unless you deal with B.C. era dates often, a date with a negative value is not a valid date.

Erlang also provides a ``calendar:valid_date/1`` that takes a tuple of the year, month, and day values as well.

```lfe
> (calendar:valid_date #(2015 11 76))
false
> (calendar:valid_date #(2015 04 24))
true
```

-Proctor, Robert
