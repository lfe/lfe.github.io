---
layout: post.liquid
title: "LFE Friday - calendar:is_leap_year/1"
description: ""
permalink: "/blog/tutorials/2015/05/02/1744-lfe-friday---calendaris_leap_year1"
categories: ["tutorials"]
tags: ["lfe friday", "lfe", "erlang"]
published_date: 2015-05-02 17:44:00 +0000
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

Today's LFE Friday is on [calendar:is_leap_year/1](http://www.erlang.org/doc/man/calendar.html#is_leap_year-1).

``calendar:is_leap_year/1`` takes a non-negative integer value representing a year, and will return ``true`` if that year is a leap year, or ``false`` otherwise.

```lfe
> (calendar:is_leap_year 2015)
false
> (calendar:is_leap_year 2012)
true
> (calendar:is_leap_year 2017)
false
> (calendar:is_leap_year 2000)
true
> (calendar:is_leap_year 1900)
false
> (calendar:is_leap_year 0)   
true
```

By having a built in function as part of the core Erlang libraries, it means you don't have to code up the rules, or even go lookup the rules to remember how the century years are determined to be leap years or not.

And if you do pass in a negative number for the year, Erlang will raise an exception, as there are no clauses which match a negative number for the year.

```lfe
> (calendar:is_leap_year -1)
exception error: function_clause
  in (: calendar is_leap_year -1)
> (calendar:is_leap_year -4)
exception error: function_clause
  in (: calendar is_leap_year -4)
```

-Proctor, Robert
