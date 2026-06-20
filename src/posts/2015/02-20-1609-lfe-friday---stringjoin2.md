---
layout: post.liquid
title: "LFE Friday - string:join/2"
description: ""
permalink: "/blog/tutorials/2015/02/20/1609-lfe-friday---stringjoin2"
categories: ["tutorials"]
tags: ["lfe-friday", "lfe", "erlang"]
published_date: 2015-02-20 16:09:00 +0000
is_draft: false
data:
  author: robert-virding
  written_for:
    lfe: "0.9"
    erlang: "17"
  last_validated: null
  cover_image: "/images/fridays/LFE_Friday_00286_.png"
  cover_alt: "Vigdís — LFE Friday, LiffyBot studying Erlang modules in a Mead-style command room"
  math: false
---
<a href="/blog/assets/images/posts/LispFlavoredErlang-medium-square.png"><img class="left tiny" src="/blog/assets/images/posts/LispFlavoredErlang-medium-square.png" /></a>This week's LFE Friday was translated with permission from the
[Erlang Thursday](http://www.proctor-it.com/category/erlang/erlang-thursday/)
series by [Steven Proctor](https://twitter.com/stevenproctor).
*This week's translator*: Robert Virding.

Today's LFE Friday is on [string:join/2](http://www.erlang.org/doc/man/string.html#join-2).

``string:join/2`` takes a list of strings as its first argument, and a string separator used to join the strings together into a single string.

```lfe
> (string:join '("a" "b" "c") "")
"abc"
> (string:join '("a" "b" "c") "-")
"a-b-c"
```

The separator string can be a string of any length, and doesn't just have to be a single character.

```lfe
> (string:join '("a" "b" "c") "___")
"a___b___c"
> (string:join '("a" "b" "c") " ")  
"a b c"
```

And as with any string, a list of characters, or even integers, can be used as the separator string.

```lfe
> (string:join '("a" "b" "c") '(#\A))
"aAbAc"
> (string:join '("a" "b" "c") '(52)) 
"a4b4c"
```

-Proctor, Robert
