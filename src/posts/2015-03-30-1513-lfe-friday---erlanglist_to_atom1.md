---
layout: post.liquid
title: "LFE Friday - erlang:list_to_atom/1"
description: ""
permalink: "/blog/tutorials/2015/03/30/1513-lfe-friday---erlanglist_to_atom1"
categories: ["tutorials"]
tags: ["lfe friday", "lfe", "erlang"]
published_date: 2015-03-30 15:13:00 +0000
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

Today’s LFE Friday covers [erlang:list_to_atom/1](http://www.erlang.org/doc/man/erlang.html#list_to_atom-1).

``erlang:list_to_atom/1`` takes a string, and returns an Erlang atom.

```lfe
> (list_to_atom "foo")
foo
> (list_to_atom "Foo")
Foo
> (list_to_atom "foo-bar")
foo-bar
> (list_to_atom "foo bar")
|foo bar|
> (list_to_atom (++ "foo" "-" "bar"))
foo-bar
> (list_to_atom "Erlang")
Erlang
> (list_to_atom "the LFE way")   
|the LFE way|
> (list_to_atom "Erlang and Elixir")
|Erlang and Elixir|
```

This can be useful if you are having to create keys or identifiers based off strings read in from outside the system, such as parsing a CSV style header.

```lfe
> (lists:map #'erlang:list_to_atom/1
             (string:tokens "firstName,lastName,age,gender,preferredName,dateOfBirth" ","))
(firstName lastName age gender preferredName dateOfBirth)
```

You do need to be careful when using ``erlang:list_to_atom/1`` on strings acquired from the outside world of your program, as it only handles strings with character values under 256. But any character value[^1] under 256 is fair game to be turned into an atom.

```lfe
> (list_to_atom "Joe, Mike and Robert")
|Joe, Mike and Robert|
> (list_to_atom "it's")
it's
> (list_to_atom "hey\n")
|hey\n|
> (list_to_atom (++ "with-supported-char-" [255]))
with-supported-char-ÿ
> (list_to_atom (++ "with-supported-char-" [256]))
exception error: badarg
  in (: erlang list_to_atom (119 105 116 104 45 115 117 112 112 111 ...))
```

-Proctor, Robert

----

[^1]: Remember that character values are non-negative integer values as well, 0-255 inclusive.
