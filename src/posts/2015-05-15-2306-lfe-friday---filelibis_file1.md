---
layout: post.liquid
title: "LFE Friday - filelib:is_file/1"
description: ""
permalink: "/blog/tutorials/2015/05/15/2306-lfe-friday---filelibis_file1"
categories: ["tutorials"]
tags: ["lfe friday", "lfe", "erlang"]
published_date: 2015-05-15 23:06:00 +0000
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

Today's LFE Friday is [filelib:is_file/1](http://www.erlang.org/doc/man/filelib.html#is_file-1).

``filelib:is_file/1`` takes a string representing a filename, and returns a ``true`` or ``false`` depending on if the name refers to a file or directory.

This can be useful if you are having to read from a configuration file and need to ensure that the file or directory exists before trying to process it, so that you can give a nice error message before quitting, instead of just causing a system error to be raised.

```lfe
> (filelib:is_file "foo")
false
> (filelib:is_file "tmp")
false
> (filelib:is_file "src")
true
> (filelib:is_file "README.md")
true
> (filelib:is_file "/usr/local/bin")
true
> (filelib:is_file "/usr/local/var")
false
> (filelib:is_file ".")
true
> (filelib:is_file "..")
true
```

``filelib:is_file/1`` can also take a atom, or even a deeplist(), representing the filename as well.

```lfe
> (filelib:is_file 'foo)
false
> (filelib:is_file '("/usr" (/local /bin)))
true
```

-Proctor, Robert
