---
layout: post.liquid
title: "LFE Friday - c:xm/1"
description: ""
permalink: "/blog/tutorials/2015/08/02/0212-lfe-friday---cxm1"
categories: ["tutorials"]
tags: ["lfe-friday", "lfe", "erlang"]
published_date: 2015-08-02 02:12:00 +0000
is_draft: false
data:
  author: robert-virding
  written_for: null
  last_validated: null
  cover_image: "/images/fridays/LFE_Friday_00271_.png"
  cover_alt: "Vigdís — LFE Friday, LiffyBot studying Erlang modules in a Mead-style command room"
  math: false
---
<a href="/blog/assets/images/posts/LispFlavoredErlang-medium-square.png"><img class="left tiny" src="/blog/assets/images/posts/LispFlavoredErlang-medium-square.png" /></a>This week's LFE Friday was translated with permission from the
[Erlang Thursday](http://www.proctor-it.com/category/erlang/erlang-thursday/)
series by [Steven Proctor](https://twitter.com/stevenproctor).
*This week's translator*: Robert Virding.

Today's LFE Friday takes a turn down a slightly different route and takes a look in the `c` module at [c:xm/1](http://erlang.org/doc/man/c.html#xm-1).

``c:xm/1`` takes either an atom, representing the name of a module, or a string, representing a filename, and inspects that module for unused and undefined functions, as well as calls to deprecated functions.

First let's take a look at the ``erlang`` module, and see if there is anything in there that is deprecated.


```lfe
> (c:xm 'erlang)
(#(deprecated ()) #(undefined ()) #(unused ()))
```

Looks like there are no calls to deprecated functions, no calls to undefined functions, and no unused functions floating around in the ``erlang`` module.  *Note*: This is running under Erlang 17.0, and you may get a different result depending on the version of Erlang you are running, because ``erlang:now/0`` has been deprecated as of v18.0.

Trying to come up with an example of an existing module that might have some of these criteria, we took a look at the [README for Erlang 17.0](http://www.erlang.org/download/otp_src_17.0.readme), and did a search for ``deprecated``. Doing that there was a note:

<blockquote>The module pg has been deprecated and will be removed in Erlang/OTP 18.</blockquote>

So let's pass that module to ``c:xm/1`` and see what we get.

```lfe
> (c:xm 'pg)
(#(deprecated
   (#(#(pg create 1) #(pg master 1)) #(#(pg create 2) #(pg master 1))))
 #(undefined ())
 #(unused ()))
```

And we can see that we do get information back about deprecated functions in the ``pg`` module.

While the odds are low that you will need to use this function in your normal day to day work, as the tooling around Erlang/LFE generally seems to take care of this for you, this was intriguing enough that it seemed worthy of calling it out, especially if for those time when the compilation of LFE code is done from inside the LFE shell.

-Proctor, Robert
