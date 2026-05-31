---
layout: post.liquid
title: "Easy Python from LFE/Erlang"
description: "The quickest way to use Python from LFE/Erlang"
permalink: "/blog/announcements/2014/12/27/1641-easy-python-from-lfeerlang"
categories: ["announcements"]
tags: ["python", "erlport", "ports", "erlang", "interop", "libraries", "python 3", "docs", "howtos", "code"]
published_date: 2014-12-27 16:41:00 +0000
is_draft: false
data:
  author: duncan-mcgreggor
  written_for: null
  last_validated: null
  cover_image: null
  cover_alt: null
  math: false
---
<a href="/blog/assets/images/posts/Python-logo.png"><img class="right small" src="/blog/assets/images/posts/Python-logo.png" /></a>Recently there has been a great deal of interest in running Python from
Erlang/LFE. In particular,
[these](http://blog.lfe.io/blog/tutorials/2014/11/21/1508-erlport-using-python-from-erlang-lfe/)
[two](http://blog.lfe.io/blog/tutorials/2014/12/03/1828-erlport-and-python-making-more-calls-from-lfe/)
posts have gotten a lot of attention. This post introduces a new library for
LFE and Erlang that builds upon the work demonstrated in those posts, and
furthermore, points towards a future of distributed Python ... a future that is
just a few days away.

The library is [py](https://github.com/lfex/py). It has an extensive README
which is, in essence, a tutorial. As such this post won't cover all the
material provided there -- we'll take a look at some samples and show some
new features that haven't been added to the README yet :-)


## Getting the Code

Be sure to check out the requirements
[here](https://github.com/lfex/py#requirements-)! Then grab the code
and get everything ready:

```bash
$ git clone git@github.com:lfex/py.git
$ cd py
$ make
$ . ./python/.venv/bin/activate
$ make repl-no-deps
```

This will put you into the LFE REPL with everything running and ready to
execute some Python.


## Using ``py``

Let's start off simple by taking a look at all the versions you have:

```lfe
> (py-util:get-versions)
(#(erlang "17")
 #(emulator "6.2")
 #(driver-version "3.1")
 #(lfe "0.9.0")
 #(erlport "0.9.8")
 #(py "0.0.1")
 #(python
   ("3.4.2 (v3.4.2:ab2c023a9432, Oct  5 2014, 20:42:22)"
    "[GCC 4.2.1 (Apple Inc. build 5666) (dot 3)]")))
```

Now a Python call:

```lfe
> (py:func 'datetime.datetime 'now)
#("datetime" #(2014 12 27 17 58 14 810870 undefined))
```

Module-level constants:

```lfe
> (py:const 'math 'pi)
3.141592653589793
```

Instantiating objects:

```lfe
> (py:init 'collections 'UserDict)
#("UserDict" ())
> (py:int #b("101010") '(#(base 2)))
42
> (py:init 'datetime 'date '(1923 4 2))
#("date" #(1923 4 1))
```

Calling methods:

```lfe
> (set now (py:func 'datetime.datetime 'now))
#("datetime" #(2014 12 23 23 14 37 677463 undefined))
> (py:method now 'strftime '(#b("%Y.%m.d %H:%M:%S")))
"2014.12.d 23:14:37"
```

Object attributes:

```lfe
> (py:attr now 'year)
2014
> (py:attr now 'microsecond)
677463
```

Operations on objects:

```lfe
> (set later (py:func 'datetime.datetime 'now))
#("datetime" #(2014 12 23 23 21 25 714474 undefined))
> (set earlier now)
#("datetime" #(2014 12 23 23 14 37 677463 undefined))
> (set diff (py:sub later earlier))
#("timedelta" 0 408 37011)
> (py:attr diff 'seconds)
408
```

Python builtins:

```lfe
> (py:dict)
#("dict" ())
> (py:dict '(#("a" 1) #("b" 2)))
#("dict" (#("b" 2) #("a" 1)))
> (py:any '(true true false false false true))
true
> (py:all '(true true false false false true))
false
> (py:round 0.666666667 5)
0.66667
> (py:range 7 42)
#($erlport.opaque python
  #B(128 2 99 95 95 98 117 105 108 ...))
> (py:len (py:range 7 42))
35
> (py:pylist (py:range 7 42))
(7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30
 31 32 33 34 35 36 ...)
```

Python operators:

```lfe
> (py:add 37 5)
42
> (py:mul 7 6)
42
> (py:sub -108 -150)
42
> (py:truediv 462 11)
42.0
> (py:floordiv 462 11)
42
> (py:gt 7 6)
true
> (py:le 7 6)
false
> (py:eq 42 42)
true
> (py:and- 60 13)
12
> (py:or- 60 13)
61
> (py:xor 60 13)
49
> (py:inv 60)
-61
> (py:rshift 60 2)
15
> (py:lshift 60 2)
240
```

## Erlang?

Yes, all of this is easily available to Erlang too :-) That's the beauty of
LFE -- 100% Erlang Core compatible :-)

There is a link in the project's README file that points to
[example Erlang usage](https://github.com/lfex/py#erlang-) -- be sure to
check that out!


## The ``py-sup`` LFE Supervisor

``py`` runs ErlPort Python servers using an Erlang supervision tree. Since
``py`` comes with an OTP application, the supervision tree gets started with
the application (this happens automatically when you use the ``make repl``
and ``make repl-no-deps`` targets).

To get a list of running Python server Erlang process IDs:

```lfe
> (py:get-python-pids)
(<0.36.0>)
```

The supervisor process ID:

```lfe
> (py:get-sup-pid)
<0.35.0>
```

If your Python server process crashes, the supervisor will restart it. You can
see this in action with the following:

```lfe
> (erlang:exit (car (py:get-python-pids)) 'kill)
true
> (py:get-python-pids)
(<0.37.0>)
```

Notice the incremented process ID for the Python server.

Wait a few seconds and try it again:

```lfe
> (erlang:exit (car (py:get-python-pids)) 'kill)
true
> (py:get-python-pids)
(<0.38.0>)
```

The default restart policy for ``py`` is to only restart if the Python server
hasn't crashed more than 3 times in 1 second. This prevents run-away restarts
in the cases of bad code, pathologically configured Python environment, etc.

Try killing the child Python server process rapidly several times in a row:

```lfe
> (erlang:exit (car (py:get-python-pids)) 'kill)
true
> (erlang:exit (car (py:get-python-pids)) 'kill)
true
> (erlang:exit (car (py:get-python-pids)) 'kill)
true
> (erlang:exit (car (py:get-python-pids)) 'kill)
true
>
=INFO REPORT==== 27-Dec-2014::18:17:21 ===
    application: py
    exited: shutdown
    type: temporary
```

Three was okay -- but not four!

Support for a supervision tree is just the beginning, though ...


## The Future

Current development is focused on three new features for the library:

1. Running multiple Python servers at the same time
1. Creating sophisticated schedulers that determine which Python node to
   execute on next
1. Distributed Python: the crowning glory of Erlang/LFE

To contribute, you can check out the existing tickets
[here](https://github.com/lfex/py/issues). You can also hop on the
[LFE mail list](http://groups.google.com/group/lisp-flavoured-erlang) and ask
questions, make suggestions, etc.
