---
layout: post.liquid
title: "Prelude to OTP"
description: "A look at some examples which motivate the use of OTP"
permalink: "/blog/tutorials/2015/05/25/0929-prelude-to-otp"
categories: ["tutorials"]
tags: ["otp", "erlang", "closures", "scheme"]
published_date: 2015-05-25 09:29:00 +0000
is_draft: false
data:
  author: duncan-mcgreggor
  written_for:
    lfe: "0.9"
    erlang: "17"
  last_validated: null
  cover_image: "/images/tutorials/LFE_Tutorial_00333_.png"
  cover_alt: "Vigdís — LFE tutorial, a retro-futurist study aboard a spaceship"
  math: false
---
<a href="/blog/assets/images/posts/LFE-signal.jpg"><img class="right thumb" src="/blog/assets/images/posts/LFE-signal.jpg" /></a>In this post we take a look at some non-OTP code examples discussed in [Casting SPELs in Lisp (LFE Edition)](http://lfe.gitbooks.io/casting-spels/), examine their differences and similarities, and then briefly describe what they are missing which OTP might be able to provide.


## LFE OTP Tutorial Series

* [Introducing the LFE OTP Tutorials](/blog/tutorials/2015/05/23/1720-new-series-lfe-otp-tutorials/)
* [What is OTP?](/blog/tutorials/2015/05/24/1808-what-is-otp/)
* [Prelude to OTP](/blog/tutorials/2015/05/25/0929-prelude-to-otp/)
* [Creating LFE Servers with OTP, Part I](/blog/tutorials/2015/05/26/1112-creating-servers-with-the-gen_server-behaviour/)
* [Creating LFE Servers with OTP, Part II](/blog/tutorials/2015/05/28/1008-creating-servers-with-the-gen_server-behaviour-ii/)
* [Distributed LFE](/blog/tutorials/2015/09/18/1604-distributed-lfe/)

You can leave feedback for the LFE OTP tutorials
[here](https://github.com/lfe/blog/issues/7).


## In This Post

* Requirements and Assumptions
* Getting the Code
* Two Rudimentary Servers
* Up Next
* Footnotes


## Requirements and Assumptions

In order to work through the code samples in these tutorials, you will need the following:

* A recent installation of Erlang (R15 and later should work just fine)
* ``git``
* ``rebar``
* The dev version of ``lfetool`` (see [Installing the dev version of lfetool](https://github.com/lfe/lfetool/tree/dev-v1#dev-))

These tutorials assume one or more of the following:

* prior experience with Erlang (see Introducing the LFE OTP Tutorials for
  excellent Erlang reference material)
* prior experience with a Lisp, preferably a Lisp-2 (since LFE is a Lisp-2)
* previous exploration of LFE, walking through the
  [LFE Quickstart](http://lfe.gitbooks.io/quick-start/), reviewing the
  [Reference Manual](http://lfe.gitbooks.io/reference-guide/), reading
  [Casting SPELs in Lisp](http://lfe.gitbooks.io/casting-spels/),
  working through the examples in the
  [LFE Tutorial](http://lfe.gitbooks.io/tutorial/), or even reading the
  first chapter of [SICP for LFE](http://lfe.gitbooks.io/sicp/).


## Getting the Code

Before we get started, let’s download the code for these tutorials:

```bash
$ git clone https://github.com/oubiwann/lfe-otp-tutorials.git
$ cd lfe-otp-tutorials
```


## Two Rudimentary Servers

One of the most common patterns that was identified in Erlang prior to the
genesis of OTP was the need to create a generic server: a long running process
doing something as simple as responding to command messages to a full-blown
TCP/IP server implementing any number of services. From this need was born the
``gen_server`` behaviour. Before we dive into that, though, let’s look at some
motivating examples.

In [Chapter 9](http://lfe.gitbooks.io/casting-spels/content/book/part7/README.html)
of the mini-book, Casting SPELs in Lisp (LFE Edition), we tackled the concept
of a game “server” whose primary purpose was twofold:

* to maintain game state data, and
* to provide an API for accessing that data.

We started with a classic Lisp example of using closures [^1] to maintain state
and then substituted LFE processes for lambdas. It is interesting to note that
there was a similar correlation made in 1972 by Sussman and Steele which
ultimately led to the birth of Scheme: [^2]

<blockquote>
“They soon concluded Actors were essentially closures that never return but
instead invoke a continuation, and thus they decided that the closure and the
Actor were, for the purposes of their investigation, essentially identical
concepts. They eliminated what they regarded as redundant code and, at that
point, discovered that they had written a very small and capable dialect of
Lisp.”
</blockquote>

Our closure-based example server was a function that looked like this: [^3]

```lfe
(defun lambda-state (state-data)
  (lambda (msg)
    (case msg
      ('inc
        (lambda-state (+ 1 state-data)))
      ('amount?
        state-data))))
```

We then converted it to something that looked like this:

```lfe
(defun process-state (caller state-data)
  (receive
    ('inc
      (process-state caller (+ 1 state-data)))
    ('amount?
        (! caller state-data)
        (process-state caller state-data))))
```

You can see that the general form remains pretty much the same: instead of a
lambda expression that takes a message as an argument, we have the LFE
``receive`` form which does the same. [^4] They differ significantly in one
respect, however: their server loop implementation. In order to maintain state
over time after changes to that state, the lambda closure version requires that
the developer re-call the ``lambda-state`` function, passing a new or changed
state value as an argument.  This is not needed when simply accessing the state
data , though, since no state has changed, and whatever state data the
developer has will remain just as valid regardless of how many times the
``amount?`` message is sent.

The process version, however, provides a true loop: every time it receives a
message it must restart the loop by calling itself again, thus re-listening for
subsequent messages. To actually get values to the caller (e.g., when the
``amount?`` message is received), it must send the values to the calling
process.

The similarities of both approaches may be summarized as the following:

* they provide a mechanism for maintaining state over time
* they implement a primitive form of “server loop”
* they provide a rudimentary “API” for accessing and modifying state data

Let’s give these a try by starting up the LFE REPL for this code (the last
shell command entered should have been the one changing directory into the
newly cloned repository for these tutorials):

```bash
$ cd tut00
$ make repl
```

With the lambda version, the following rather awkward usage was required:

```lfe
> (set st (tut00:lambda-state 0))
#Fun<tut00.0.29422318>
> (set st (funcall st inc))
#Fun<tut00.0.29422318>
> (set st (funcall st inc))
#Fun<tut00.0.29422318>
> (funcall st 'amount?)
2
> (set st (funcall st inc))
#Fun<tut00.0.29422318>
> (funcall st 'amount?)
3
```

With the LFE process version, usage was much cleaner:

```lfe
> (set st (spawn 'tut00 'process-state `(,(self) 0)))
<0.35.0>
> (! st 'inc)
inc
> (! st 'inc)
inc
> (! st 'amount?)
amount?
> (c:flush)
Shell got 2
ok
> (! st 'inc)
inc
> (! st 'amount?)
amount?
> (c:flush)
Shell got 3
ok
```

In the process version, we don’t have to keep setting our state value, since
the process we spawn (and which calls itself again after receiving each
message) does that for us. We also don’t need to ``funcall`` everything, since
there are no lambda functions being returned. However, we do need a process
that can receive messages from our spawned “server” when we send the
``amount?`` message. The REPL plays that role, but to see the messages that is
sent to the REPL from our server process, we need to flush the REPL process’
mailbox. So there’s still some awkwardness.

We could, of course, create a second process (a “callback” process) which does
a better job in that role than what the REPL does. If we did, then we’d find
that once we need to run multiple long-running processes, we’d have to add some
code to make sure they all came up in the proper order with the necessary
dependencies managed. We’d eventually need to implement strategies for flapping
services, hot-code loading, etc. So here’s a better idea: let’s use
``gen_server`` instead, and we’ll get all of that for free -- even if we don’t
need to use all of it just yet :-)

## Up Next

With the next post we will finally get to write some LFE code for OTP! We’ll
take a look at some of the basics of the ``gen_server`` behaviour and how it
helps us maintain state more easily and allows for a cleaner API.

----

### Footnotes

[^1]: The term “closure” was coined by Peter Landin as part of his work on the
    SECD virtual machine, the first such to be specifically designed for
    evaluating lambda calculus expressions. (He also tutored Tony Hoare in
    Algol.)

[^2]: Taken from [Carl Hewitt, the Actor model, and the birth of
    Scheme](http://en.wikipedia.org/wiki/History_of_the_Scheme_programming_language#Carl_Hewitt.2C_the_Actor_model.2C_and_the_birth_of_Scheme)

[^3]: This use of closures can be used to build object-oriented frameworks in
    Lisp, and in fast, is what gave birth to the Common Lisp Object System.
    Peter Norvig’s excellent [Paradigms of Artificial Intelligence Programming:
    Case Studies in Common Lisp](http://smile.amazon.com/dp/1558601910) steps
    through an example of this in Chapter 13.

[^4]: For more insight on closures in LFE, you can review
    [an example](https://github.com/rvirding/lfe/blob/develop/examples/internal-state.lfe)
    converted from Peter Norvig's PAIP. Furthermore, you can see another
    example contrasting closures and processes in the LFE source code:

      * [objects via closures](https://github.com/rvirding/lfe/blob/develop/examples/object-via-closure.lfe)
      * [objects via processes](https://github.com/rvirding/lfe/blob/develop/examples/object-via-process.lfe)
