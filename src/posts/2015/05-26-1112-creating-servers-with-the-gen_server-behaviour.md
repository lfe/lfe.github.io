---
layout: post.liquid
title: "Creating LFE Servers with OTP, Part I"
description: "Creating a generic OTP server in LFE"
permalink: "/blog/tutorials/2015/05/26/1112-creating-servers-with-the-gen_server-behaviour"
categories: ["tutorials"]
tags: ["otp", "erlang"]
published_date: 2015-05-26 11:12:00 +0000
is_draft: false
data:
  author: duncan-mcgreggor
  written_for:
    lfe: "0.9"
    erlang: "17"
  last_validated: null
  cover_image: "/images/tutorials/LFE_Tutorial_00348_.png"
  cover_alt: "Vigdís — LFE tutorial, a retro-futurist study aboard a spaceship"
  math: false
---
<a href="/blog/assets/images/posts/LFE-signal.jpg"><img class="right thumb" src="/blog/assets/images/posts/LFE-signal.jpg" /></a>As mentioned in the previous
post, one of the most common patterns that was identified in Erlang was the
need to create a generic, long running process. This pattern has been codified
in the gen_server behaviour, and it is now time that we got our hands dirty by
creating a few :-)


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

* Requirements, Assumptions, and Code
* How We're Going to Do This
  * About ``gen_server``
  * OTP Boilerplate
  * ``gen_server`` in Two Parts
* Creating a Callback Module
* Creating a Server Module
* Creating An API
* Updating An API
* Full Source Code
* Up Next


## Requirements, Assumptions, and Code

Before reading this tutorial, be sure you have read the ones preceding
it in this series. For a list of what you need to have installed before working
through the examples as well as getting the source code for these tutorials,
please see the post [Prelude to OTP](/blog/tutorials/2015-05-25-0929-prelude-to-otp/),
in particular the sections "Requirements and Assumptions" and "Getting the Code".

Once you have the source code cloned to a working directory, you can compile
the source and start up the LFE REPL with the following:

```bash
$ cd ../tut01
$ make repl
```

## How We're Going to Do This

### About ``gen_server``

The ``gen_server`` behaviour defines a contract between the programmer and the
world of OTP, expecting you to do the following:

* Define a module which implements ``gen_server`` functions
* Define a callback module which implements the required message-passing (and
  callback-related) functions

In return for following these rules, you get an infinitely flexible server with
some amazing capabilities: fault-tolerance, the capacity to handle an incredible
number of simultaneous connections, [^yaws-benchmarks] and the ability to scale
across many cores or many servers.

### OTP Boilerplate

If you have ever looked at Erlang code for ``gen_server``s or other OTP
behaviour implementations, you will have
noticed that setting up a ``gen_server`` involves some boilerplate Erlang data
structures. Newcomers often shake their heads (or even complain loudly!) about
the need for so much awkward data. Once you get used to it, it's really not
a big deal. And, again, the benefits of using OTP -- and the massive time-savings
that go hand-in-hand with those -- far out-weigh the minor inconvenience. You
know this immediately when using OTP, if you have ever had to implement
production-ready custom servers in other programming languages or frameworks.

Perhaps the bit that it most cumbersome for new OTP developers is the fact that,
due to the lack of keyword arguments in Erlang (and the tendency for older Erlang
code not to use
[property lists](http://learnyousomeerlang.com/a-short-visit-to-common-data-structures#key-value-stores)
as a way around this), reading the implementations for the various OTP
behaviours can be a bewildering and frustrating experience.

We are not going to following the Erlang idiom in the LFE tutorials below:
we're going to define variables for all the parameters so that you may more
easily decipher what's happening when you read the code.

### ``gen_server`` in Two Parts

When teaching OTP to new programmers and even seasoned programmers new to Erlang,
I often get questions like the following:

* Why do I have to type the module name so many times?
* What is the different between callback and server modules?
* Which one am I writing now?
* Why is the documentation for them in two different places?
* Why do I put them in a single file?

These questions may not make sense right now, but they will by the time you finish
the tutorial for this post! Hopefully, though, the approach we have decided to
take will not leave you frustrated, but instead the proud holder of new knowledge
and insight.

In particular, we're are again going to follow a non-traditional route, and for
Part I of the ``gen_server`` tutorial we will be splitting our code across two
modules. Furthermore, we will only do a partial implementation of ``gen_server``
in this part.

In Part II, we will migrate our two-module code to a single, integrated module
which a complete implementation of ``gen_server``. In the process, we hope to
answer any lingering questions about the "how" and "why" of ``gen_server``.

That said, we're ready for some code!


## Creating a Callback Module

Let's take a quick look back at our process server from the previous post:

```lfe
(defun process-state (caller state-data)
  (receive
    ('inc
      (process-state caller (+ 1 state-data)))
    ('amount?
        (! caller state-data)
        (process-state caller state-data))))
```

This code combines two aspects:

* the server: the functionality provided by calling ``(receive ...)`` and waiting
  for matching messages, and
* the logic: the code that gets executed when a message matches either ``add``
  or ``amount?``

The most obvious bit, and the bulk of the code, is in the logic, so let's port
that to OTP first. This code will be put in a "callback" module, something
which our new OTP server will make use of. We'll discuss this more shortly,
but for now here's what our logic looks when ported to ``gen_server`` callbacks:

```lfe
(defun handle_cast
  (('increment state-data)
    `#(noreply ,(+ 1 state-data))))

(defun handle_call
  (('amount _caller state-data)
    `#(reply ,state-data ,state-data))
  ((message _caller state-data)
    `#(reply ,(unknown-command) ,state-data)))
```

If we compare this to the process server from the previous post, we can see
that things have started to change rather significantly. First of all, our
callback module has two functions instead of just one: ``handle_cast`` and
``handle_call``. These functions are not used by developers or users of
the OTP software we write; they are defined in a callback module for use by
our ``gen_server``. Let this sit for now -- we'll come back to it shortly.
Let's keep looking at this code:

The ``handle_call`` function is used for making synchronous calls, usually
where a result is expected. This is why we return the ``#(reply ...)`` tuple:
we’re letting OTP know that whatever made this call should get the second
element of the tuple sent to it (in this case, the ``state-data``). The
third element of the tuple is used internally by ``gen_server`` as the
state data used when restarting the loop after this call (all under the hood
and away from view). We did something almost identical in our process example
in the last post: whenever we needed to restart the loop, we passed it the
updated state data. [^return-and-state-same]

Note the reply of ``(unknown-command)`` in the catch-all function head pattern
for ``handle_call``. This is used here for demonstration purposes only. In
Part II of this post we will cover error handling and how to best deal with
unexpected messages in a ``gen_server``.

The ``handle_cast`` function is used for making asynchronous calls, often
convenient when you want to execute a function and don’t care about returning
data to the caller. This is exactly what we’re using it for: we just want our
state data to get incremented; we don’t want a result.

Both functions expect a message (any Erlang term) and the state data for our
``gen_server`` loop. Additionally, the ``handle_call`` function takes a
parameter for the calling function so that it can send results back to it. When
we look at the the API code in our server module, we’ll see where this code
gets called.

The other thing our callback module needs to define is an ``init`` function.
This is used to “prime the pump”, as it were, for the the ``gen_server`` loop.
In other words, this is what initializes the state that gets passed to the
various ``handle_*`` functions. Note that for our example, our state data is
extremely simple: it’s just an integer. But it could be any LFE data structure,
including records (which is very often what the state data is in Erlang and LFE
applications).


## Creating a Server Module

Okay, so we know how our logic gets converted from the non-OTP server loops to
the callback code ... but what calls the callback? If we’re creating a server
in this post then where is the server code? Thanks to OTP (which takes care of
so many of the details), our server code is very simple:

```lfe
(defun start ()
  (gen_server:start (register-name)
                    (callback-module)
                    (initial-state)
                    (genserver-opts)))
```

As we promised earlier, instead of arcane data structures, we have very clearly
defined the variables which are being used as the ``gen_server:start``
arguments. The source code for this tutorial defines those at the top
of the ``tut01-server`` module: [^genserver-args]

```lfe
(defun server-name () (MODULE))
(defun callback-module () 'tut01-callback)
(defun initial-state () 0)
(defun genserver-opts () '())
(defun register-name () `#(local ,(server-name)))
```

Let's address each of the four items that were passed to the ``gen_server:start``
function:

1. We passed a name with which the server will be
   registered. [^start-name] The name is a tuple with the first element being
   either ``local`` or ``global`` and the second being the actual name for the
   process. [^via-name] In our case, we're just using the module name
   to name the server.
1. The second argument is the callback module associated with this server.
   That's what we created in the previous section; it's where all our logic
   lives.
1. In our case, the next argument is the initial state for our server loop, but
   more generally, this is where you (indirectly) pass arguments to the
   ``init`` function you have defined in your ``gen_server``'s callback module.
1. Finally, if we want to pass any options to the ``gen_server`` process
   itself, we can do that here. We've defined ``(genserver-opts)`` to be an
   empty list, since we don’t need to do anything special here. [^genserver-opts]

The full listing of the source code for our server and callback modules is
given at the end of this post, if you'd like to see what we've talked about so
far the their full context.


## Creating An API

Next we will look at our server API. As you recall from the last post, our
“APIs” were hardly that. They consisted of making ``funcall``s in one case, and
in the other, sending messages to the server process via the ``(! ...)`` form.
That changes now :-)

Whenever you have created an implementation of the ``gen_server`` behaviour
(and its associated callback module), you can execute the callback code
by sending messages to your server via ``(gen_server:call ...)`` and
``(gen_server:cast ...)``. We will use these to define a nicely usable API
for our server:

```lfe
(defun inc ()
  (gen_server:cast (server-name) 'increment))

(defun amount? ()
  (gen_server:call (server-name) 'amount))
```

You can imagine that for a large server module, there would be a great many API
functions defined here.

How this works is you call these functions, then ``gen_server`` looks up
the callback module which has been defined for the given server. It then passes
the given message (in our case either ``increment`` or ``amount``). If
``gen_server:cast`` was used to pass the message, then ``handle_cast`` will be
called in the callback module; if ``call`` was used, then ``handle_call`` will
be called.


Let’s try it out:

```lfe
> (tut01-server:start)
#(ok <0.35.0>)
> (tut01-server:inc)
ok
> (tut01-server:inc)
ok
> (tut01-server:amount?)
2
> (tut01-server:inc)
ok
> (tut01-server:amount?)
3
> (gen_server:call (tut01:server-name) 'bingo)
#(error "Unknown command.")
```

How’s *that* for clean! That’s what a good developer experience should look
like :-) None of this crazy you-gotta-make-``funcall``s-and-then-save-state
business.

Let's go over what happened above:

* We started up our server.
  * This initialized the loop with the function from the callback module.
  * But remember: the ``init`` function gets its argument from what we passed
    to ``gen_server:start``, ``0`` in our case.
* We made some API calls -- these were passed on to our callback module by
  the underlying OTP infrastructure.
* We got results for our API functions which made ``call``s.
* We got a simple and reassuring ``ok`` for our API functions which make
  ``cast``s.
* When we skipped the API functions and passed an unexpected message to our
  callbacks directly via ``gen_server:call``, we got the error we defined for
  unknown messages.


## Updating An API

What if we needed to make a change to our API? Asked another way, what does one
need to do in order to add new functionality to a server API? Let’s answer this
by adding a decrement capability to our simple server. We'll start by updating
the API:

```lfe
(defun dec ()
  (gen_server:cast (server-name) 'decrement))
```

That’s the the API function we'll be calling. Now let’s add support for the new
``decrement`` message that it will be sending to ``handle_cast`` in the
callback module:

```lfe
(defun handle_cast
  (('increment state-data)
    `#(noreply ,(+ 1 state-data)))
  (('decrement state-data)
    `#(noreply ,(- state-data 1))))
```

Let's make sure these work as expected:

```lfe
> (tut01-server:start)
#(ok <0.35.0>)
> (tut01-server:inc)
ok
> (tut01-server:amount?)
1
> (tut01-server:dec)
ok
> (tut01-server:dec)
ok
> (tut01-server:amount?)
-1
```

It may seem odd that we've got two distinct bits of code that need to be
updated when when an API is added, but it's really just one: the logic in
the callback module. The server API is syntactic sugar for a better developer
experience; everything will function just fine without it. But you wouldn't
want to do that to your developers, right?


## Full Source Code

The full source code for this tutorial is in the repo you have checked out.
However, it is nice to see the code in the same context as the
blog post, so we've pasted it below.

Here is the server module:

```lfe
(defmodule tut01-server
  (behaviour gen_server)
  (export all))

;;; config functions

(defun server-name () (MODULE))
(defun callback-module () 'tut01-callback)
(defun initial-state () 0)
(defun genserver-opts () '())
(defun register-name () `#(local ,(server-name)))

;;; gen_server implementation

(defun start ()
  (gen_server:start (register-name)
                    (callback-module)
                    (initial-state)
                    (genserver-opts)))

;;; our server API

(defun inc ()
  (gen_server:cast (server-name) 'increment))

(defun dec ()
  (gen_server:cast (server-name) 'decrement))

(defun amount? ()
  (gen_server:call (server-name) 'amount))
```

And here’s the callback module code:

```lfe
(defmodule tut01-callback
  (export all))

;;; config functions

(defun unknown-command () #(error "Unknown command."))

;;; callback implementation

(defun init (initial-state)
  `#(ok ,initial-state))

(defun handle_cast
  (('increment state-data)
    `#(noreply ,(+ 1 state-data)))
  (('decrement state-data)
    `#(noreply ,(- state-data 1))))

(defun handle_call
  (('amount _caller state-data)
    `#(reply ,state-data ,state-data))
  ((message _caller state-data)
    `#(reply ,(unknown-command) ,state-data)))

(defun terminate (_reason _state-data)
  'ok)
```

Note that our callback module doesn’t implement all the callbacks it would need
as part of a full-blown OTP application; we’ll address much of that in the next
post.

Also, we've taken the easy way out for exports (and this is generally frowned
upon): we don't explicitly state which functions we consider public and should
be exported (leaving private functions un-exported). We're trying to keep Part
I very simple so that the concepts don't get lost in the details.


## Up Next

The next post will carry on with ``gen_server``, updating it to handle errors
in a better way and fixing it to reflect the best practices and community
conventions.

----

### Footnotes

[^yaws-benchmarks]: In 2008, The Erlang webserver YAWS was compared to Apache,
    demonstrating its capacity to handle over 80,000 concurrent client
    connections while Apache died at about 4,000. You can view an archived
    version of the report for the benchmark
    [here](https://www.sics.se/~joe/apachevsyaws.html).

[^return-and-state-same]: Note that in this simple example, our
    return value and our state data are one and the same. In a more complicated
    example, one might extract the result from the state data or perform some
    operations on the state data.  Whatever you did, you would put the result
    you wanted to send back to the caller in the second element of the tuple,
    and the updated (or sometimes unchanged) state data you'd put in the third
    element of the tuple.

[^start-name]: In general, this is optional -- you could use ``start/3`` which
    doesn't take a name. In our case, however, we need it so that we can easily
    make calls to the ``gen_server`` process. For that we need to register a
    name so the process can be looked up; if we didn’t do this, we’d need to
    keep track of the process id for our server.

[^via-name]: A third alternative is more rarely used in the cases where one
    needs to implement a custom global registry. In that event, you create a
    3-tuple where the second element is the name of the module which implements
    the registry functions.

[^genserver-opts]: For a list of available options, see the
    [gen_server:start docs](http://www.erlang.org/doc/man/gen_server.html#start-4).

[^genserver-args]: There is no defined convention in LFE for how one sets up
    module-level configuration variables or where these might go: you can put
    the data for the argument values anywhere it makes sense to you.  You don't
    even have to define any -- you can just pass the data as-is in the function
    arguments.  However, there is a lot to be said for the readability of the
    approach we have taken.
