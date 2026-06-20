---
layout: post.liquid
title: "Creating LFE Servers with OTP, Part II"
description: "Taking the LFE gen_server to the next level"
permalink: "/blog/tutorials/2015/05/28/1008-creating-servers-with-the-gen_server-behaviour-ii"
categories: ["tutorials"]
tags: ["otp", "erlang"]
published_date: 2015-05-28 10:08:00 +0000
is_draft: false
data:
  author: duncan-mcgreggor
  written_for:
    lfe: "0.9"
    erlang: "17"
  last_validated: null
  cover_image: "/images/tutorials/LFE_Tutorial_00332_.png"
  cover_alt: "Vigdís — LFE tutorial, a retro-futurist study aboard a spaceship"
  math: false
---
<a href="/blog/assets/images/posts/LFE-signal.jpg"><img class="left thumb" src="/blog/assets/images/posts/LFE-signal.jpg" /></a>
In the last post, we went on a whirlwind tour of ``gen_server``'s basic
functionality: we created a callback module which embodied our logic; we
created a server module that was responsible for setting up the loop;, and we
added an API to wrap ``gen_server:cast`` and ``gen_server:call`` functions that
passed messages to our callback logic. In this post we're going to follow up on
that work:

* Update our code to use OTP community best practices
* Add support for stopping the server.
* Improve the support for handling unexpected messages.


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
* Best Practices
  * Unified Code
  * Exports
  * All Callbacks
* Stopping a ``gen_server``
* Expecting the Unexpected
* Full Source Code
* Learning More About ``gen_server``
* Up Next


## Requirements, Assumptions, and Code

Before reading this tutorial, be sure you should have read the ones preceding
it in this series. For a list of what you need to have installed before working
through the examples as well as getting the source code for these tutorials,
please see the post [Prelude to OTP](/blog/tutorials/2015-05-25-0929-prelude-to-otp/).

In the last post, we compiled all the code and started the LFE REPL with the
following command:

```bash
$ cd tut01
$ make repl
```

## Best Practices

The next few sections will cover some of the best practices that we glossed
over or completely ignored in the last post. Our intent was to convey the
core concepts of ``gen_server`` without drowning the new OTP developer in
a sea of details. Now it's time to complete your ``gen_server`` education
and tell you the rest of the story. Ready to swim?


### Unified Code

Even though OTP provides the developer with the ability to define the callbacks
in a separate module like we did in the last post, in practice this feature is
not generally utilized. Instead, both the callback logic and the server code are
kept in the same module. This makes it possible for the two aspects of ``gen_server``
to share private functions and it makes it easier to refer to each other (no need to
make calls to another module).

It may seem a bit awkward at first, though, since you'll be using the same name for
the ``gen_server`` and for the callback module (namely, ``(MODULE)``), but you'll
get used to it quickly enough.

The ``tut01/src`` directory which holds the source code for this post (and the previous
one) has a combined module, ``tut01.lfe`` holding the functionality we previously
defined in ``tut01-server.lfe`` and ``tut01-callback.lfe``. We'll give a full
listing at the end of this post.


### Explicit Exports

The next thing we need to fix from the last tutorial is the lazy use of ``(export all)``.
It is much better to declare exactly what you want exported for public use. The explicit
exporting of public functions is part of the (self-) documentation for your module.

When opening your module in an editor, this is going to be less helpful:

```lfe
(defmodule tut01-server
  (behaviour gen_server)
  (export all))
```

Than this:

```lfe
(defmodule tut01
  (behaviour gen_server)
  (export
    ;; gen_server implementation
    (start 0)
    (stop 0)
    ;; callback implementation
    (init 1)
    (handle_call 3)
    (handle_cast 2)
    (handle_info 2)
    (terminate 2)
    (code_change 3)
    ;; server API
    (inc 0)
    (dec 0)
    (amount? 0)))
```


### All Callbacks

The last best practice we're going to look at now is the inclusion of all callbacks.
When you compile an LFE module that declares an OTP behaviour, it doesn't complain
if you leave out a required function. It successfully compiles and will run just fine.
However, when you do this you are not abiding by the contract with the OTP world.
This can have the practical result of causing unexpected bugs and/or breakages in code,
especially in the code of your users who would be expecting your application to
respect the OTP contract.

So what do we need to add to our new, unified ``gen_server`` and callback module that
we left out in the last post? Here are the additional required ``gen_server``
functions:

* ``handle_info/2``
* ``terminate/2``
* ``code_change/3``

The final, optional ``gen_server`` callback function is ``format_status/2``, but we'll
discuss that in a future post when we touch on the topic of monitoring nodes.

The functions ``handle_info`` and ``terminate`` will be the topic of the next sections,
so let's stub out a quick ``code_change`` implementation:

```lfe
(defun code_change (_old-version state _extra)
  `#(ok ,state))
```

A future blog post will dive into the topic of hot-loading new code into a running
server (zero downtime!), and at that point we'll provide a *real* implementation
of ``code_change``. For now, what we have above will be a placeholder.


## API Update Reprise: Stopping a ``gen_server``

In the last post we showed how easy it was to update an API (and what was needed in
order to do so). We're going to return to this topic, but with a twist. We're going
to add a ``stop`` function to our API, but stopping a service is a little more
involved than a regular API addition. We'll explain as we go.

Here's the new API function:

```lfe
(defun stop ()
  (gen_server:call (server-name) 'stop))
```

That’s the easy bit. Now let’s add support for this new ``stop`` message to our
``handle_call`` callback function:

```lfe
(defun handle_call
  (('amount _caller state-data)
    `#(reply ,state-data ,state-data))
  (('stop _caller state-data)
    `#(stop shutdown ok state-data))
  ((message _caller state-data)
    `#(reply ,(unknown-command) ,state-data)))
```

Notice that we've got a new return tuple: it doesn't start with ``reply`` or ``noreply``.
Instead, it sends the ``stop`` message.

If we tried to run our server with just this
change to the callback, we would get an error after calling our new ``(tut01:stop)``
API function:

```erlang
=ERROR REPORT==== 25-May-2015::19:27:16 ===
** Generic server tut01 terminating
** Last message in was stop
** When Server state == 'state-data'
** Reason for termination ==
** {'function not exported',
       [{'tut01-callback',terminate,...},
        {gen_server,try_terminate,...},
        {gen_server,terminate,7,...},
        {gen_server,handle_msg,5,...},
        {proc_lib,init_p_do_apply,3,...}]}
```

You will see that error message when you haven’t defined the ``terminate``
callback function. Here’s a quick one. Let's fix that:

```lfe
(defun terminate (_reason _state-data)
  'ok)
```

Now when we stop our server using our new API, we have success:

```lfe
> (tut01:start)
#(ok <0.35.0>)
> (tut01:stop)
ok
```

And we can demonstrate that it’s really stopped by trying to call our
``amount?`` API function:

```lfe
> (tut01:amount?)
exception exit: #(noproc #(gen_server call (tut01 amount)))
  in gen_server:call/2 (gen_server.erl, line 182)
```


## Expecting the Unexpected

In the last post, we added a quick catch-all pattern to our ``handle_call`` callback
to provide a user with feedback whenever they attempt to call an API that's not
defined. This is rather fragile, since there are different types of messages which
can be sent to the ``gen_server``, many of which won't be done by a human user.

Here's an example stray message:

```lfe
> (! (whereis 'tut01) 'bingo)
bingo
```

Which causes the termination of our server:

```erlang
=ERROR REPORT==== 30-May-2015::20:38:25 ===
** Generic server tut01 terminating
** Last message in was bingo
** When Server state == 0
** Reason for termination ==
** {'function not exported',
       [{tut01,handle_info,[bingo,0],[]},
        {gen_server,try_dispatch,4,[{file,"gen_server.erl"},{line,593}]},
        {gen_server,handle_msg,5,[{file,"gen_server.erl"},{line,659}]},
        {proc_lib,init_p_do_apply,3,[{file,"proc_lib.erl"},{line,237}]}]}
```
        
The callback that ``gen_server`` will use to handle undefined messages is ``handle_info``.
Let's create an implementation for this callback which is less fragile that our
original:

```lfe
(defun handle_info
  ((`#(EXIT ,_pid normal) state-data)
   `#(noreply ,state-data))
  ((`#(EXIT ,pid ,reason) state-data)
   (io:format "Process ~p exited! (Reason: ~p)~n" `(,pid ,reason))
   `#(noreply ,state-data))
  ((_msg state-data)
   `#(noreply ,state-data)))
```

Let's play ``gen_server`` bingo again:

```lfe
> (tut01:start)
#(ok <0.35.0>)
> (! (whereis 'tut01) 'bingo)
bingo
```

So much nicer!


## Full Source Code

With all these changes, we have some new source code to enjoy:

```lfe
(defmodule tut01
  (behaviour gen_server)
  (export
    ;; gen_server implementation
    (start 0)
    (stop 0)
    ;; callback implementation
    (init 1)
    (handle_call 3)
    (handle_cast 2)
    (handle_info 2)
    (terminate 2)
    (code_change 3)
    ;; server API
    (inc 0)
    (dec 0)
    (amount? 0)))

;;; config functions

(defun server-name () (MODULE))
(defun callback-module () (MODULE))
(defun initial-state () 0)
(defun genserver-opts () '())
(defun register-name () `#(local ,(server-name)))
(defun unknown-command () #(error "Unknown command."))

;;; gen_server implementation

(defun start ()
  (gen_server:start (register-name)
                    (callback-module)
                    (initial-state)
                    (genserver-opts)))

(defun stop ()
  (gen_server:call (server-name) 'stop))

;;; callback implementation

(defun init (initial-state)
  `#(ok ,initial-state))

(defun handle_cast
  (('inc state-data)
    `#(noreply ,(+ 1 state-data)))
  (('dec state-data)
    `#(noreply ,(- state-data 1))))

(defun handle_call
  (('amount _caller state-data)
    `#(reply ,state-data ,state-data))
  (('stop _caller state-data)
    `#(stop shutdown ok state-data))
  ((message _caller state-data)
    `#(reply ,(unknown-command) ,state-data)))

(defun handle_info
  ((`#(EXIT ,_pid normal) state-data)
   `#(noreply ,state-data))
  ((`#(EXIT ,pid ,reason) state-data)
   (io:format "Process ~p exited! (Reason: ~p)~n" `(,pid ,reason))
   `#(noreply ,state-data))
  ((_msg state-data)
   `#(noreply ,state-data)))

(defun terminate (_reason _state-data)
  'ok)

(defun code_change (_old-version state _extra)
  `#(ok ,state))

;;; our server API

(defun inc ()
  (gen_server:cast (server-name) 'inc))

(defun dec ()
  (gen_server:cast (server-name) 'dec))

(defun amount? ()
  (gen_server:call (server-name) 'amount))
```

Ah, look at all those beautiful parentheses :-) [^stop-placement]


## Learning More About ``gen_server``

There’s a lot of information we didn’t cover in this tutorial, however it’s
enough to get started writing simple OTP servers in LFE. If you’d like to learn
more, one of the newest books out there on the topic has been recently
published by O’Reilly:
[Designing for Scalability with Erlang/OTP](http://shop.oreilly.com/product/0636920024149.do).
The chapter on ``gen_server`` covers this material in much more detail,
including timeouts, deadlocks, hibernating, and custom global registries.

Another good resource, once you get up to speed, is the
[Erlang documentation](http://www.erlang.org/doc/man/gen_server.html) (and
[here](http://www.erlang.org/doc/design_principles/gen_server_concepts.html)).
Often overlooked, it's actually really good and will be a constant companion
for you any time you need to do something with OTP that you haven't tried
before.

In future posts in this series, we will be covering bits we've left out of this
tutorial, namely:

* ``code_change`` - supporting hot-loading of code in a running system
* ``format_status`` - providing custom status data for a running server


## Up Next

Before we tackle any other behaviours, we’re going to explore distributed
generic servers: running our code on multiple cores and multiple machines.

----

### Footnotes

[^stop-placement]: You might have noticed that we put the ``stop`` API function
                   in with ``start``. Even though ``stop`` is not defined for
                   ``gen_server``, we still consider it a "server management"
                   function and thus place it with its peer, ``start``.
