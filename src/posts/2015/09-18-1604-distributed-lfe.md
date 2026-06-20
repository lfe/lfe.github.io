---
layout: post.liquid
title: "Distributed LFE"
description: ""
permalink: "/blog/tutorials/2015/09/18/1604-distributed-lfe"
categories: ["tutorials"]
tags: ["erlang", "otp", "distributed-systems", "remote-nodes", "rpc"]
published_date: 2015-09-18 16:04:00 +0000
is_draft: false
data:
  author: duncan-mcgreggor
  written_for:
    lfe: "0.10"
    erlang: "18"
  last_validated: null
  cover_image: "/images/tutorials/LFE_Tutorial_00375_.png"
  cover_alt: "Vigdís — LFE tutorial, a retro-futurist study aboard a spaceship"
  math: false
---
<a href="/blog/assets/images/posts/LFE-signal.jpg"><img class="right medium" src="/blog/assets/images/posts/LFE-signal.jpg" /></a>The
posts in this series are focused on OTP. However, we're going to take a break
from OTP-proper to talk about some of the basics underlying the patterns that
OTP provides to programmers of the Erlang VM. In particular, we going to take a
look at distributed systems and then do some hands-on distributed system
experimentation with LFE.

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

* Definitions
* Two Nodes, Same Machine
* Two Nodes, Same LAN
* Two Nodes, Same Internet
* Up Next

## Definitions

Let's start off with some definitions. The text *Principles of Distributed
Database Systems* provides us with the following: [^ozsu]

<blockquote>
"The working definition we use for a distributed computing system states that
it is a number of autonomous processing elements (not necessarily homogeneous)
that are interconnected by a computer network and that cooperate in performing
their assigned tasks."
</blockquote>

Another Springer text, *Distributed Algorithms for Message-Passing Systems*
give this as their definition, [^raynal] one closely followed by Wikipedia:

<blockquote>
"A distributed system is made up of a collection of computing units, each one
abstracted through the notion of a process. The processes are assumed to
cooperate on a common goal, which means that they exchange information in one
way or another."
</blockquote>

The [Wikipedia article on distributed computing](https://en.wikipedia.org/wiki/Distributed_computing#Applications)
gives a nice list of applications and examples of distributed systems,
including the following major areas:

* Internetworks (telecommunications and computing)
* Network applications
* Real-time control systems
* Scientific computing and parallel processing

We mention this to provide a connection to reality, so that you don't get lost
in the minutiae of distributed LFE, coming out at the end wondering what it
was all for.

The above two definitions of distributed systems refer to processes. This is a
little too general for our purposes, since all functions in LFE are processes,
and any function can be set up to send or receive messages. We want to take
advantage of the features baked into the Erlang VM for communicating with what
are called *nodes*. As such, the node will be our defining element in a
distributed system.

An LFE node is more than just a process, it is an instance of the Erlang
runtime system (erts) which has been given a name (it cannot
be communicated with if it doesn't have a name).

Okay! Enough with definitions -- let's get coding :->

## Confirm Setup

If you've been following along with these tutorials, then you already have the
code checked out and have been running LFE examples for each. If not, be sure
to following the instructions at the beginning of the post
[Prelude to OTP](/blog/tutorials/2015/05/25/0929-prelude-to-otp/). In particular,
note the instructions in the sections *Requirements and Assumptions* and
*Getting the Code*.

Once you're set up and are in the cloned directory, switch to
this tutorial's directory and start up the REPL:

```bash
$ cd tut02
$ make repl
```
```lfe
LFE Shell V7.0 (abort with ^G)
(repl1@cahwsx01)>
```

## Two Nodes, One Machine

Both Erlang shell and LFE REPL instances are example of Erlang Runtime Systems,
so we can use the REPL as a node. In order to do so, it needs to be given a
name when it starts up. If you take a look at the ``make`` target for starting
the REPL, you'll see that's exactly what we just did. Our ``Makefile`` assigned
the LFE node the default name ``repl1``.

We're going to start another node (REPL) in a separate terminal window, but
we'll override the default with another name, since ``repl1`` is already in
use:

```bash
$ make repl NODE=repl2
```

Just like with our first node, this starts up an LFE REPL named ``repl2``; you
can see the node name in the prompt:

```lfe
LFE Shell V7.0 (abort with ^G)
(repl1@cahwsx01)>
```

Our ``Makefile`` (and its includes) set up various paths, etc., but when all is
said and done, the target above is just calling ``lfe`` with the parameter
``-sname repl2`` (the "s" stands for short). Once our two nodes on the our
machine have been given their names and started, they'll be able to talk to
each other.

Let's use the network administration module's ``ping/1`` function to make sure
we the two nodes are on speaking terms:

```lfe
(repl1@cahwsx01)> (net_adm:ping 'repl2@cahwsx01)
pong
```
```lfe
(repl2@cahwsx01)> (net_adm:ping 'repl1@cahwsx01)
pong
```

We can also use the ``names/0`` function to make sure that both nodes are up:

```lfe
(repl1@cahwsx01)> (net_adm:names)
#(ok (#("repl1" 54162) #("repl2" 54168)))
```

One of the easiest ways to execute code on remote nodes is using the ``rpc``
module in the Erlang stdlib. Here's how you make a call on one particular node:

```lfe
(repl1@cahwsx01)> (rpc:call 'repl2@cahwsx01 'math 'pi '())
3.141592653589793
```

And here's how you make a call on all nodes:

```lfe
(repl1@cahwsx01)> (rpc:multicall 'math 'pi '())
#((3.141592653589793 3.141592653589793) ())
```

You can also make a single call on an arbitrary node:
```lfe
(repl1@cahwsx01)> (rpc:parallel_eval '(#(math pi ())))
(3.141592653589793)
```

``parallel_eval/1`` actually takes a list of calls, returning a list of
results. Here's a better example of that:

```lfe
(repl2@cahwsx01)> (rpc:parallel_eval '(#(math pi ()) #(math exp (1))))
(3.141592653589793 2.718281828459045)
```

Just to mix things up, we made that call on the second node, with the first
node being the remote one.

In addition to calling functions on a remote node and getting results back, we
can actually spawn long-running processes remotely too. This is done in the same
way as spawning a process locally ... except that you need say which node to
spawn on.

Let's define some functions -- we can define all three of these locally -- they
will be sent to the remote node to be executed:

```lfe
(repl1@cahwsx01)> (defun ackermann
                    ((0 n) (+ n 1))
                    ((m 0) (ackermann (- m 1) 1))
                    ((m n) (ackermann (- m 1) (ackermann m (- n 1)))))
ackermann
(repl1@cahwsx01)> (defun ack-server ()
                    (register 'acksvr (self))
                    (ack-loop))
ack-serverack-loop
(repl1@cahwsx01)> (defun ack-loop ()
                    (receive
                      (`#(,pid terminate)
                        (! pid #(ok server-stopped)))
                      (`#(,pid ,m ,n)
                        (! pid (ackermann m n))
                        (ack-loop))))
ack-loop
```

Now we can start up our long-running process and then make calls to it. Note
that since the sender is the REPL process itself, we'll need to call the REPL
``flush`` function to see the messages it received.

```lfe
(repl1@cahwsx01)> (set rem-pid (spawn_link 'repl2@cahwsx01 #'ack-server/0))
<5881.91.0>
(repl1@cahwsx01)> (! #(acksvr repl2@cahwsx01) `#(,(self) 0 3))
#(<0.145.0> 0 3)
(repl1@cahwsx01)> (! #(acksvr repl2@cahwsx01) `#(,(self) 3 0))
#(<0.145.0> 3 0)
(repl1@cahwsx01)> (! #(acksvr repl2@cahwsx01) `#(,(self) 3 3))
#(<0.145.0> 3 3)
(repl1@cahwsx01)> (! #(acksvr repl2@cahwsx01) `#(,(self) terminate))
#(<0.145.0> terminate)
(repl1@cahwsx01)> (c:flush)
Shell got 4
Shell got 5
Shell got 61
Shell got {ok,'server-stopped'}
ok
```

In the ``!`` (send) calls we made above, the process we sent to is a remote
one, so we needed to tell it not only the process (id or name), but also the
node upon which it is running.

Also, we used ``spawn_link`` as opposed to just ``spawn``. That way, if our
long-running ``acksvr`` process died, we'd get a message in the REPL to that
effect. This frees us from having to anticipate timeouts, network outage
conditions, etc., and just handle the failure if and when it occurs.

Alrighty, we've got some basic operations of distributed nodes on a single
machine under our belts. Let's see how things change when we move outside one
machine ...

## Two Nodes, One LAN

One of the simple and brilliant design principles of Erlang is that whether
you're making a call to a process in the same node, to a process in a different
node on the same machine, or a node that is somewhere across a network -- the
usage is the same. Sure, you may need to add more specificity, but that's just
data. Neither the syntax nor semantics change with the location change of the
node.

It should then come as no surprise that two nodes on two different machines in
a local area network are going to communicate using the same ideas we've looked
at already. One difference, though, is how you start the nodes. We're going to
need to start our nodes with the ``-name`` flag instead of the ``-sname`` flag:
when operating across the network, the node name needs to include the hostname
or IP address.

Another is that we'll need to make sure we're using the same Erlang cookie.
We'll do that with the ``-setcookie`` flag in our ``make`` target.

We have a make-target for this, so you don't have to type in all the library
paths. Let's start up one local node and then on a separate machine start
another one:

```bash
$ make net-repl NODE=repl1@10.0.4.64
(repl1@cahwsx01.local)>
```

```bash
$ make net-repl NODE=repl1@10.0.4.181
(repl1@cahltx01.local)>
```

Let's make sure the nodes can see each other:

```lfe
(repl1@10.0.4.64)> (net_adm:ping 'repl1@10.0.4.181)
pong
(repl1@10.0.4.64)>  (net_adm:names "10.0.4.181")
#(ok (#("repl1" 50123)))
```

```lfe
(repl1@10.0.4.181)> (net_adm:ping 'repl1@10.0.4.64)
pong
(repl1@10.0.4.181)> (net_adm:names "10.0.4.64")
#(ok (#("repl1" 54906)))
```

We can make one of the ``rpc`` calls that evaluates code on all nodes as a
check, too:

```lfe
(repl1@10.0.4.64)> (rpc:multicall 'math 'pi '())
#((3.141592653589793 3.141592653589793) ())
```

Looks good!

Now in one of your terminal windows, paste the Ackermann function (and server)
code we created before. The code can be pasted as-is, since none of the logic
is changing. The usage, however, will change, since we need to be more specific
about the host the node is running on.

We chose to define the functions on ``10.0.4.181`` and then to execute remotely
on ``10.0.4.64``:

```lfe
(repl1@10.0.4.181)> (set rem-pid (spawn_link 'repl1@10.0.4.64 #'ack-server/0))
<6430.54.0>
(repl1@10.0.4.181)> (! #(acksvr repl1@10.0.4.64) `#(,(self) 0 3))
#(<0.35.0> 0 3)
(repl1@10.0.4.181)> (! #(acksvr repl1@10.0.4.64) `#(,(self) 3 0))
#(<0.35.0> 3 0)
(repl1@10.0.4.181)> (! #(acksvr repl1@10.0.4.64) `#(,(self) 3 3))
#(<0.35.0> 3 3)
(repl1@10.0.4.181)> (! #(acksvr repl1@10.0.4.64) `#(,(self) terminate))
#(<0.35.0> terminate)
(repl1@10.0.4.181)> (c:flush)
Shell got 4
Shell got 5
Shell got 61
Shell got {ok,'server-stopped'}
ok
```

That's pretty durned sweet,

Note that we could save this in a module and execute as an RPC, or use OTP
behaviours and put it in a supervision tree, etc. All of LFE/Erlang/OTP
applies. Nothing has to be fundamentally changed just because we're operating
on remote nodes. The simplicity and elegance of this is quite wonderful.

## Two Nodes, One Internet

Guess what? Same deal! You just need to make sure that your selected hosts
resolve and that they share the same Erlang cookie data. Please be careful,
though: only connect to remote Erlang nodes across non-secure networks if the
node-to-node communications are secure (e.g., SSH tunnels, VPN connections,
etc.).

## Up Next

In the next installment of this series we'll dive back into OTP, examining
one of the lesser-known behaviours it offers: a finite state machine server.

----

[^ozsu]: See M. Tamer  Özsu's *Principles of Distributed Database Systems* from Springer, page 2.

[^raynal]: See Michel Raynal's *Distributed Algorithms for Message-Passing Systems* from Springer, page 3.

