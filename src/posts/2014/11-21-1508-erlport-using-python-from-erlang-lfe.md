---
layout: post.liquid
title: "ErlPort: Using Python from Erlang/LFE"
description: "A Quick Introduction to ErlPort via LFE"
permalink: "/blog/tutorials/2014/11/21/1508-erlport-using-python-from-erlang-lfe"
categories: ["tutorials"]
tags: [howtos, docs, erlport, python, interop, lfe, code, ports, jinterface, clojure, java, libraries, lisp, python 3, tools, reblog, quick-starts]
published_date: 2014-11-21 15:08:00 +0000
is_draft: false
data:
  author: duncan-mcgreggor
  written_for: null
  last_validated: null
  cover_image: "/images/default/LFE_00247_.png"
  cover_alt: "Vigdís — LFE, retro-futurist digital painting"
  math: false
---
This post was originally featured on
[cogitat.io](http://technicae.cogitat.io/2014/11/erlport-using-python-from-erlanglfe.html)
and is being reblogged here as an experiment. This repost is an adventure into
GitHub pages as a blogging platform for code-heavy posts. For year, I have found
Google's blogger.com cumbersome as a medium for sharing code. The burden has
finally grown too great. It makes sense to use the same platform to share the
prose description of code as that which shares the code itself (i.e.,
repositories and `README` files). I can only imagine this will be much less
painful than creating gist code snippets and tweaking them in blogger. As a
bonus, code should now appear in RSS/Atom feeds :-)

## Intro
<a href="/blog/assets/images/posts/Erlang-Python-Greats.png"><img class="right thumb" src="/blog/assets/images/posts/Erlang-Python-Greats.png" /></a>

This blog post is one I've been wanting to get out there ever since
I ran across [the erlport project](http://erlport.org/) a few years ago. I'm
glad to finally have the chance to sit down and get it out there. I hope that
more people who need to take advantage of Python's strengths from Erlang/LFE
find out about this project.

## Strengths and Weaknesses

Erlang was built for fault-tolerance. It had a goal of unprecedented uptimes,
and these have been achieved. It powers 40% of our world's telecommunications
traffic. It's capable of supporting amazing levels of concurrency
(remember the [2007 announcement](https://www.sics.se/~joe/apachevsyaws.html)
about the performance of YAWS vs. Apache?).

However, with this knowledge in mind, a common mistake by folks new to Erlang
is to think these performance characteristics will be applicable to their own
particular domain. This has often resulted in failure, disappointment, and the
unjust blaming of Erlang. If you want to process huge files, do lots of string
manipulation, or crunch tons of numbers, Erlang's not your bag, baby. Try
[Python](https://www.python.org/) or [Julia](http://julialang.org/).

But then, you may be thinking: I like supervision trees. I have long-running
processes that I want to be managed per the rules I establish. I want to run
lots of jobs in parallel on my 64-core box. I want to run jobs in parallel
over the network on 64 of my 64-core boxes. Python's the right tool for the
jobs, but I wish I could manage them with Erlang.

(There are sooo many other options for the use cases above, many of them
really excellent. But this post is about Erlang/LFE :-)).

## Erlang Ports

Traditionally, if you want to run other languages with Erlang in a reliable
way that doesn't bring your Erlang nodes down with badly behaved code, you
use [Ports](http://erlang.org/doc/reference_manual/ports.html).
(more info is available in the
[Interoperability Guide](http://www.erlang.org/doc/tutorial/overview.html)).
This is what
[JInterface](http://www.erlang.org/doc/apps/jinterface/jinterface_users_guide.html)
builds upon (and, incidentally, allows for some pretty cool
[integration with Clojure](https://github.com/oubiwann/lfecljapp)). However,
this still leaves a pretty significant burden for the Python or Ruby developer
for any serious application needs (quick one-offs that only use one or two
data types are not that big a deal).

## ErlPort Quick-Start

[erlport](https://github.com/hdima/erlport) was created by
[Dmitry Vasiliev](https://twitter.com/hdima) in 2009 in an effort to solve just this
problem, making it easier to use of and integrate between Erlang and more common
languages like Python and Ruby. The project is maintained, and in fact has just
received a few updates. Below, we'll demonstrate some usage in
[LFE](http://lfe.io/) with [Python 3](https://docs.python.org/3/).

If you want to follow along, there's a demo repo you can check out:

```bash
$ git clone git@github.com:oubiwann/erlport-demo.git
$ cd erlport-demo
```

Change into the repo directory and set up your Python environment:

```bash
$ cd python
$ python3.4 -m venv .venv
$ . .venv/bin/activate
$ cd ../
```

Next, switch over to the LFE directory, and fire up a REPL:

```bash
$ cd lfe
$ make repl

[snip]

Starting an LFE REPL ...
Erlang/OTP 17 [erts-6.2] [source] [64-bit] [smp:8:8] ...

LFE Shell V6.2 (abort with ^G)
>
```

Note that this will first download the necessary dependencies and compile them
(that's what the `[snip]` is eliding).

Now we're ready to take erlport for a quick trip down to the local:

```lfe
> (set `#(ok ,pid) (python:start))
#(ok <0.32.0>)
> (set result (python:call pid 'sys 'version.__str__ ()))
"3.4.2 (v3.4.2:ab2c023a9432, Oct  5 2014, 20:42:22) ..."
>
```

And that's all there is to it :-)

Perhaps in a future post we can dive into the internals, showing you more of the
glory that is erlport. Even better, we could look at more compelling example
usage, approaching some of the functionality offered by such projects as
[Disco](http://discoproject.org/) or
[Anaconda](http://continuum.io/anaconda-server).

