---
layout: post.liquid
title: "DragonFly BSD & LFE"
description: ""
permalink: "/blog/tutorials/2015/07/08/1349-dragonflybsd--lfe"
categories: ["tutorials"]
tags: ["dragonflybsd", "installs", "bsd"]
published_date: 2015-07-08 13:49:00 +0000
is_draft: false
data:
  author: duncan-mcgreggor
  written_for: null
  last_validated: null
  cover_image: "/images/tutorials/LFE_Tutorial_00332_.png"
  cover_alt: "Vigdís — LFE tutorial, a retro-futurist study aboard a spaceship"
  math: false
---
<a href="/blog/assets/images/posts/dfly-bsd-logo.png"><img class="right thumb" src="/blog/assets/images/posts/dfly-bsd-logo.png" /></a>Yesterday
there was [a tweet](https://twitter.com/dysinger/status/618485238230966272)
playfully ribbing Golang. It referenced an old WhatsApp
[blog post](https://blog.whatsapp.com/196/1-million-is-so-2011) which
briefly went over the WhatsApp engineering team's use of tuned FreeBSD
machines running Erlang ... machines that were capable of handling 2 million
TCP connections on a single machine. As such, it seemed like a good time to
talk about BSD and LFE :-)

[DragonFly BSD](https://www.dragonflybsd.org/) and LFE are a *great* match.
DragonFly's core focus is scalability, with features such as lightweight
kernel threads, a lightweight ports/messaging system, and the
[HAMMER file system](https://en.wikipedia.org/wiki/HAMMER). Installation of
Erlang, LFE, and related tools is a snap. By default, DragonFly comes with
``git`` and ``curl`` pre-installed, so this is all that's needed for a base
Erlang system:

```
% su -
# pkg update
# pkg install -y erlang rebar rebar3
```

The latest DragonFly installs Erlang 17.5.

To support LFE and ``lfetool``, a few more packages are needed:

```
# pkg install -y gmake base64 bash
# rehash
```

With this done, you just need to do the usual to get LFE:

```
# cd /usr/local
# git clone https://github.com/rvirding/lfe
# cd lfe
# gmake && gmake install
```

And the dev-v1 version of ``lfetool`` now supports installation on *BSD
machines:

```
# curl -L -o ./lfetool https://raw.github.com/lfe/lfetool/dev-v1/lfetool
# bash ./lfetool install &&  rm lfetool
# rehash
```

With that, you can exit as ``root``

```
# exit
logout
oubiwann@mndvmb01% lfe
```

and run LFE as a normal user:

```bash
# exit
oubiwann@mndvmb01% lfe
Erlang/OTP 17 [erts-6.4.1] [source] [64-bit] ,,,

> (* 2 (lists:foldl (lambda (n acc) (+ n acc)) 0 (lists:seq 1 6)))
42
```

And that's all there is to it!
