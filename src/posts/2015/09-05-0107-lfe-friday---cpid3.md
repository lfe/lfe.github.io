---
layout: post.liquid
title: "LFE Friday - c:pid/3"
description: ""
permalink: "/blog/tutorials/2015/09/05/0107-lfe-friday---cpid3"
categories: ["tutorials"]
tags: ["lfe friday", "lfe", "erlang"]
published_date: 2015-09-05 01:07:00 +0000
is_draft: false
data:
  author: robert-virding
  written_for: null
  last_validated: null
  cover_image: "/images/fridays/LFE_Friday_00269_.png"
  cover_alt: "Vigdís — LFE Friday, LiffyBot studying Erlang modules in a Mead-style command room"
  math: false
---
<a href="/blog/assets/images/posts/LispFlavoredErlang-medium-square.png"><img class="left tiny" src="/blog/assets/images/posts/LispFlavoredErlang-medium-square.png" /></a>This week's LFE Friday was translated with permission from the
[Erlang Thursday](http://www.proctor-it.com/category/erlang/erlang-thursday/)
series by [Steven Proctor](https://twitter.com/stevenproctor).
*This week's translator*: Robert Virding.

Today's LFE Friday is a short one on [c:pid/3](http://erlang.org/doc/man/c.html#pid-3).

``c:pid/3`` takes the three different parts of a pid as its arguments, and returns a constructed Pid type corresponding to those values.

We'll call ``(self)`` to get a pid that we know is good, and we can use that to compare the result of calling ``c:pid/3``.

```lfe
> (self)        
<0.87.0>
> (c:pid 0 87 0)
<0.87.0>
> (=:= (self) (c:pid 0 87 0))
true
```

Why is this useful?  Sometimes when inspecting what is going on in a live system there are certain calls in LFE that expect a ``pid()`` type, and not just the pid numbers.

```lfe
> (c:regs)

** Registered procs on node nonode@nohost **
Name                  Pid          Initial Call                      Reds Msgs
application_controlle <0.7.0>      erlang:apply/2                    3594    0
code_server           <0.12.0>     erlang:apply/2                  415654    0
erl_prim_loader       <0.3.0>      erlang:apply/2                  900396    0
error_logger          <0.6.0>      gen_event:init_it/6               9565    0
file_server_2         <0.20.0>     file_server:init/1               18072    0
global_group          <0.19.0>     global_group:init/1               8057    0
global_name_server    <0.15.0>     global:init/1                     6341    0
inet_db               <0.18.0>     inet_db:init/1                    2394    0
init                  <0.0.0>      otp_ring0:start/2                 9920    0
kernel_safe_sup       <0.31.0>     supervisor:kernel/1               4389    0
kernel_sup            <0.11.0>     supervisor:kernel/1               9798    0
rex                   <0.14.0>     rpc:init/1                        3086    0
standard_error        <0.22.0>     erlang:apply/2                    5386    0
standard_error_sup    <0.21.0>     supervisor_bridge:standar         1402    0
user                  <0.25.0>     group:server/3                    3202    0
user_drv              <0.24.0>     user_drv:server/2                41003    0

** Registered ports on node nonode@nohost **
Name                  Id              Command                                 
ok
> (erlang:is_process_alive (c:pid 0 7 0))
true
```

So let's see what happens when we try to feed it something to break it, but in a meaningful way.

```lfe
> (c:pid 0 0 0)
<0.0.0>
> (c:pid 111110 0 1111110)
exception error: badarg
  in (: erlang list_to_pid "<111110.0.1111110>")
  in c:pid/3 (c.erl, line 426)
```

So it looks like the pid ``<0.0.0>`` is a valid pid, but when we throw it something else, we see it is trying to call ``erlang:list_to_pid``.

So let's take a quick look at ``erlang:list_to_pid``.

```lfe
> (erlang:list_to_pid "<0.47.0>")
<0.47.0>
> (=:= (c:pid 0 47 0) (erlang:list_to_pid "<0.47.0>"))
true
```

So ``c:pid/3`` is really just a wrapper function around
``erlang:list_to_pid`` that builds the 3 parts of a pid into a string,
and then gets a ``pid()`` type from that call.

As with the earlier ``c`` functions this function exists as an LFE repl built-in command which can be called from the repl with just ``(pid 0 47 0)``.

-Proctor, Robert
