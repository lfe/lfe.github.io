---
layout: post.liquid
title: "LFE Friday - c:regs/0"
description: ""
permalink: "/blog/tutorials/2015/08/15/0226-lfe-friday---cregs0"
categories: ["tutorials"]
tags: ["lfe-friday", "lfe", "erlang"]
published_date: 2015-08-15 02:26:00 +0000
is_draft: false
data:
  author: robert-virding
  written_for: null
  last_validated: null
  cover_image: "/images/fridays/LFE_Friday_00288_.png"
  cover_alt: "Vigdís — LFE Friday, LiffyBot studying Erlang modules in a Mead-style command room"
  math: false
---
<a href="/blog/assets/images/posts/LispFlavoredErlang-medium-square.png"><img class="left tiny" src="/blog/assets/images/posts/LispFlavoredErlang-medium-square.png" /></a>This week's LFE Friday was translated with permission from the
[Erlang Thursday](http://www.proctor-it.com/category/erlang/erlang-thursday/)
series by [Steven Proctor](https://twitter.com/stevenproctor).
*This week's translator*: Robert Virding.

Today's LFE Friday continues looking at the ``c`` module, and looks at [c:regs/0](http://erlang.org/doc/man/c.html#regs-0).

``c:regs/0`` displays information about the registered processes on the current node, such as process name, process id, the number of reductions[^1] performed, and more.

```lfe
> (c:regs)

** Registered procs on node nonode@nohost **
Name                  Pid          Initial Call                      Reds Msgs
application_controlle <0.7.0>      erlang:apply/2                     404    0
code_server           <0.12.0>     erlang:apply/2                  126526    0
erl_prim_loader       <0.3.0>      erlang:apply/2                  171328    0
error_logger          <0.6.0>      gen_event:init_it/6                223    0
file_server_2         <0.20.0>     file_server:init/1                  84    0
global_group          <0.19.0>     global_group:init/1                 53    0
global_name_server    <0.15.0>     global:init/1                       45    0
inet_db               <0.18.0>     inet_db:init/1                     196    0
init                  <0.0.0>      otp_ring0:start/2                 2392    0
kernel_safe_sup       <0.31.0>     supervisor:kernel/1                 56    0
kernel_sup            <0.11.0>     supervisor:kernel/1               1860    0
rex                   <0.14.0>     rpc:init/1                          21    0
standard_error        <0.22.0>     erlang:apply/2                       9    0
standard_error_sup    <0.21.0>     supervisor_bridge:standar           34    0
user                  <0.25.0>     group:server/3                      36    0
user_drv              <0.24.0>     user_drv:server/2                 1305    0

** Registered ports on node nonode@nohost **
Name                  Id              Command                                 
ok
```

While this is not quite as nice as what is provided by the ``observer`` GUI, this is a useful tool to be able to get an idea what what the processes are, and what they are doing when you are not able to have the ``observer`` GUI running.

The ``c`` module also contains a function ``c:nregs/0`` which displays information about all processes for all of the nodes that the node it is run from knows about.

```lfe
(foo@127.0.0.1)> (node)
foo@127.0.0.1
(foo@127.0.0.1)> (nodes)
(bar@127.0.0.1)
(foo@127.0.0.1)> (c:nregs)

** Registered procs on node 'foo@127.0.0.1' **
Name                  Pid          Initial Call                      Reds Msgs
application_controlle <0.7.0>      erlang:apply/2                     404    0
auth                  <0.21.0>     auth:init/1                        844    0
code_server           <0.12.0>     erlang:apply/2                  131711    0
erl_epmd              <0.20.0>     erl_epmd:init/1                    263    0
erl_prim_loader       <0.3.0>      erlang:apply/2                  208774    0
error_logger          <0.6.0>      gen_event:init_it/6                304    0
file_server_2         <0.26.0>     file_server:init/1                  84    0
global_group          <0.25.0>     global_group:init/1                 63    0
global_name_server    <0.15.0>     global:init/1                      281    0
inet_db               <0.18.0>     inet_db:init/1                     214    0
inet_gethost_native   <0.45.0>     inet_gethost_native:serve           83    0
inet_gethost_native_s <0.44.0>     supervisor_bridge:inet_ge           34    0
init                  <0.0.0>      otp_ring0:start/2                 3304    0
kernel_safe_sup       <0.37.0>     supervisor:kernel/1                127    0
kernel_sup            <0.11.0>     supervisor:kernel/1               2611    0
net_kernel            <0.22.0>     net_kernel:init/1                  817    0
net_sup               <0.19.0>     supervisor:erl_distributi          288    0
rex                   <0.14.0>     rpc:init/1                          21    0
standard_error        <0.28.0>     erlang:apply/2                       9    0
standard_error_sup    <0.27.0>     supervisor_bridge:standar           34    0
user                  <0.31.0>     group:server/3                      36    0
user_drv              <0.30.0>     user_drv:server/2                 4380    0

** Registered ports on node 'foo@127.0.0.1' **
Name                  Id              Command                                 

** Registered procs on node 'bar@127.0.0.1' **
Name                  Pid          Initial Call                      Reds Msgs
application_controlle <5563.7.0>   erlang:apply/2                     404    0
auth                  <5563.21.0>  auth:init/1                        844    0
code_server           <5563.12.0>  erlang:apply/2                  126892    0
erl_epmd              <5563.20.0>  erl_epmd:init/1                    263    0
erl_prim_loader       <5563.3.0>   erlang:apply/2                  169796    0
error_logger          <5563.6.0>   gen_event:init_it/6                267    0
file_server_2         <5563.26.0>  file_server:init/1                  84    0
global_group          <5563.25.0>  global_group:init/1                 63    0
global_name_server    <5563.15.0>  global:init/1                      281    0
inet_db               <5563.18.0>  inet_db:init/1                     214    0
init                  <5563.0.0>   otp_ring0:start/2                 3237    0
kernel_safe_sup       <5563.37.0>  supervisor:kernel/1                 56    0
kernel_sup            <5563.11.0>  supervisor:kernel/1               2611    0
net_kernel            <5563.22.0>  net_kernel:init/1                  812    0
net_sup               <5563.19.0>  supervisor:erl_distributi          288    0
rex                   <5563.14.0>  rpc:init/1                         953    0
standard_error        <5563.28.0>  erlang:apply/2                       9    0
standard_error_sup    <5563.27.0>  supervisor_bridge:standar           34    0
user                  <5563.31.0>  group:server/3                      36    0
user_drv              <5563.30.0>  user_drv:server/2                  481    0

** Registered ports on node 'bar@127.0.0.1' **
Name                  Id              Command                                 
ok
```

We see here that when a node is *alive* (can communicate with other nodes) the LFE repl prompt contains the name of the node.

This ``c`` function exists as an LFE repl built-in command which can be called from the repl with just ``(regs)``.

-Proctor, Robert

[^1]: The word *reductions* comes from the very early versions of Erlang which were implemented in Prolog which doesn't have function calls but reductions. Basically it is equivalent to the number of function calls the process has made and is a good indicator of how work it has done.
