---
layout: post.liquid
title: "LFE Friday - c:i/0"
description: ""
permalink: "/blog/tutorials/2015/08/22/1310-lfe-friday---ci0"
categories: ["tutorials"]
tags: ["lfe-friday", "lfe", "erlang"]
published_date: 2015-08-22 13:10:00 +0000
is_draft: false
data:
  author: robert-virding
  written_for: null
  last_validated: null
  cover_image: "/images/fridays/LFE_Friday_00277_.png"
  cover_alt: "Vigdís — LFE Friday, LiffyBot studying Erlang modules in a Mead-style command room"
  math: false
---
<a href="/blog/assets/images/posts/LispFlavoredErlang-medium-square.png"><img class="left tiny" src="/blog/assets/images/posts/LispFlavoredErlang-medium-square.png" /></a>This week's LFE Friday was translated with permission from the
[Erlang Thursday](http://www.proctor-it.com/category/erlang/erlang-thursday/)
series by [Steven Proctor](https://twitter.com/stevenproctor).
*This week's translator*: Robert Virding.

Today's LFE Friday continues with another function in the ``c`` module, [c:i/0](http://erlang.org/doc/man/c.html#i-0).

``c:i/0`` reports the system information, displaying information about all the processes on a given node.

```lfe
> (c:i)
Pid                   Initial Call                          Heap     Reds Msgs
Registered            Current Function                     Stack              
<0.0.0>               otp_ring0:start/2                     1598     2392    0
init                  init:loop/1                              2              
<0.3.0>               erlang:apply/2                        6772   176779    0
erl_prim_loader       erl_prim_loader:loop/3                   6              
<0.6.0>               gen_event:init_it/6                    376      223    0
error_logger          gen_event:fetch_msg/5                    8              
<0.7.0>               erlang:apply/2                         987      404    0
application_controlle gen_server:loop/6                        7              
<0.9.0>               application_master:init/4              376       44    0
                      application_master:main_loop/2           6              
<0.10.0>              application_master:start_it/4          233       73    0
                      application_master:loop_it/4             5              
<0.11.0>              supervisor:kernel/1                    376     1891    0
kernel_sup            gen_server:loop/6                        9              
<0.12.0>              erlang:apply/2                        6772   129164    0
code_server           code_server:loop/1                       3              
<0.14.0>              rpc:init/1                             233       21    0
rex                   gen_server:loop/6                        9              
<0.15.0>              global:init/1                          233       45    0
global_name_server    gen_server:loop/6                        9              
<0.16.0>              erlang:apply/2                         233       21    0
                      global:loop_the_locker/1                 5              
<0.17.0>              erlang:apply/2                         233        3    0
                      global:loop_the_registrar/0              2              
<0.18.0>              inet_db:init/1                         376      236    0
inet_db               gen_server:loop/6                        9              
<0.19.0>              global_group:init/1                    233       53    0
global_group          gen_server:loop/6                        9              
<0.20.0>              file_server:init/1                     233       84    0
file_server_2         gen_server:loop/6                        9              
<0.21.0>              supervisor_bridge:standard_error/      233       34    0
standard_error_sup    gen_server:loop/6                        9              
<0.22.0>              erlang:apply/2                         233        9    0
standard_error        standard_error:server_loop/1             2              
<0.23.0>              supervisor_bridge:user_sup/1           233       69    0
                      gen_server:loop/6                        9              
<0.24.0>              user_drv:server/2                     1598     2064    0
user_drv              user_drv:server_loop/6                   9              
<0.25.0>              group:server/3                         233       36    0
user                  group:server_loop/3                      4              
<0.26.0>              group:server/3                         987    11825    0
                      group:server_loop/3                      4              
<0.27.0>              erlang:apply/2                         233      360    0
                      lfe_shell:shell_eval/3                   5              
<0.28.0>              erlang:apply/2                        6772    17553    0
                      c:pinfo/1                               38              
<0.30.0>              kernel_config:init/1                   233      233    0
                      gen_server:loop/6                        9              
<0.31.0>              supervisor:kernel/1                    233       56    0
kernel_safe_sup       gen_server:loop/6                        9              
Total                                                      30252   343672    0
                                                             196              
ok
```

We can see that it returns the process id (pid), the initial function that started the process, the size of the heap, the number of reductions, the number of messages in the message queue, the registered name, the current function the process is in, and the stack size.

``c:i/0`` also includes a total of the total heap size, reductions, message queue size and stack size.

There ``c`` module also includes a [c:ni/0](http://erlang.org/doc/man/c.html#ni-0), that reports the system information above across all nodes.

Looking at the process info we see a process related to the LFE repl ``lfe_shell``. We can grab the pid of that process by calling [c:i/3](http://erlang.org/doc/man/c.html#i-3) which displays information about a process but can reference a pid by the three integer values that make up the process's pid.

```lfe
> (c:i 0 27 0)
(#(current_function #(lfe_shell shell_eval 3))
 #(initial_call #(erlang apply 2))
 #(status waiting)
 #(message_queue_len 0)
 #(messages ())
 #(links (<0.28.0> <0.26.0>))
 #(dictionary ())
 #(trap_exit true)
 #(error_handler error_handler)
 #(priority normal)
 #(group_leader <0.26.0>)
 #(total_heap_size 11334)
 #(heap_size 10958)
 #(stack_size 5)
 #(reductions 1576)
 #(garbage_collection
   (#(min_bin_vheap_size 46422)
    #(min_heap_size 233)
    #(fullsweep_after 65535)
    #(minor_gcs 6)))
 #(suspending ()))
```

When looking at the information related to a specific process, we can see it shows the linked process, messages and message queue length, heap and stack information, ad various other settings that could be of use.

Note that the ``c:i/0`` function prints out the data so it is intended to be called from the shell and not from another function. While the output is not as pretty as the ``observer`` application this function is very useful when you only have access to through a terminal.

The ``c:i/0`` function gets the information about a process using the ``BIF``s [process_info/1](http://erlang.org/doc/man/erlang.html#process_info-1) and [process_info/2](http://erlang.org/doc/man/erlang.html#process_info-2). These functions are very useful when need to access the process information from within a function or application.

```lfe
> (process_info (pid 0 27 0))
(#(current_function #(lfe_shell shell_eval 3))
 #(initial_call #(erlang apply 2))
 #(status waiting)
 #(message_queue_len 0)
 #(messages ())
 #(links (<0.38.0> <0.26.0>))
 #(dictionary ())
 #(trap_exit true)
 #(error_handler error_handler)
 #(priority normal)
 #(group_leader <0.26.0>)
 #(total_heap_size 57380)
 #(heap_size 28690)
 #(stack_size 5)
 #(reductions 3770)
 #(garbage_collection
   (#(min_bin_vheap_size 46422)
    #(min_heap_size 233)
    #(fullsweep_after 65535)
    #(minor_gcs 1)))
 #(suspending ()))
```

As with the ``c`` functions [c:m/1](https://lfe.io/blog/tutorials/2015/08/10/0123-lfe-friday---cm1/) and [c:regs/0](https://lfe.io/blog/tutorials/2015/08/15/0226-lfe-friday---cregs0/) this function exists as an LFE repl built-in command which can be called from the repl with just ``(i)``.

-Proctor, Robert
