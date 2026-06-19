---
layout: post.liquid
title: "LFE Friday - ETS Introduction, part 1"
description: ""
permalink: "/blog/tutorials/2015/11/28/1721-lfe-friday---ets-introduction-part-1"
categories: [tutorials]
tags: [lfe-friday, lfe, erlang]
published_date: 2015-11-28 17:21:00 +0000
is_draft: false
data:
  author: robert-virding
  written_for:
    lfe: "0.10"
    erlang: "18"
  last_validated: null
  cover_image: "/images/fridays/LFE_Friday_00283_.png"
  cover_alt: "Vigdís — LFE Friday, LiffyBot studying Erlang modules in a Mead-style command room"
  math: false
---

<a href="/blog/assets/images/posts/LispFlavoredErlang-medium-square.png"><img class="left tiny" src="/blog/assets/images/posts/LispFlavoredErlang-medium-square.png" /></a>This week's LFE Friday was translated with permission from the
[Erlang Thursday](http://www.proctor-it.com/category/erlang/erlang-thursday/)
series by [Steven Proctor](https://twitter.com/stevenproctor).
*This week's translator*: Robert Virding.

Today's LFE Friday starts the beginning of an intro to the [ets](http://www.erlang.org/doc/man/ets.html) module, and ETS in general.

ETS stands for Erlang Term Storage, and is a in-memory store for Erlang/LFE terms, a.k.a pieces of an Erlang/LFE data type, that provides constant access time to the data stored.

ETS can be thought of as a key/value store style storage, and it uses the concept of tables as the way of grouping together data.

One of the first things that is useful to know is that ETS tables are created by a process which, unless transferred to another process, is the owner of the table.

When the owner dies, the table gets deleted, and is no longer accessible.

Let's see what this would look like.

First, after starting a new LFE shell, we will check the PID (process identifier) of the shell we are in.

```lfe
> (self)
<0.28.0>
```

We then will create a new ETS table.  We will be going into future details about the various ways new tables can be created in future posts, so for now, we will just create a new table by only specifying a name and empty list of options.

```lfe
> (set table-id (ets:new 'table ()))
8207
```

Capturing table id, we will take a look at the info that ETS knows about that table with ``ets:info/1``.

```lfe
> (ets:info table-id)
(#(read_concurrency false)
 #(write_concurrency false)
 #(compressed false)
 #(memory 305)
 #(owner <0.28.0>)
 #(heir none)
 #(name table)
 #(size 0)
 #(node nonode@nohost)
 #(named_table false)
 #(type set)
 #(keypos 1)
 #(protection protected))
```

We see in the table info that the shell process, ``<0.28.0>``, is the "owner" of the table. Time to cause the owning process to crash.  In this case we'll do a bad pattern match to cause a bad match exception.

```lfe
> (set 1 2)
exception error: #(badmatch 2)
```

And let's check the PID of the process to double check that the shell has indeed started a new process for us to run in.

```lfe
> (self)
<0.38.0>
```

And yes, the PID ``(self)`` returned is different than the PID we got when we called ``(self)`` the first time.

Time to look at the info for the table we created earlier again and see what we get.

```lfe
> (ets:info table-id)
undefined
```

``undefined``.  So we no longer have any table found by ETS for that table id.

We take a secondary look using ``ets:all/0`` to see if we can see if it might be floating around somewhere still but the call to ``ets:info/1`` is just not returning for the table id.

```lfe
> (ets:all)
(file_io_servers inet_hosts_file_byaddr inet_hosts_file_byname
 inet_hosts_byaddr inet_hosts_byname inet_cache inet_db
 global_pid_ids global_pid_names global_names_ext global_names
 global_locks 4098 1 ac_tab)
```

Doesn't look like it, so let's create another table with the same table name as before.

```lfe
> (set table-id-2 (ets:new 'table ()))
12303
```

That succeeds and doesn't complain about trying to create a table with the same name as an existing table.

We will call ``ets:all/0`` again, and we can see there is an item in the list with the id that was returned from ``ets:new/2``.

```lfe
> (ets:all)                           
(12303 file_io_servers inet_hosts_file_byaddr inet_hosts_file_byname
 inet_hosts_byaddr inet_hosts_byname inet_cache inet_db
 global_pid_ids global_pid_names global_names_ext global_names
 global_locks 4098 1 ac_tab)
```

Time to crash the process again.

```lfe
> (set 1 2)                           
exception error: #(badmatch 2)
```

We note that we do have a new PID again.

```lfe
> (self)                              
<0.45.0>
```

And if we call ``ets:all/0`` one more time, we can see that the table identifier that was previously in the list has gone away.

```lfe
> (ets:all)
(file_io_servers inet_hosts_file_byaddr inet_hosts_file_byname
 inet_hosts_byaddr inet_hosts_byname inet_cache inet_db
 global_pid_ids global_pid_names global_names_ext global_names
 global_locks 4098 1 ac_tab)
```

So with this initial look at ETS, we have demonstrated an owning process crash does remove the table, and we have also gotten an preview of a couple of the functions in the ``ets`` module, specifically ``ets:new/2``, ``ets:info/1``, and ``ets:all/0``.

We will continue looking at the overview of ETS for a few posts, while doing some cursory coverage of some of the functions in the ``ets`` module, and after that, we will then start to get into the specifics of the different functions in the ``ets`` module.

\- Proctor, Robert
