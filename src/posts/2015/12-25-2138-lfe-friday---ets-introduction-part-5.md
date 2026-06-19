---
layout: post.liquid
title: "LFE Friday - ETS Introduction, part 5: keypos, read_concurrency and write_concurrency"
description: ""
permalink: "/blog/tutorials/2015/12/25/2138-lfe-friday---ets-introduction-part-5"
categories: ["tutorials"]
tags: ["lfe-friday", "lfe", "erlang"]
published_date: 2015-12-25 21:38:00 +0000
is_draft: false
data:
  author: robert-virding
  written_for: null
  last_validated: null
  cover_image: "/images/fridays/LFE_Friday_00283_.png"
  cover_alt: "Vigdís — LFE Friday, LiffyBot studying Erlang modules in a Mead-style command room"
  math: false
---
<a href="/blog/assets/images/posts/LispFlavoredErlang-medium-square.png"><img class="left tiny" src="/blog/assets/images/posts/LispFlavoredErlang-medium-square.png" /></a>This week's LFE Friday was translated with permission from the
[Erlang Thursday](http://www.proctor-it.com/category/erlang/erlang-thursday/)
series by [Steven Proctor](https://twitter.com/stevenproctor).
*This week's translator*: Robert Virding.

Today's Christmas LFE Friday continues the introduction to ETS and <a href="https://lfe.io/blog/tutorials/2015/12/22/0113-lfe-friday---ets-introduction-part-4/">picks up with the promise from last week</a>, and looks at the ``keypos`` ETS table setting, and the *Tweaks* that can be set.

First, we will take a look at the ``keypos`` setting.

The ``keypos`` is the 1-based index in the tuple to be stored that will be used as the key for the entry.  If you remember from the <a href="https://lfe.io/blog/tutorials/2015/12/12/1716-lfe-friday---ets-introduction-part-3/">part 3 of the introduction to ETS about the different table types</a>, they use this index for their key comparison to determine if this is a unique item or not.

If we create a new table without specifying the ``keypos`` option, it defaults to ``1``.

```lfe
> (set table (ets:new 'some-name ()))
8207
> (ets:info table)
(#(read_concurrency false)
 #(write_concurrency false)
 #(compressed false)
 #(memory 305)
 #(owner <0.28.0>)
 #(heir none)
 #(name some-name)
 #(size 0)
 #(node nonode@nohost)
 #(named_table false)
 #(type set)
 #(keypos 1)
 #(protection protected))
```

To show the ``keypos`` in action, we will create a couple of items to insert into our ETS table so we can see the ``keypos`` in action.

```lfe
> (set item-1 #(1 a))
#(1 a)
> (set item-2 #(1.0 "a"))
#(1.0 "a")
> (set item-3 #(1 "one"))
#(1 "one")
> (set item-4 #(a "a"))
#(a "a")
> (set item-5 #("a" a))
#("a" a)
```

In the items above, we have some duplicate entries across both the first item and the second item in the two-tuples.

We will go ahead and insert each one of these items in turn, keeping in mind that this table is a set, so any new insert with the same key, will override the previous value for the same key.

```lfe
> (ets:insert table item-1)
true
> (ets:tab2list table)
(#(1 a))
> (ets:insert table item-2)
true
> (ets:tab2list table)
(#(1 a) #(1.0 "a"))
> (ets:insert table item-3)
true
> (ets:tab2list table)
(#(1 "one") #(1.0 "a"))
> (ets:insert table item-4)
true
> (ets:tab2list table)
(#(1 "one") #(a "a") #(1.0 "a"))
> (ets:insert table item-5)
true
> (ets:tab2list table)
(#("a" a) #(1 "one") #(a "a") #(1.0 "a"))
```

When we added ``item-3`` above, it replaced ``item-1`` in the table, since they both have a ``1`` for the first element in their two-tuple.

We will now create a new table with a ``keypos`` of ``2``, and see how the exact same steps of inserting is changed with a different ``keypos`` value.

```lfe
> (set key-pos-2 (ets:new 'key-pos-2 '(#(keypos 2))))
12304
> (ets:insert key-pos-2 item-1)
true
> (ets:tab2list key-pos-2)
(#(1 a))
> (ets:insert key-pos-2 item-2)
true
> (ets:tab2list key-pos-2)
(#(1.0 "a") #(1 a))
> (ets:insert key-pos-2 item-3)
true
> (ets:tab2list key-pos-2)
(#(1 "one") #(1.0 "a") #(1 a))
> (ets:insert key-pos-2 item-4)
true
> (ets:tab2list key-pos-2)
(#(1 "one") #(a "a") #(1 a))
> (ets:insert key-pos-2 item-5)
true
> (ets:tab2list key-pos-2)
(#(1 "one") #(a "a") #("a" a))
```

In this case, it wasn't until we added ``item-4`` that we had an override, as both ``item-2`` and ``item-4`` both have an ``"a"`` as their second item.  Then we we add ``item-5`` it overwrites the ``item-1``, as they both have the atom ``a`` as their second element.

And if we set a ``keypos`` of some value, say three, and we try to insert a tuple that has fewer items, we will get an exception of type ``badarg``.

```lfe
> (set key-pos-3 (ets:new 'key-pos-3 '(#(keypos 3))))
16401
> (ets:insert key-pos-3 item-1)
exception error: badarg
  in (: ets insert 16401 #(1 a))
```

Now it is time to look at the ``compressed`` option when creating a table.

When creating a new table, the default setting is for it to be uncompressed, as we can see in the table info since it shows ``#(compressed false)``.

```lfe
> (set uncompressed-table (ets:new 'uc ()))
20495
> (ets:info uncompressed-table)
(#(read_concurrency false)
 #(write_concurrency false)
 #(compressed false)
 #(memory 305)
 #(owner <0.64.0>)
 #(heir none)
 #(name uc)
 #(size 0)
 #(node nonode@nohost)
 #(named_table false)
 #(type set)
 #(keypos 1)
 #(protection protected))
```

We create a new table, with the ``compressed`` option, and when we look at ``ets:info/1`` for the table, we see that it shows ``#(compressed true)``.

```lfe
> (set compressed-table (ets:new 'c '(compressed)))
28689
> (ets:info compressed-table)
(#(read_concurrency false)
 #(write_concurrency false)
 #(compressed true)
 #(memory 305)
 #(owner <0.64.0>)
 #(heir none)
 #(name c)
 #(size 0)
 #(node nonode@nohost)
 #(named_table false)
 #(type set)
 #(keypos 1)
 #(protection protected))
```

``compressed``, according to the documentation at least, says that it stores the data in a "more compact format to consume less memory".  It also warns that this can this can make operations that need to check the entire tuple slower, and that the key is not stored compressed, at least in the _current_ implementation.

So let's see what kind of memory difference ``compressed`` makes.

To start with, we will insert 100_000 items into our ETS tables and see what the resulting memory size becomes.  We will insert a new tuple of ``#(x x)``, for all numbers from 1 to 100_000.

```lfe
> (lists:foreach (lambda (x) (ets:insert compressed-table (tuple x x))) (lists:seq 1 100000))
ok
> (lists:foreach (lambda (x) (ets:insert uncompressed-table (tuple x x))) (lists:seq 1 100000))
ok
> (ets:info compressed-table)
(#(read_concurrency false)
 #(write_concurrency false)
 #(compressed true)
 #(memory 814643)
 #(owner <0.64.0>)
 #(heir none)
 #(name c)
 #(size 100000)
 #(node nonode@nohost)
 #(named_table false)
 #(type set)
 #(keypos 1)
 #(protection protected))
> (ets:info uncompressed-table)
(#(read_concurrency false)
 #(write_concurrency false)
 #(compressed false)
 #(memory 714643)
 #(owner <0.64.0>)
 #(heir none)
 #(name uc)
 #(size 100000)
 #(node nonode@nohost)
 #(named_table false)
 #(type set)
 #(keypos 1)
 #(protection protected))
```

Interesting.

For the compressed table the memory is reported to be ``814643``, but the uncompressed shows the memory to be less than that with ``714643``.

Maybe it doesn't like to compact integer values very much, so let's do the same thing, but use a string for the second item in the tuple.

```lfe
> (lists:foreach (lambda (x) (ets:insert compressed-table (tuple x (integer_to_list x)))) (lists:seq 1 100000))
ok
> (lists:foreach (lambda (x) (ets:insert uncompressed-table (tuple x (integer_to_list x)))) (lists:seq 1 100000))
ok
> (ets:info compressed-table)
(#(read_concurrency false)
 #(write_concurrency false)
 #(compressed true)
 #(memory 914644)
 #(owner <0.64.0>)
 #(heir none)
 #(name c)
 #(size 100000)
 #(node nonode@nohost)
 #(named_table false)
 #(type set)
 #(keypos 1)
 #(protection protected))
> (ets:info uncompressed-table)
(#(read_concurrency false)
 #(write_concurrency false)
 #(compressed false)
 #(memory 1692433)
 #(owner <0.64.0>)
 #(heir none)
 #(name uc)
 #(size 100000)
 #(node nonode@nohost)
 #(named_table false)
 #(type set)
 #(keypos 1)
 #(protection protected))
```

Now using strings in our tuples instead of just using integers, we can see that the compressed ETS table memory is ``914644``, where as the uncompressed ETS table's memory is ``1692433``.

So in addition to thinking about the way you are going to be matching on the data when trying to determine if the table should be compressed, it looks like you also need to think about the type of data you are going to be putting into the ETS table.

The last two options to be discussed are ``read_concurrency`` and ``write_concurrency``.

``read_concurrency`` is by default set to ``false``, and, according to the documentation is best for when "read operations are much more frequent than write operations, or when concurrent reads and writes comes in large read and write bursts".

So if you have a table that has a bunch of reads with the writes infrequently interspersed between the reads, this would be when you would want to enable ``read_concurrency``, as the documentation states that switching between reads and writes is more expensive.

The ``write_concurrency`` option is set to ``false`` by default, causing any additional concurrent writes to block while an write operation is proceeding.  When set to ``true`` different tuples of the same table can be written to by concurrent processes, and does not affect any table of the type ``ordered_set``.

This _should_ be it as far as the introduction goes.  Next week we will start looking at the different operations we can perform using ETS and ETS tables.

\- Proctor, Robert
