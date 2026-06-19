---
layout: post.liquid
title: "LFE Friday - The digraph module"
description: ""
permalink: "/blog/tutorials/2015/10/18/1701-lfe-friday---the-digraph-module"
categories: ["tutorials"]
tags: ["lfe-friday", "lfe", "erlang"]
published_date: 2015-10-18 17:01:00 +0000
is_draft: false
data:
  author: robert-virding
  written_for: null
  last_validated: null
  cover_image: "/images/fridays/LFE_Friday_00278_.png"
  cover_alt: "Vigdís — LFE Friday, LiffyBot studying Erlang modules in a Mead-style command room"
  math: false
---
<a href="/blog/assets/images/posts/LispFlavoredErlang-medium-square.png"><img class="left tiny" src="/blog/assets/images/posts/LispFlavoredErlang-medium-square.png" /></a>This week's LFE Friday was translated with permission from the
[Erlang Thursday](http://www.proctor-it.com/category/erlang/erlang-thursday/)
series by [Steven Proctor](https://twitter.com/stevenproctor).
*This week's translator*: Robert Virding.

Today's LFE Friday kicks of taking a look at the [digraph](http://erlang.org/doc/man/digraph.html) module.

As I was looking into it, this module didn't line up with my expectations of Erlang behavior, so I want to focus on that difference before taking a look at the functions in the module.

If we start by browsing through the function signatures in the ``digraph`` module, we can see that the only function that returns a ``digraph()`` are ``digraph:new/0`` and ``digraph:new/1``.

Thinking this was odd for a Erlang API, I went to the LFE shell, and added a vertex to the `digraph()`, and then inspected the result of that operation.

```lfe
> (set g (digraph:new))
#(digraph 8207 12304 16401 true)
> (set g2 (digraph:add_vertex g 'foo 'bar))
foo
```

The return value of calling ``digraph:add_vertex/3`` was ``foo``, which was the second argument, and doesn't match up with what the representation of a graph looks like.

Okay, time to look at the ``digraph()`` in `g` again then to see if that changed.

```lfe
> g
#(digraph 8207 12304 16401 true)
```

That tuple result of the `digraph()` looks the same, so let's see if that vertex we added is in the graph, since we did get the return value of `foo`.

```lfe
> (digraph:vertices g)
(foo)
> (digraph:vertex g 'foo)
#(foo bar)
```

Hrmm... Okay, looks like that vertex is in there.

Let's add another vertex to the ``digraph()`` bound to `g`.

```lfe
> (set v (digraph:add_vertex g))
($v . 0)
> (digraph:vertices g)          
(foo ($v . 0))
```

That one is added as well.

## HC SVNT DRACONES (Here Be Dragons)

So the behavior I want to call out in this post before we start looking at the functions in this module is that these functions exhibit observably mutable behavior on a ``digraph()``.

I say it is observably mutable, because while if it is not being changed under the covers of the implementation, the structure can be changed while the binding of the variable to the reference stays the same.

```lfe
> (digraph:vertices g)          
(foo ($v . 0))
> (set copy g)
#(digraph 8207 12304 16401 true)
> (set v2 (digraph:add_vertex g 'wat))
wat
> (digraph:vertices copy)             
(foo ($v . 0) wat)
> (digraph:vertices g)   
(foo ($v . 0) wat)
```

This even mutates other variable references as well, so this breaks any convention that I have seen in the Erlang ecosystem about keeping all data immutable.

We will continue looking at the ``digraph`` module in future [LFE Friday](https://lfe.io/blog/) posts, but I wanted to spend some time calling out the mutability inherent in the ``digraph()``s, so that when you need to use a one, you can be aware that this is not something you want to use in your concurrent parts of your application without great caution.

## The Dragons slain

The reason behind the dragons is how a ``digraph()`` is implemented. A ``digraph`` is built of 3 [ETS](http://erlang.org/doc/man/ets.html) tables, with, in this case, the table ids ``8207``, ``12304`` and ``16401``. You can see this by calling [ets:i/0](http://erlang.org/doc/man/ets.html#i-0) which lists information about all the current tables. You can see that the 3 tables are owned by the LFE shell process:

```lfe
> (self)
<0.28.0>
> (ets:i)
 id              name              type  size   mem      owner
 ----------------------------------------------------------------------------
 1               code              set   282    10393    code_server
 4098            code_names        set   64     7713     code_server
 8207            vertices          set   3      328      <0.28.0>
 12304           edges             set   0      305      <0.28.0>
 16401           neighbours        bag   2      319      <0.28.0>
 ac_tab          ac_tab            set   6      839      application_controller
 file_io_servers file_io_servers   set   0      305      file_server_2
 global_locks    global_locks      set   0      305      global_name_server
 global_names    global_names      set   0      305      global_name_server
...
 
ok
```

The ``digraph()`` structure itself is just a tagged tuple containing the table ids. As all changes are made to the ``ETS`` tables the structure itself never changes. Data about the tables and their contents can be read with [ets:info/1](http://erlang.org/doc/man/ets.html#info-1) and [ets:i/1](http://erlang.org/doc/man/ets.html#i-1).

\- Proctor, Robert
