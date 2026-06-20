---
layout: post.liquid
title: "LFE Friday - ETS Introduction, part 3: ETS Table Types"
description: ""
permalink: "/blog/tutorials/2015/12/12/1716-lfe-friday---ets-introduction-part-3"
categories: ["tutorials"]
tags: ["lfe-friday", "lfe", "erlang"]
published_date: 2015-12-12 17:16:00 +0000
is_draft: false
data:
  author: robert-virding
  written_for:
    lfe: "0.10"
    erlang: "18"
  last_validated: null
  cover_image: "/images/fridays/LFE_Friday_00280_.png"
  cover_alt: "Vigdís — LFE Friday, LiffyBot studying Erlang modules in a Mead-style command room"
  math: false
---
<a href="/blog/assets/images/posts/LispFlavoredErlang-medium-square.png"><img class="left tiny" src="/blog/assets/images/posts/LispFlavoredErlang-medium-square.png" /></a>This week's LFE Friday was translated with permission from the
[Erlang Thursday](http://www.proctor-it.com/category/erlang/erlang-thursday/)
series by [Steven Proctor](https://twitter.com/stevenproctor).
*This week's translator*: Robert Virding.

Today's LFE Friday continues the introduction to ETS and takes a look at the different types of storage strategies that ETS supports.

The different types that ETS supports are: ``set``, ``ordered_set``, ``bag``, and ``duplicate bag``.

Each of these different types can be passed in when creating a new ETS table, but let's see what type of ETS table we get when we don't specify any of the types.

```lfe
> (set ets-empty (ets:new 'ets-empty ()))
8207
> (ets:info ets-empty)
(#(read_concurrency false)
 #(write_concurrency false)
 #(compressed false)
 #(memory 305)
 #(owner <0.28.0>)
 #(heir none)
 #(name ets-empty)
 #(size 0)
 #(node nonode@nohost)
 #(named_table false)
 #(type set)
 #(keypos 1)
 #(protection protected))
```

If we look above, we can see the ``type`` tagged tuple has the type of ``set``. We also see that the ``keypos`` tagged tuple has the value ``1`` which tells us that it is the first element of the tuples which are the key.

To see how the different types can work we will create three tuples to add to the ETS tables of the different type to see what they store.

```lfe
> (set item-1 #(1 a))
#(1 a)
> (set item-2 #(1.0 "a"))
#(1.0 "a")
> (set item-3 #(1 "one"))
#(1 "one")
```

We will have a two tuples with the first element, the key, of ``1``, and one tuple whose first element is ``1.0`` to see how the different types of ETS tables behave when given the "same" key.

Why have key of both ``1`` and ``1.0``?  Because depending on the comparison of equality used, they may or may not be seen as the same, and therefore the same key.

```lfe
> (== 1 1.0)
true
> (=:= 1 1.0)
false
```

First we will take a look at an ETS table of type ``set``.

```lfe
> (set ets-set (ets:new 'ets-set '(set)))
12304
```

We insert ``item-1`` followed by an insert of ``item-2``, and use ``ets:tab2list/1`` to see what is stored in the ETS table.

```lfe
> (ets:insert ets-set item-1)
true
> (ets:insert ets-set item-2)
true
> (ets:tab2list ets-set)
(#(1 a) #(1.0 "a"))
```

An ETS table of type set sees ``1`` and ``1.0`` as different keys.  So now let's add ``item-3`` and see what happens when we do an insert with an already existing key.

```lfe
> (ets:insert ets-set item-3)
true
> (ets:tab2list ets-set)
(#(1 "one") #(1.0 "a"))
```

The previous tuple with the key of ``1`` was replaced by the tuple for ``item-3`` which is the last thing we inserted.

Let's look at what an ``ordered_set`` does.

```lfe
> (set ets-ordset (ets:new 'ets-ordset '(ordered_set)))
16401
```

Again we'll insert `item-1`` followed by ``item-2`` and use ``ets:tab2list/1`` to check it's state.

```lfe
> (ets:insert ets-ordset item-1)
true
> (ets:insert ets-ordset item-2)
true
> (ets:tab2list ets-ordset)
(#(1.0 "a"))
```

In this case, the key of ``1.0`` was seen the same as the previous ``1`` that was in there, so it overwrites the first item inserted.

We insert ``item-3`` to the ``ordered_set``, and we can see it gets replaced yet again.

```lfe
> (ets:insert ets-ordset item-3)
true
> (ets:tab2list ets-ordset)
(#(1 "one"))
```

Now lets check an ETS table that is a ``bag``.

```lfe
> (set ets-bag (ets:new 'ets-bag '(bag)))
20498
```

And we yet again add ``item-1`` and ``item-2`` to the table.

```lfe
> (ets:insert ets-bag item-1)
true
> (ets:insert ets-bag item-2)
true
> (ets:tab2list ets-bag)
(#(1 a) #(1.0 "a"))
```

Looking at ``ets:tab2list/1``, we can see that for a bag they are treated as two different items.

And again we will see what happens when we insert ``item-3`` into this ETS table.

```lfe
> (ets:insert ets-bag item-3)
true
> (ets:tab2list ets-bag)
(#(1 a) #(1 "one") #(1.0 "a"))
```

In the case of a ``bag`` type of ETS table, we have ``item-2`` along with entries ``item-1`` *and* ``item-3`` even though ``item-1`` and ``item-3`` both have the same key.

The last type of ETS table we have is a ``duplicate_bag``.

```lfe
> (set ets-dupbag (ets:new 'ets-dupbag '(duplicate_bag)))
24595
```

We insert ``item-1`` followed by ``item-2`` as we did with all of the other types of ETS tables.

```lfe
> (ets:insert ets-dupbag item-1)
true
> (ets:insert ets-dupbag item-2)
true
> (ets:tab2list ets-dupbag)
(#(1 a) #(1.0 "a"))
```

And like all of the other ETS table types, we insert ``item-3`` into the `duplicate_bag` ETS table type.

```lfe
> (ets:insert ets-dupbag item-3)
true
> (ets:tab2list ets-dupbag)
(#(1 a) #(1 "one") #(1.0 "a"))
```

And we see we have all three items in the ETS table for a ``duplicate_bag`` type.

If we look at the behavior of ``bag`` and ``duplicate_bag`` though, we see that the behavior of both seems to be the same.

So what is the difference between the two???

If you dig into the documentation, and look at the description of the types under [ets:new/2](http://erlang.org/doc/man/ets.html#new-2), it says that a ``bag`` will allow duplicate keys, but allow the item to only be added once, a ``duplicate_bag`` will allow multiple entries even if they have the same values as well.

To see this in action, we will add ``item-1`` to both the ``ets-bag`` table and the ``ets-dupbag`` table and see what happens.

First with just the ETS ``bag`` type.

```lfe
> (ets:insert ets-bag item-1)
true
> (ets:tab2list ets-bag)
(#(1 a) #(1 "one") #(1.0 "a"))
```

The return value is the same as it was before, so adding an item that is already in a ETS table of type ``bag`` will not add it again.

So what does the ``duplicate_bag`` type of ETS table do?

```lfe
> (ets:insert ets-dupbag item-1)
true
> (ets:tab2list ets-dupbag)
(#(1 a) #(1 "one") #(1 a) #(1.0 "a"))
```

And we can see the tuple ``#(1 a)`` shows up twice, because we called ``ets:insert/2`` with that value twice.

\- Proctor, Robert
