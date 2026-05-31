---
layout: post.liquid
title: "LFE Friday - ETS Introduction, part 4: ETS Access Protections"
description: ""
permalink: "/blog/tutorials/2015/12/22/0113-lfe-friday---ets-introduction-part-4"
categories: ["tutorials"]
tags: ["lfe friday", "lfe", "erlang"]
published_date: 2015-12-22 01:13:00 +0000
is_draft: false
data:
  author: robert-virding
  written_for: null
  last_validated: null
  cover_image: null
  cover_alt: null
  math: false
---
<a href="/blog/assets/images/posts/LispFlavoredErlang-medium-square.png"><img class="left tiny" src="/blog/assets/images/posts/LispFlavoredErlang-medium-square.png" /></a>This week's LFE Friday was translated with permission from the
[Erlang Thursday](http://www.proctor-it.com/category/erlang/erlang-thursday/)
series by [Steven Proctor](https://twitter.com/stevenproctor).
*This week's translator*: Robert Virding.

Today's LFE Friday continues the introduction to ETS and takes a look at the different access levels that ETS supports.

The different access levels that ETS supports are: ``public``, ``protected``, and ``private``.

Each of these different types can be passed in when creating a new ETS table, but let's see what type of ETS table we get when we don't specify an access level.

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

So the default access level is ``protected`` when not specified.

So what does it mean for a ETS table to be ``protected`` then?  The documentation states that ``protected`` tables can be written to by only the owning process, but read by other processes.

So let's see that at work then.

First let's create a process that we can give ETS tables away to.

```lfe
> (set fun (lambda () (receive (after 'infinity 'ok))))
#Fun<lfe_eval.23.88887576>
> (set some-process (spawn fun))
<0.36.0>
```

We create a new ETS table and specify it is ``protected``, and we also specify that it is a ``named_table`` as a bonus.

```lfe
> (ets:new 'protected-named-ets '(protected named_table))
protected-named-ets
```

The result of that evaluation is ``protected-named-ets`` and not a number like the call to ``ets:new/2`` above, so we should be able to use the name of the table to access the table instead of just the identifier.

We will insert an entry into the ETS table, and we will use the name of the ETS table as the ETS table reference since we said the table is a ``named_table``.

```lfe
> (ets:insert 'protected-named-ets #(foobar baz))
true
```

``ets:insert/2`` returned ``true`` so we should now have some data in the table.  Let's pull it out using ``ets:match/2``, and let's match everything while we are at it by using a ``$1`` for the pattern.

```lfe
> (ets:match 'protected-named-ets '$1)
((#(foobar baz)))
```

So as the owner process of the ETS table, since this was the process that created it, we can read an write to the table.

Now time to give our table away.

```lfe
> (ets:give_away 'protected-named-ets some-process ())
true
```

Since the documentation says is is available for reads, we will do the same ``match`` we just did before giving it away.

```lfe
> (ets:match 'protected-named-ets '$1)
((#(foobar baz)))
```

We get our results back.

What does a write look like then, since it says only the owning process has access to write, and the return value of calling ``ets:insert/2`` is always ``true``.

```lfe
> (ets:insert 'protected-named-ets #(barbaz foo))
exception error: badarg
  in (: ets insert protected-named-ets #(barbaz foo))
```

An exception, and it is of type ``badarg``, which does hold that it doesn't allow writes from non-owning processes, but doesn't exactly make it clear that is what is happening.

How about if we see what we get if we try to call ``ets:insert/2`` on a table that doesn't exist?

```lfe
> (ets:insert 'no-such-table #(foo bar))
exception error: badarg
  in (: ets insert no-such-table #(foo bar))
```

Same exception and same format of the error with just the name of the table and the tuple being different.

Thinking about this some, it does make sense that these two difference cases would be the same error.  As far as the inserting process knows, there is no such table when trying to do an insert if no table exists, or if it is set to be ``protected``.  Either way, the caller passed in a bad ETS table reference for the call to ``ets:insert/2``.

So we have now seen how ``protected`` behaves, which is the default access level, so let's take a look at ``public`` next.

```lfe
(ets:new 'public-named-ets '(public named_table))
public-named-ets
```

We will do an insert and a match from our current process, which is the owner.

```lfe
> (ets:insert 'public-named-ets #(foo bar))
true
> (ets:match 'public-named-ets '$1)
((#(foo bar)))
```

All looks good there.

The documentation states that ``public`` allows any process to read from and write to the table, so let's give the public table away to ``some-process`` and try to read and write.

```lfe
> (ets:give_away 'public-named-ets some-process ())
true
```

Now that we have given it away, time to try to add a new entry to the table, and see if we can read that write back out.

```lfe
> (ets:insert 'public-named-ets #(bar baz))
true
> (ets:match 'public-named-ets '$1)
((#(foo bar)) (#(bar baz)))
```

There we go.  We have just inserted new data into that table, and when we do the ``ets:match/2`` on everything, we see the new data in the result.

Now let's create a ``private`` table.  The documentation states that for `private` ETS tables, only the owner is allowed to read or write to the ETS table.

```lfe
> (ets:new 'private-named-ets '(private named_table))
private-named-ets
```

Again, while this process still owns the table, we will add an item and do a read from the table.

```lfe
> (ets:insert 'private-named-ets #(fizz buzz))
true
> (ets:match 'private-named-ets '$1)
((#(fizz buzz)))
```

Time to give this table away to ``some-process`` again.

```lfe
> (ets:give_away 'private-named-ets some-process ())
true
```

Now that the ETS table is owned by a different process, time to try a read.

```lfe
> (ets:match 'private-named-ets '$1)
exception error: badarg
  in (: ets match private-named-ets $1)
```

``badarg`` exception, just like the attempted ``ets:insert/2`` we tried on the protected ETS table above when it was owned by a different process.

And time for a write.

```lfe
> (ets:insert 'private-named-ets #(buzz fizz)) '#))
exception error: badarg
  in (: ets insert private-named-ets #(buzz fizz))
```

A ``badarg`` exception here as well, which should not be a surprise at this point, as both the protected write, and this private read both raised that same exception.

So in total, for this introduction so far, we have seen the Type, Access, Named Table, Heir, and Owner settings of an ETS table, and how they relate.

Next week, we will conclude the introduction of ETS by going over the **Key Position** option and the **Tweaks** that an ETS table can take when being setup.

\- Proctor, Robert
