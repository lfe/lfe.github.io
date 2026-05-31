---
layout: post.liquid
title: "LFE Friday - ETS selects, continuations and inserts"
description: ""
permalink: "/blog/tutorials/2016/01/31/0030-lfe-friday---ets-selects-continuations-and-inserts"
categories: ["tutorials"]
tags: ["lfe friday", "lfe", "erlang"]
published_date: 2016-01-31 00:30:00 +0000
is_draft: false
data:
  author: robert-virding
  written_for: null
  last_validated: null
  cover_image: "/images/fridays/LFE_Friday_00272_.png"
  cover_alt: "Vigdís — LFE Friday, LiffyBot studying Erlang modules in a Mead-style command room"
  math: false
---
<a href="/blog/assets/images/posts/LispFlavoredErlang-medium-square.png"><img class="left tiny" src="/blog/assets/images/posts/LispFlavoredErlang-medium-square.png" /></a>This week's LFE Friday was translated with permission from the
[Erlang Thursday](http://www.proctor-it.com/category/erlang/erlang-thursday/)
series by [Steven Proctor](https://twitter.com/stevenproctor).
*This week's translator*: Robert Virding.

At the end of [last week's LFE Friday](http://blog.lfe.io/blog/tutorials/2016/01/23/0122-lfe-friday---using-ets-select-with-a-limit/), we said we would continue looking at the behavior of the ``select`` functions in the ``ets`` module.

So before we do any experimentation, we setup our test ETS tables, and this time we will also create a table of type ``ordered_set``.

```lfe
> (set fun (lambda () (receive (after 'infinity 'ok))))
#Fun<lfe_eval.23.76437303>
> (set some-process (spawn fun))
<0.36.0>
> (set set-table (ets:new 'set-table '(public set)))
8207
> (set ordered-set-table (ets:new 'ordered-set-table '(public ordered_set)))
12304
> (ets:give_away set-table some-process ())
true
> (ets:give_away ordered-set-table some-process ())
true
```

Next we will load our test ETS table with some dummy data, leaving some gaps in the sequence, allowing us to fill those gaps in later.

```lfe
> (list-comp ((<- x (lists:seq 1 30 2))) (ets:insert set-table (tuple x x)))
(true true true true true true true true true true true true true true true)
> (list-comp ((<- x (lists:seq 1 30 2))) (ets:insert ordered-set-table (tuple x x)))
(true true true true true true true true true true true true true true true)
```

We then do a select to get all of the records from the table so we can see how the results are ordered for the different table types.

```lfe
> (ets:select set-table '(#(#($1 $2) () (#(#($1 $2))))))
(#(15 15)
 #(25 25)
 #(13 13)
 #(21 21)
 #(11 11)
 #(1 1)
 #(23 23)
 #(7 7)
 #(3 3)
 #(9 9)
 #(19 19)
 #(29 29)
 #(27 27)
 #(17 17)
 #(5 5))
> (ets:select ordered-set-table '(#(#($1 $2) () (#(#($1 $2))))))
(#(1 1)
 #(3 3)
 #(5 5)
 #(7 7)
 #(9 9)
 #(11 11)
 #(13 13)
 #(15 15)
 #(17 17)
 #(19 19)
 #(21 21)
 #(23 23)
 #(25 25)
 #(27 27)
 #(29 29))
```

The ``ets`` module also has a function [ets:select_reverse](http://erlang.org/doc/man/ets.html#select_reverse-2), so let's take a quick stop and see what that does for our ETS tables.

```lfe
> (ets:select_reverse set-table '(#(#($1 $2) () (#(#($1 $2))))))
(#(15 15)
 #(25 25)
 #(13 13)
 #(21 21)
 #(11 11)
 #(1 1)
 #(23 23)
 #(7 7)
 #(3 3)
 #(9 9)
 #(19 19)
 #(29 29)
 #(27 27)
 #(17 17)
 #(5 5))
> (ets:select_reverse ordered-set-table '(#(#($1 $2) () (#(#($1 $2))))))
(#(29 29)
 #(27 27)
 #(25 25)
 #(23 23)
 #(21 21)
 #(19 19)
 #(17 17)
 #(15 15)
 #(13 13)
 #(11 11)
 #(9 9)
 #(7 7)
 #(5 5)
 #(3 3)
 #(1 1))
```

If we look at the results of ``ets:select/2`` and ``ets:select_reverse/2``, we see that for ``set-table`` we get the same result, and for ``ordered-set-table`` we get the results in a reverse order, which is what the documentation for ``ets:select_reverse/2`` states.  Which makes sense if you think about it, 

With that brief diversion out of the way, lets run our same ``match_spec()``s from above, but limit the results to ``5`` records so we get a continuation back.

```lfe
> (set `#(,set-result ,set-continuation) (ets:select set-table '(#(#($1 $2) () (#(#($1 $2))))) 5))
#((#(19 19) #(29 29) #(27 27) #(17 17) #(5 5))
  #(8207 214 5 #"" () 0))
> (set `#(,ord-set-result ,ord-set-continuation) (ets:select ordered-set-table '(#(#($1 $2) () (#(#($1 $2))))) 5))
#((#(1 1) #(3 3) #(5 5) #(7 7) #(9 9))
  #(12304 9 () 5 #"" () 0 0))
```

And with those continuations, we will see what the next results we would fetch would be.

```lfe
> (ets:select set-continuation)
#((#(1 1) #(23 23) #(7 7) #(3 3) #(9 9))
 #(8207 111 5 #"" () 0))
> (ets:select ord-set-continuation)
#((#(11 11) #(13 13) #(15 15) #(17 17) #(19 19))
  #(12304 19 () 5 #"" () 0 0))
```

Remember those "gaps" we left in our sequence of numbers we used to create tuples?

Time to "fill in" those gaps of the sequence to see what happens if we fetch with our existing continuation as data gets populated concurrently.

```lfe
> (list-comp ((<- x (lists:seq 2 30 2))) (ets:insert set-table (tuple x x)))
(true true true true true true true true true true true true true true true)
> (list-comp ((<- x (lists:seq 2 30 2))) (ets:insert ordered-set-table (tuple x x)))
(true true true true true true true true true true true true true true true)
```

Now we re-run our `ets:select/1` functions with the same continuations as before.

```lfe
> (ets:select set-continuation)
#((#(12 12) #(7 7) #(3 3) #(10 10) #(9 9))
  #(8207 224 5 #"" () 0))
> (ets:select ord-set-continuation)
#((#(10 10) #(11 11) #(12 12) #(13 13) #(14 14))
  #(12304 14 () 5 #"" () 0 0))
```

If we compare that to before we can see the we now have even number items in the list.  For our ``set-table`` if we look above at the ``set-continuation`` value itself, we have the continuation point as ``214``, since that is the only thing that has changed between that continuation and the resulting continuations from calling ``(ets:select set-continuation)``.  So with just a number it is hard to infer just how we might expect the continuation to change.

The ``ord-set-continuation`` on the other hand, has a ``9`` as its second element in the tuple, after the ETS table id of ``12304``.  This also happens to be the key of the last tuple in the result set, which matches up with the ``19`` and ``14`` in the other continuations.  So in the case of the ordered set, we can infer that as part of the continuation for an ETS table of type ``ordered_set``, the continuation tells us the specific key of the last record that was returned, and we continue from that record regardless of any concurrent inserts that may have taken place.

Next time we will take a look at [ets:is_compiled_ms/1](http://erlang.org/doc/man/ets.html#is_compiled_ms-1) and how match specs might play in with continuations based off reading the documentation about ``ets:is_compiled_ms/1``.
