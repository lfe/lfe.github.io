---
layout: post.liquid
title: "LFE Friday - More ETS data matching (and querying)"
description: ""
permalink: "/blog/tutorials/2016/01/10/2016-lfe-friday---more-ets-data-matching"
categories: ["tutorials"]
tags: ["lfe-friday", "lfe", "erlang"]
published_date: 2016-01-10 20:16:00 +0000
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

In today's LFE Friday we continue from last week in looking at getting data from ETS.

To refresh, we have a module ``markov-words``, and for this week we have added a new function ``markov-words:create-word-triples/1``.

```lfe
(defmodule markov-words
  (export (create-word-pairs 1)
	  (create-word-triples 1)))

(defun create-word-pairs (text)
  (let ((words (string:tokens text " \t\n")))
    (create-word-pairs words ())))

(defun create-word-pairs
  ([`(,word ,following . ,words) word-pairs]
   (let ((updated-word-pairs `(#(,word ,following) . ,word-pairs)))
     (create-word-pairs (cons following words) updated-word-pairs)))
  ([words word-pairs] word-pairs))

(defun create-word-triples (text)
  (let ((words (string:tokens text " \t\n")))
    (create-word-triples words ())))

(defun create-word-triples
  ([`(,word ,second-word ,following . ,words) word-triples]
   (let ((updated-word-triples
	  `(#(,word ,second-word ,following) . ,word-triples)))
     (create-word-triples `(,second-word ,following . ,words) updated-word-triples)))
  ([words word-triples] word-triples))
```

The excuse for having this new function is that it would allow us to get more refined Markov chains by picking the probability of the next word by having the state be the compound key of the last two words seen.

As before we save our text in the file ``tail-of-two-cities.txt``.

```bash
$  cat > tail-of-two-cities.txt
It was the best of times, it was the worst of times,
it was the age of wisdom, it was the age of foolishness,
it was the epoch of belief, it was the epoch of incredulity,
it was the season of Light, it was the season of Darkness,
it was the spring of hope, it was the winter of despair,
we had everything before us, we had nothing before us,
we were all going direct to Heaven,
we were all going direct the other way--in short,
the period was so far like the present period,
that some of its noisiest authorities insisted on its
being received, for good or for evil, in the superlative
degree of comparison only.

There were a king with a large jaw and a queen with a
plain face, on the throne of England; there were a king
with a large jaw and a queen with a fair face,
on the throne of France. In both countries it was
clearer than crystal to the lords of the State preserves
of loaves and fishes, that things in general were
settled for ever.
```

We then need to compile our module, and then we will create the variable ``totc`` to hold the text from the file which we want to use to prime our Markov Chain.

```lfe
> (c 'markov-words)
(#(module markov-words))
> (set totc (binary_to_list (element 2 (file:read_file "tail-of-two-cities.txt"))))
"It was the best of times, it was the worst of times,\nit was the age of wisdom, it was the age of foolishness,\nit was the epoch of belief, it was the epoch of incredulity,\nit was the season of Light, it was the season of Darkness,\nit was the spring of hope, it was the winter of despair,\nwe had everything before us, we had nothing before us,\nwe were all going direct to Heaven,\nwe were all going direct the other way--in short,\nthe period was so far like the present period,\nthat some of its noisiest authorities insisted on its\nbeing received, for good or for evil, in the superlative\ndegree of comparison only.\n\nThere were a king with a large jaw and a queen with a\nplain face, on the throne of England; there were a king\nwith a large jaw and a queen with a fair face,\non the throne of France. In both countries it was\nclearer than crystal to the lords of the State preserves\nof loaves and fishes, that things in general were\nsettled for ever.\n"
```

We create our fresh ETS table for this week, create a new process to own it, and give it away (in case we type something wrong and cause the current session of the shell to crash).

```lfe
> (set markov-words (ets:new 'markov-words '(public duplicate_bag)))
8207
> (set fun (lambda () (receive (after 'infinity 'ok))))
#Fun<lfe_eval.23.88887576>
> (set some-process (spawn fun))
<0.42.0>
> (ets:give_away markov-words some-process ())
true
```

This week, in addition to adding our word pair tuples to ETS, we will also add in our new word triple tuples to ETS in the same table.

```lfe
> (list-comp ((<- word-pair (markov-words:create-word-pairs totc))) (ets:insert markov-words word-pair))
(true true true true true true true true true true true true true
 true true true true true true true true true true true true true
 true true true true ...)
> (list-comp ((<- word-triple (markov-words:create-word-triples totc))) (ets:insert markov-words word-triple))
(true true true true true true true true true true true true true
 true true true true true true true true true true true true true
 true true true true ...)
```

Since we have both word pairs and word triples in the same ETS table, we can see that with ``ets:match_object/2``, we can specify a ``match_pattern()`` for only the two tuples

```lfe
> (ets:match_object markov-words #("of" $1))
(#("of" "loaves")
 #("of" "the")
 #("of" "France.")
 #("of" "England;")
 #("of" "comparison")
 #("of" "its")
 #("of" "despair,")
 #("of" "hope,")
 #("of" "Darkness,")
 #("of" "Light,")
 #("of" "incredulity,")
 #("of" "belief,")
 #("of" "foolishness,")
 #("of" "wisdom,")
 #("of" "times,")
 #("of" "times,"))
```

or a ``match_pattern()`` that will only match the three tuples.

```lfe
> (ets:match_object markov-words #("of" $1 $2))
(#("of" "loaves" "and")
 #("of" "the" "State")
 #("of" "France." "In")
 #("of" "England;" "there")
 #("of" "comparison" "only.")
 #("of" "its" "noisiest")
 #("of" "despair," "we")
 #("of" "hope," "it")
 #("of" "Darkness," "it")
 #("of" "Light," "it")
 #("of" "incredulity," "it")
 #("of" "belief," "it")
 #("of" "foolishness," "it")
 #("of" "wisdom," "it")
 #("of" "times," "it")
 #("of" "times," "it"))
```

Where as if we use the ``ets:lookup/2`` with the key, we get all items with the key, regardless of the tuple size.

```lfe
> (ets:lookup markov-words "of")
(#("of" "loaves")
 #("of" "the")
 #("of" "France.")
 #("of" "England;")
 #("of" "comparison")
 #("of" "its")
 #("of" "despair,")
 #("of" "hope,")
 #("of" "Darkness,")
 #("of" "Light,")
 #("of" "incredulity,")
 #("of" "belief,")
 #("of" "foolishness,")
 #("of" "wisdom,")
 #("of" "times,")
 #("of" "times,")
 #("of" "loaves" "and")
 #("of" "the" "State")
 #("of" "France." "In")
 #("of" "England;" "there")
 #("of" "comparison" "only.")
 #("of" "its" "noisiest")
 #("of" "despair," "we")
 #("of" "hope," "it")
 #("of" "Darkness," "it")
 #("of" "Light," "it")
 #("of" "incredulity," "it")
 #("of" "belief," ...)
 #("of" ...)
 #(...) ...)
```

And unlike ``ets:lookup/2``, with ``ets:match_object/2`` we can match on any tuple element, and not just the key.

```lfe
> (ets:match_object markov-words #($1 "the" $2))
(#("on" "the" "throne")
 #("on" "the" "throne")
 #("direct" "the" "other")
 #("short," "the" "period")
 #("like" "the" "present")
 #("of" "the" "State")
 #("to" "the" "lords")
 #("in" "the" "superlative")
 #("was" "the" "winter")
 #("was" "the" "spring")
 #("was" "the" "season")
 #("was" "the" "season")
 #("was" "the" "epoch")
 #("was" "the" "epoch")
 #("was" "the" "age")
 #("was" "the" "age")
 #("was" "the" "worst")
 #("was" "the" "best"))
```

And like ``ets:match_object/2``, ``ets:match/2`` can match based off the tuple itself as well.

```lfe
> (ets:match markov-words #("was" "the" $1))
(("winter")
 ("spring")
 ("season")
 ("season")
 ("epoch")
 ("epoch")
 ("age")
 ("age")
 ("worst")
 ("best"))
```

But sometimes we might want finer grain control over how the results are given back to us, such as a single list of items instead of a nested list of strings.  Or maybe we even have some criteria that we want to hold true as part of our selections on the data.

Enter [ets:select/2](http://www.erlang.org/doc/man/ets.html#select-2).

``ets:select/2`` takes the table as its first argument, and a ``match_spec()`` as its second argument.

The ``match_spec()`` is a list of three-tuples, where the first element is the match pattern, second element is a list of guard clause tuples, and the last element is the result is a term representation of the result for each match.

If we want to call ``ets:select/2`` and have it align with ``ets:match/2`` our call looks like the following.

```lfe
> (ets:select markov-words '(#(#("was" "the" $1) () (($1)))))
(("winter")
 ("spring")
 ("season")
 ("season")
 ("epoch")
 ("epoch")
 ("age")
 ("age")
 ("worst")
 ("best"))
```

The second argument is a list of ``match_spec()``s, of which there is only one which consists of:

1 - a ``match_pattern()`` of ``#("was" "the" $1)``, which is the same thing we gave to ``ets:match/2``

2 - ``()``, and empty list of guard condition tuples, and

3 - ``(($1))`` for the result term, which is the list of terms we want the result formatted as, in this case we want each result to be in its own list.

If we just wanted to get the word themselves as a list, we can update the result term part of the ``match_spec()`` to be ``($1)`` instead.

```lfe
> (ets:select markov-words '(#(#("was" "the" $1) () ($1))))  
("winter"
 "spring"
 "season"
 "season"
 "epoch"
 "epoch"
 "age"
 "age"
 "worst"
 "best")
```

If we wanted something that looked more like a ``ets:match_object/2`` result set we can use the result term of ``$_``, which signifies the whole object.

```lfe
> (ets:select markov-words '(#(#("was" "the" $1) () ($_))))
(#("was" "the" "winter")
 #("was" "the" "spring")
 #("was" "the" "season")
 #("was" "the" "season")
 #("was" "the" "epoch")
 #("was" "the" "epoch")
 #("was" "the" "age")
 #("was" "the" "age")
 #("was" "the" "worst")
 #("was" "the" "best"))
```

And if we wanted to only match on one of the items, and capture the other items in the tuple, we can use the result of ``$$`` which returns all of the match variable in a list, ordered by variable number as opposed to position in the ``match_pattern()``.

```lfe
> (ets:select markov-words '(#(#("was" $1 $2) () ($$))))
(("clearer" "than")
 ("so" "far")
 ("the" "winter")
 ("the" "spring")
 ("the" "season")
 ("the" "season")
 ("the" "epoch")
 ("the" "epoch")
 ("the" "age")
 ("the" "age")
 ("the" "worst")
 ("the" "best"))
> (ets:select markov-words '(#(#("was" $2 $1) () ($$))))
(("than" "clearer")
 ("far" "so")
 ("winter" "the")
 ("spring" "the")
 ("season" "the")
 ("season" "the")
 ("epoch" "the")
 ("epoch" "the")
 ("age" "the")
 ("age" "the")
 ("worst" "the")
 ("best" "the"))
```

With ``ets:select/2`` we also get the ability to specify multiple ``match_spec()``s.  This allows us to find all word triple word triples that have either ``"of"`` or ``"the"`` as the middle word.

```lfe
> (ets:select markov-words '(#(#($1 "the" $2) () ($_)) #(#($1 "of" $2) () ($_))))
(#("some" "of" "its")
 #("on" "the" "throne")
 #("on" "the" "throne")
 #("direct" "the" "other")
 #("preserves" "of" "loaves")
 #("throne" "of" "France.")
 #("throne" "of" "England;")
 #("worst" "of" "times,")
 #("short," "the" "period")
 #("winter" "of" "despair,")
 #("degree" "of" "comparison")
 #("epoch" "of" "incredulity,")
 #("epoch" "of" "belief,")
 #("spring" "of" "hope,")
 #("like" "the" "present")
 #("of" "the" "State")
 #("age" "of" "foolishness,")
 #("age" "of" "wisdom,")
 #("best" "of" "times,")
 #("season" "of" "Darkness,")
 #("season" "of" "Light,")
 #("to" "the" "lords")
 #("in" "the" "superlative")
 #("was" "the" "winter")
 #("was" "the" "spring")
 #("was" "the" "season")
 #("was" "the" "season")
 #("was" "the" ...)
 #("was" ...)
 #(...) ...)
```

And with guard clauses, we can find third item in the three-tuples that start with ``"was"``, that comes later in the dictionary than the word in the second position of the tuple.

```lfe
> (ets:select markov-words '(#(#("was" $1 $2) (#(< $1 $2)) ($2))))
("than" "winter" "worst")
```

So with this week's post we have seen other ways of using ``ets:match/2`` and ``ets:match_object/2``, and what they can get over using just a ``ets:lookup/2`` for a key, as well as being able to take advantage of even more powerful querying by using ``ets:select/2``.

Next week, we will look at more ways to use ``ets:select/2``, and how we can use some other ``ets`` module functions to help create queries that can be easier to deconstruct at a quicker glance.

\- Proctor, Robert
