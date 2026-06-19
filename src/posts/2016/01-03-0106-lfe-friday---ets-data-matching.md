---
layout: post.liquid
title: "LFE Friday - ETS data matching"
description: ""
permalink: "/blog/tutorials/2016/01/03/0106-lfe-friday---ets-data-matching"
categories: [tutorials]
tags: [lfe-friday, lfe, erlang]
published_date: 2016-01-03 01:06:00 +0000
is_draft: false
data:
  author: robert-virding
  written_for:
    lfe: "1.2"
    erlang: "18"
  last_validated: null
  cover_image: "/images/fridays/LFE_Friday_00279_.png"
  cover_alt: "Vigdís — LFE Friday, LiffyBot studying Erlang modules in a Mead-style command room"
  math: false
---

<a href="/blog/assets/images/posts/LispFlavoredErlang-medium-square.png"><img class="left tiny" src="/blog/assets/images/posts/LispFlavoredErlang-medium-square.png" /></a>This week's LFE Friday was translated with permission from the
[Erlang Thursday](http://www.proctor-it.com/category/erlang/erlang-thursday/)
series by [Steven Proctor](https://twitter.com/stevenproctor).
*This week's translator*: Robert Virding.

Today's New Year LFE Friday moves on from the introduction to ETS, and starts using it to store some data and do some retrieval of data in ETS.

First we need some data to have in ETS, so we will fall back to one of Proctor's goto problems, Markov Chains.

For those unfamiliar with what a Markov Chain is, it is a state machine that transitions to the next state based off a probability instead of a specific input.  The common example that people are familiar with in "everyday use" is predictive typing on smart phones, where the next word or letter is offered up as a prediction, and the predicted words are chosen by the historical likelihood that the words predicted follows the current word.

The first thing we will do is to create a module that given a string of text, will return a list of tuples representing a word and the word that follows.

```lfe
(defmodule markov-words
  (export (create-word-pairs 1)))

(defun create-word-pairs (text)
  (let ((words (string:tokens text " \t\n")))
    (create-word-pairs words ())))

(defun create-word-pairs
  (((list* word following words) word-pairs)
   (let ((updated-word-pairs `(#(,word ,following) . ,word-pairs)))
     (create-word-pairs (cons following words) updated-word-pairs)))
  ((words word-pairs) word-pairs))
```

The above code takes a string of text and splits that text into "words" based off using the space, tab, and newline characters as a word boundary.  With that list of "words", we then create a list of word to following word tuples, which is what we will be inserting into our ETS table.

Time to fire up the Erlang shell and start experimenting.

We first save the text we want to use in the file ``tail-of-two-cities.txt``.

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

We will create a new process to give our ETS table away to, just in case we bomb out the shell.

```lfe
> (set fun (lambda () (receive (after 'infinity 'ok))))
#Fun<lfe_eval.23.88887576>
> (set some-process (spawn fun))
<0.43.0>
```

And we now create an ETS table that will store data for us to use as part of our Markov Chain generation.

```lfe
> (set word-pairs (ets:new 'word-pairs '(public duplicate_bag)))
8207
> (ets:give_away word-pairs some-process ())
true
```

We make the table ``public`` in this case, since we want our shell process, which is no longer the owner, to be able to add items to the table, and we make the table type a duplicate bag.

The reason for the ``duplicate_bag``, is that for demonstration reasons, we want to be able to have multiple entries with the same key, as we are likely to see any word multiple times, and that some sets of word pairs are more common, so we want to be able to capture (and retain) those "duplicates".

And for ease of population from inside the shell, we will use a list comprehension to add each word pair tuple we create from the text into our ETS table by calling `ets:insert/2`.

```lfe
> (list-comp ((<- word-pair (markov-words:create-word-pairs totc))) (ets:insert word-pairs word-pair))
(true true true true true true true true true true true true true
 true true true true true true true true true true true true true
 true true true true ...)
```

Now we should have some data in our ETS table, it is time to see how we can access our data.  For this we turn to [ets:match/2](http://www.erlang.org/doc/man/ets.html#match-2).  ``ets:match/2`` takes a table to query, and a ``pattern``.

The pattern is made up an Erlang term to be matched against; ``_``, which matches anything and doesn't bind; or pattern variables, which take the form of ``$N`` where ``N`` is any non-negative integer.  The return result of ``ets:match/2`` is a list containing the list of values in the pattern variables in order of variable name order.

So with this knowledge, we can try to find the word pairs to find the words that follow ``"of"``.  If we were doing a pattern match it would look like ``(tuple "of" following)``, but using ETS, we need to use a pattern variable which would make our spec ``#("of" $1)``.

So lets run that against our ETS table.

```lfe
> (ets:match word-pairs #("of" $1))
(("loaves")
 ("the")
 ("France.")
 ("England;")
 ("comparison")
 ("its")
 ("despair,")
 ("hope,")
 ("Darkness,")
 ("Light,")
 ("incredulity,")
 ("belief,")
 ("foolishness,")
 ("wisdom,")
 ("times,")
 ("times,"))
```

And there we go, we can see the results is a list of the list of variable matches, in this case, just what ``$1`` matched to.

For fun and exploration, let's confirm what we would get if we look for the words that follow ``"it"`` in our Tale of Two Cities intro text.

```lfe
> (ets:match word-pairs #("it" $1))
(("was")
 ("was")
 ("was")
 ("was")
 ("was")
 ("was")
 ("was")
 ("was")
 ("was")
 ("was"))
```

Just a bunch of ``"was"`` which is right for the first two paragraphs of the book.

And we will double check and look for any words that follow ``"Scrooge"``.

```lfe
> (ets:match word-pairs #("Scrooge" $1))
()
```

And if we wanted to get the whole tuple back, we could use ``ets:match_object/2``, which will return the whole object that satisfies the match

```lfe
> (ets:match_object word-pairs #("of" $1))
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

or, in this case we could use ``ets:lookup/2`` which returns all of the items for which the key matches.

```lfe
> (ets:lookup word-pairs "of")
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

So to take a brief detour away from the Markov Chain example, why might we want to use either ``ets:lookup/2`` or ``ets:match_object/2`` versus the other?  To answer that with an example, let's add another entry into our ``word-pairs`` table, that is a three-tuple.

```lfe
> (ets:insert word-pairs #("of" "times" "it"))
true
```

If we do a ``ets:lookup/2`` we get all items with the specified key.

```lfe
> (ets:lookup word-pairs "of")
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
 #("of" "times" "it"))
```

But if we use ``ets:match_object/2``, and use a two-tuple because we only want the word _pairs_, we don't get the item that is a three-tuple in the results.

```lfe
> (ets:match_object word-pairs #("of" _))
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

Back to the Markov Chain scenario, we can start to see how we can get some text that follows the Markov Chain rules.

We get the match of potential words to choose from for a given word, and we pick a uniformly random result from the list of following words.

```lfe
> (set potential-choices (ets:match word-pairs #("of" $1)))
(("loaves")
 ("the")
 ("France.")
 ("England;")
 ("comparison")
 ("its")
 ("despair,")
 ("hope,")
 ("Darkness,")
 ("Light,")
 ("incredulity,")
 ("belief,")
 ("foolishness,")
 ("wisdom,")
 ("times,")
 ("times,"))
> (set (list next-word) (lists:nth (random:uniform (length potential-choices)) potential-choices))
("hope,")
```

We can write a function that will repeat these steps until we get to our termination case. Some examples of a termination state could be a word that doesn't have a word that follows it; we get to a certain number of words picked to make up our text; or we get to a certain total length, say to fit in a SMS or Tweet.

With that, we have started to scratch the surface of putting some "real" data into ETS, and doing matching against the data for some given pattern.  Next week we will continue looking at this example with other ways to get data out of our ETS tables into something that might be nicer to consume.

\- Proctor, Robert
