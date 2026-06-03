---
layout: post.liquid
title: "lutil 0.5.0: Composition, Predicates and Core lutil"
description: "New features in lutil 0.4.1"
permalink: "/blog/announcements/2014/12/14/1645-lutil-050-composition-predicates-core"
categories: ["announcements"]
tags: ["releases", "libraries", "language"]
published_date: 2014-12-14 16:45:00 +0000
is_draft: false
data:
  author: duncan-mcgreggor
  written_for: null
  last_validated: null
  cover_image: "/images/newsroom/LFE_Newsroom_00308_.png"
  cover_alt: "Vigdís — LFE news, a busy rotating space-station newsroom"
  math: false
---
<a href="/blog/assets/images/posts/lutil-leonardo-gears-large.png"><img class="right thumb" src="/blog/assets/images/posts/lutil-leonardo-gears.png" /></a>With the
[release of lutil 0.5.0](https://github.com/lfex/lutil/releases/tag/0.5.0),
there are new "compose" functions accompanying the
[previously-merged](https://github.com/lfex/lutil/commits/master/include/compose-macros.lfe?author=dysinger)
thrushing macros as well as a new convenience include file which contains all
of lutil's predicate functions defined for easy use in the REPL or in modules.
Additionally there is a new, experimental include file that is beginning to
define functions and macros considered "core" to the LFE experience but which
aren't yet (and may never be) included in LFE-proper. Some of these may wrap
Erlang functions with more options, others may provide new syntax, etc. See
below for usage examples.


## Core Include File

Be warned! This is for experimentation! Do not depend upon these functions
remaining here in perpetuity.

This is a new include file while is the home for any functions that feel like
they should be part of the language. They might wrap Erlang functions or
provide basic functionality that's not in Erlang or LFE proper.

For now, it's just the following:

* ``seq``
* ``range``
* ``next``
* ``take``


### ``seq`` Functions

Let's start with pulling in the ``core`` include in the LFE REPL:

```lfe
> (include-lib "lutil/include/core.lfe")
loaded
```

Erlang doesn't have a ``lists:seq/1``, so we made one:

```lfe
> (seq 10)
(1 2 3 4 5 6 7 8 9 10)
```

Having done that, we also provided wrappers for Erlang's ``lists:seq/2``
and ``lists:seq/3``.

As you can see, we opted for 1 as the default starting element. This follows in
the tradition of many of Erlang's ``lists`` functions. 0-based sequences can
just use ``seq/2``, e.g. ``(seq 0 10)``.


### ``range`` Functions

These functions were inspired by Clojure's
[range](https://github.com/clojure/clojure/blob/clojure-1.6.0/src/clj/clojure/core.clj#L2725)
function as well as
[Python generators](https://docs.python.org/3/glossary.html#term-generator).
Our ``range`` provides us with the ability to generate an endless series of
integers or floating point numbers without using more memory that what is
required to create a few functions.

Unlike Python and Clojure, ``range`` is based upon Erlang's capacity for its
own brand of lazy evaluation as demonstrated in
[this blog post](http://erlangraymondtay.blogspot.com/2009/08/example-of-lazy-evaluation-for.html).
In particular, ``(range)`` returns a function (and so is more akin to Python's
generators that Clojure's ``range`` function). When called, it will return a
``cons`` of:

* the next element of the defined series, and
* another function, which will do the same as the previous function (but whose
  first ``cons`` element is the next element in the series)

Some example usage:

```lfe
> (range)
#Fun<lfe_eval.23.86468545>
> (funcall (range))
(1 . #Fun<lfe_eval.23.86468545>)
> (funcall (range 100))
(100 . #Fun<lfe_eval.23.86468545>)
```

The ``range`` function is actually a special case of the more general ``next``
function in lutil ``core``. More on that below.

lutil ``core`` defines the following:

 * ``range/0`` (default ``start`` of ``1`` and ``step`` of ``1``)
 * ``range/1`` - ``(range start)`` (default ``step`` of ``1``)
 * ``range/2`` - ``(range start step)``


### ``take`` Functions

For ``range`` to be very useful, we need be able to pull values from it.
Otherwise, we're left with usage like the following:

```lfe
> (funcall (range))
(1 . #Fun<lfe_eval.23.86468545>)
> (funcall (cdr (funcall (range))))
(2 . #Fun<lfe_eval.23.86468545>)
> (funcall (cdr (funcall (cdr (funcall (range))))))
(3 . #Fun<lfe_eval.23.86468545>)
> (funcall (cdr (funcall (cdr (funcall (cdr (funcall (range))))))))
(4 . #Fun<lfe_eval.23.86468545>)
>
```

That certainly has its own *peculiar* charm, but does not rate too highly in
convenience. As such, a function like Clojure's ``take`` has been added to
lutil ``core``. It does just what is says: takes a certain number of elements
from our infinite series.

```lfe
> (take 4 (range))
(1 2 3 4)
```

Hej! That's much nicer than the above :-)

Sometimes one's code will be using both infinite series as well as definite
lists and it would be nice to not have to change functions if the source
of the data changes. As such, we've modified ``take`` to provide a wrapper
for ``lists:sublist/2``:

```lfe
> (take 5 '(1 2 3 4 5 6 7 8 9 10 11 12))
(1 2 3 4 5)
```
Note that the ``take`` wrapper swaps the positions of
the arguments so that it may be used with the ``->>`` macro. If you need to
take from a list with the ``->`` macro, you will need to use
``lists:sublist/2``. (Be sure to see the section below for usage examples
of ``->`` and ``->>``!)

We also added the following, as it ended up being useful:

```lfe
> (take 'all '(1 2 3 4 5 6 7 8 9 10 11 12))
(1 2 3 4 5 6 7 8 9 10 11 12)
```

One last point on ``take``: it is not based upon an element value, but rather
the length of the accumulator. If you have use cases where you need to only
take elements up to a certain value, let us know and we can generalize this
further (also: patches welcome!).


### ``next`` Functions

Under the hood, the ``range`` function actually wraps the ``next`` function.
``next`` is a more general form that will repeatedly call a user-provided
2-arity function. In the case of ``range``, that function is addition.

For example, the following are identical:

```lfe
> (take 21 (range))
(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21)
> (take 21 (next #'+/2 1 1))
(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21)
> (take 21 (next (lambda (x y) (+ x y)) 1 1))
(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21)
```

You may use ``next`` directly to define your own infinite sequences. Here
are a few examples:

```lfe
> (take 10 (next (lambda (x y) (* 3 (+ x y))) 1 1))
(1 6 21 66 201 606 1821 5466 16401 49206)
> (take 17 (next (lambda (x _) (* 2 x)) 1 1))
(1 2 4 8 16 32 64 128 256 512 1024 2048 4096 8192 16384 32768 65536)
> (take 7 (next (lambda (x _) (math:pow (+ x 1) 2)) 1 1))
(1 4.0 25.0 676.0 458329.0 210066388900.0 4.4127887745906175e22)
```


## Predicates Include File

This set of changes (and examples) is the most tame of the bunch. lutil
has implemented several predicates of the form ``name?`` for the past while.

As projects have started to rely upon these more heavily, it seemed prudent
to provide the increasingly-more-used predicates in include-form (thus
alleviating developers having to use the full ``mod:fun`` syntax or from
complicated and hard-to-maintain special imports).

Here's a quick way of seeing which predicates are supported:

```lfe
> (set before (sets:from_list (lutil:get-env-funcs $ENV)))
#(set 11 16 16 8 80 48
...)
> (include-lib "lutil/include/predicates.lfe")
loaded
> (set after (sets:from_list (lutil:get-env-funcs $ENV)))
#(set 49 16 16 8 80 48
...)
> (set loaded-funcs (lists:sort
                      (sets:to_list
                        (sets:subtract after before)))))
...
```

Now we can see the functions available in our REPL environment that were
brought in from ``include-lib``:

```lfe
> (lfe_io:format "~p~n" (list loaded-funcs))
(all? any? atom? binary? bitstring? bool? dict? element? empty? even?
 every? false? float? func? function? identical? in? int? integer?
 list? loaded neg? nil? not-any? not-in? number? odd? pos? record?
 reference? set? string? true? tuple? undef? undefined? unicode?
 zero?)
ok
```

You can use the predicates include from the REPL or in modules with the usual
``include-lib`` call, as above.

Some example usage:

```lfe
> (zero? 0)
true
> (zero? 1)
false
> (all? #'even?/1 '(2 4 6 8 9))
false
> (all? #'even?/1 '(2 4 6 8 10))
true
```


## ``compose`` Functions

All of the 0.5.0 changes detailed above were actually yak-shavings in support of
the ``compose`` functions. These new functions have been added as companions to
the threshing macros (see below). These are similar to Clojure's ``compose``
function, but with some syntactic sugar to assist with the fact that LFE is a
Lisp-2.

Pull in the functions:

```lfe
> (include-lib "lutil/include/compose.lfe")
loaded
```

Let's call ``compose/2`` on two math functions:

```lfe
> (funcall (compose #'math:sin/1 #'math:asin/1)
           0.5)
0.49999999999999994
```

Now let's use ``compose/1`` on a list of functions:

```lfe
> (funcall (compose `(,#'math:sin/1
                      ,#'math:asin/1
                      ,(lambda (x) (+ x 1))))
           0.5)
1.5
```

Here is compose being used in a filter:

```lfe
> (include-lib "lutil/include/predicates.lfe")
loaded
> (lists:filter (compose #'not/1 #'zero?/1)
                '(0 1 0 2 0 3 0 4))
(1 2 3 4)
```

Unlike schemes and Clojure, when calling ``compose`` directly, we can't just
wrap parens around our function -- we need to call ``funcall`` on it. But we can
cheat, with a little help from Erlang arities :-)

The following are provided as conveniences when using compose by itself (in
other words, not in a call to ``lists:map``, ``lists:filter``, a predicate,
etc.):

```lfe
> (compose #'math:sin/1 #'math:asin/1 0.5)
0.49999999999999994
> (compose `(,#'math:sin/1
             ,#'math:asin/1
             ,(lambda (x) (+ x 1)))
           0.5)
1.5
```

## Thrushing Macros

And now we've reached dessert :-)

The following examples are for functionality that was previously added to
lutil, authored originally by
[Tim Dysinger](https://github.com/dysinger/lfesl/blob/master/include/thread.lfe).
Though not part of this release, these bonus usage examples are provided since
it's such a cool set of macros, inspired by their
Clojure analogs [->](http://clojuredocs.org/clojure.core/-%3E)
and [-> >](http://clojuredocs.org/clojure.core/-%3E%3E).

### The ``->`` Macro

Reading (and sometimes writing) deeply nested functions can be a bit awkward:

```lfe
> (lists:sublist
    (lists:reverse
      (lists:sort
        (lists:merge
          (string:tokens
            (string:to_upper "a b c d e")
            " ")
          '("X" "F" "L"))))
    2 3)
("L" "F" "E")
```

This may seem like a contrived example (and well, yes, it is), but there are
use cases where this comes up. In particular, the world of web application
middleware where code is run between request and response one can get large
stacks of nested functions.

Now grab the thrushing macros:

```lfe
> (include-lib "lutil/include/compose.lfe")
loaded
```

Here's how the first thrushing macro can help the previous example:

```lfe
> (-> "a b c d e"
      (string:to_upper)
      (string:tokens " ")
      (lists:merge '("X" "F" "L"))
      (lists:sort)
      (lists:reverse)
      (lists:sublist 2 3))
("L" "F" "E")
```

What's happening here is that the output from one function is passed as
(inserted, really) the first argument in the next function.

The next macro does the opposite ...


### The ``->>`` Macro

Let's get some includes:

```lfe
> (include-lib "lutil/include/predicates.lfe")
loaded
> (include-lib "lutil/include/core.lfe")
loaded
```

Next let's make a bunch of nested calls:

```lfe
> (lists:foldl #'+/2 0
    (take 10
      (lists:filter
        (compose #'even?/1 #'round/1)
        (lists:map
          (lambda (x)
            (math:pow x 2))
          (seq 42)))))
1540.0
```

Grab the thrushing macros:

```lfe
> (include-lib "lutil/include/compose.lfe")
loaded
```

And now let's rewrite the nested functions using the ``->>`` macro:

```lfe
> (->> (seq 42)
       (lists:map (lambda (x) (math:pow x 2)))
       (lists:filter (compose #'even?/1 #'round/1))
       (take 10)
       (lists:foldl #'+/2 0))
1540.0
```

As promised, ``->>`` does the opposite of ``->`` in that the output from one
function is *appended* to the arguments for the next call. In other words, the
output of the previous call is the last argument in the next call.
