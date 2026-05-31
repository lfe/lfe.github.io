---
layout: post.liquid
title: "LFE Friday - ETS, match_specs and functions"
description: ""
permalink: "/blog/tutorials/2016/01/18/1312-lfe-friday---ets-match_specs-and-functions"
categories: ["tutorials"]
tags: ["lfe friday", "lfe", "erlang"]
published_date: 2016-01-18 13:12:00 +0000
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

In [last week's LFE Friday](http://blog.lfe.io/tutorial/2016/01/10/2016-lfe-friday---more-ets-data-matching/) we concluded with showing how we can take advantage of using ``ets:select`` to take advantage of making our queries more expressive.

First we will need a new ETS table, so we start with a new public "Products" table, and do our standard of creating a new process and giving ownership of the table away.

```lfe
> (set fun (lambda () (receive (after 'infinity 'ok))))
#Fun<lfe_eval.23.88887576>
> (set some-process (spawn fun))
<0.36.0>
> (set products (ets:new 'products '(public)))
8207
> (ets:give_away products some-process ())
true
```

Next we will load our "Products" into the table.

In our case, we are just creating a "product" with the "name" as a binary and a "price in CWC" (Common World Currency) as an integer.

```lfe
> (list-comp ((<- x (lists:seq 1 100))) (ets:insert products `#(,(integer_to_binary x) ,x)))
(true true true true true true true true true true true true true
 true true true true true true true true true true true true true
 true true true true ...)
```

As we saw last week, we can manually build up a list of tuples into the ``match_spec()`` to run our query, say for items less than 10 CWCs.

```lfe
> (ets:select products '(#(#($1 $2) (#(< $2 10)) ($1))))
(#"8" #"6" #"5" #"3" #"7" #"1" #"4" #"9" #"2")
```

We can also find those item names that are more than 10 CWCs and less than 25 CWCS.

```lfe
> (ets:select products '(#(#($1 $2) (#(> $2 10) #(< $2 25)) ($1))))
(#"11"
 #"15"
 #"23"
 #"20"
 #"21"
 #"14"
 #"12"
 #"13"
 #"16"
 #"19"
 #"17"
 #"18"
 #"22"
 #"24")
```

But this isn't necessarily clear, as we are using numerical values for the items in the tuple, and lists of tuples with lists of tuples inside them.

Enter the ``match-spec`` macro to the rescue. ``match-spec`` will take a ``match-lambda`` and turn that into a ``match_spec()``.

This allows us to write a function that takes a tuple of ``product`` and ``cost``, and will return the ``product`` if the ``cost`` is less than 10.

```lfe
> (match-spec ([(tuple product cost)] (when (< cost 10)) product))
(#(#($1 $2) (#(< $2 10)) ($1)))
```

We can also have compound checks in our guard clauses on the functions we pass to ``match-spec``, such as and clauses,

```lfe
> (set between-25-and-35-CWC (match-spec ([`#(,product ,cost)] (when (> cost 25) (< cost 35)) product)))
(#(#($1 $2) (#(> $2 25) #(< $2 35)) ($1)))
> (ets:select products between-25-and-35-CWC)
(#"30" #"33" #"32" #"29" #"28" #"26" #"34" #"27" #"31")
```

as well as or style clauses. We used the `` ` `` macro here to write the pattern.

While this is useful it does have its limitations, as this parse transform on the function, so you can't use everything that you would be able to with a normal function.

```lfe
> (match-spec ([`#(,product ,cost)] (when (> cost 30)) (lists:reverse (binary:bin_to_list product))))
1: error expanding (match-spec
                  ((`#(,product ,cost))
                   (when (> cost 30))
                   (lists:reverse (binary:bin_to_list product))))
error
```

But then again, the results part of the ``match_spec()``, doesn't support advanced functionality anyways.

```lfe
> (ets:select products '(#(#($1 $2) (#(< 90 $2)) (#(binary $1)))))            
exception error: badarg
  in (: ets select 8207 (#(#($1 $2) (#(< 90 $2)) (#(binary $1)))))
```

But even with its limitations, ``match-spec`` still does a good job to help make our ETS queries more expressive.  Not only can we reference a function with expressive variable names over just ``$N``, as well as give guard clauses instead of just guard tuples, but we can also use those variable names in our results as well, and do the basic formatting as part of the function.

And make sure to check back in next week as we continue with looking at the different versions of ``ets:select``.

\- Proctor, Robert
