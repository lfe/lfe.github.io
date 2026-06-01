---
layout: post.liquid
title: "Evaluating dynamic expressions in LFE"
description: ""
permalink: "/blog/tutorials/2015/05/02/1910-evaluating-dynamic-expressions-in-lfe"
categories: ["tutorials"]
tags: ["lfe", "backquote", "techniques", "tips"]
published_date: 2015-05-02 19:10:00 +0000
is_draft: false
data:
  author: robert-virding
  written_for: null
  last_validated: null
  cover_image: null
  cover_alt: null
  math: false
---
<a href="/blog/assets/images/posts/LispFlavoredErlang-medium-square.png"><img class="right small" src="/blog/assets/images/posts/LispFlavoredErlang-medium-square.png" /></a>Sometimes you need to evaluate a dynamic expression in LFE, one that has been created during the execution of the program. For example, imagine some other process has sent us an LFE expression which we need to evaluate.

This can be done with the function ``eval/1``. As one might guess, ``eval/1`` evaluates its argument as an expression.

Here is an example:

```lfe
> (eval '(+ 1 2 3))
6
> (eval '(calendar:is_leap_year 1200))
true
```

This works well if the expression is a literal value, but what if the expression has free variables for which we want to import values. For example we want get the value of ``x`` into ``(+ 1 2 3 x)``:

```lfe
> (set x 42)
42
> (eval '(+ 1 2 3 x))
exception error: #(unbound_symb x)
```

which is not quite what we had hoped. The solution is to build a ``let`` form where we can import values for the variables which need binding:

```lfe
> (eval '(let ((x 42)) (+ 1 2 3 x)))
48
```

However, if the expression and values are not static then we must build the whole ``let`` expression to evaluate:

```lfe
> (set x 42)
42
> (set expr '(+ 1 2 3 x))
(+ 1 2 3 x)
> (list 'let (list (list 'x x)) expr)
(let ((x 42)) (+ 1 2 3 x))
> (eval (list 'let (list (list 'x x)) expr))
48
```

Problem solved, now we can build expressions and import values into them. However, even with an expression as simple as this one building the structure can be quite complex. Fortunately, to the rescue comes the backquote `` ` `` macro[^1] which quotes *and* lets us import values:

```lfe
> `(let ((x ,x)) ,expr)
(let ((x 42)) (+ 1 2 3 x))
> (eval `(let ((x ,x)) ,expr))
48
```

You can read more about the backquote macro in the [LFE Tutorial: The Backquote Macro](http://lfe.gitbooks.io/tutorial/content/macros/backquote.html).

Robert

[^1]: This basically the same backquote as is in Common Lisp.
