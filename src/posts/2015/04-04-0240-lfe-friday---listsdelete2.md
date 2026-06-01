---
layout: post.liquid
title: "LFE Friday - lists:delete/2"
description: ""
permalink: "/blog/tutorials/2015/04/04/0240-lfe-friday---listsdelete2"
categories: ["tutorials"]
tags: ["lfe friday", "lfe", "erlang"]
published_date: 2015-04-04 02:40:00 +0000
is_draft: false
data:
  author: robert-virding
  written_for: null
  last_validated: null
  cover_image: "/images/fridays/LFE_Friday_00271_.png"
  cover_alt: "Vigdís — LFE Friday, LiffyBot studying Erlang modules in a Mead-style command room"
  math: false
---
<a href="/blog/assets/images/posts/LispFlavoredErlang-medium-square.png"><img class="left tiny" src="/blog/assets/images/posts/LispFlavoredErlang-medium-square.png" /></a>This week's LFE Friday was translated with permission from the
[Erlang Thursday](http://www.proctor-it.com/category/erlang/erlang-thursday/)
series by [Steven Proctor](https://twitter.com/stevenproctor).
*This week's translator*: Robert Virding.

Today’s LFE Friday is about [lists:delete/2](http://www.erlang.org/doc/man/lists.html#delete-2).

``lists:delete/2`` takes a Erlang term as it’s first argument, and
will remove that item from the list passed in as the second argument.

```lfe
> (lists:delete 1 '(8 7 6 5 4 3 2 1))                         
(8 7 6 5 4 3 2)
> (lists:delete 4 '(1 1 2 3 5 8))    
(1 1 2 3 5 8)
> (lists:delete 72 "Hello World!")
"Hello World!"
> (lists:delete 'd '(a b c d))
(a b c)
> (lists:delete 4 ())         
()
> (lists:delete #(b 2) '(#(a 1) #(b 2) #(c 3)))
(#(a 1) #(c 3))
> (lists:delete '(1 2 3) '((4 5 6) (7 8 9) (1 2 3)))
((4 5 6) (7 8 9))
```

Note that ``lists:delete/2`` only removes the first item found in the
list, and leaves any other occurrences of the item in the list.

```lfe
> (lists:delete 1 '(1 1 2 3 5 8))                   
(1 2 3 5 8)
```

As ``lists:delete/2`` was a easy function to demonstrate, and leaving
it at this would be a very short post, I thought it might be worth
showing a how you might write a very naive[^1] implementation of
``lists:delete/2`` yourself.

```lfe
(defmodule my-lists
  (export (delete 2)))
 
(defun delete (item list)
  (delete item list ()))
 
(defun delete
  ((item (cons head rest) checked) (when (=:= item head))
   (lists:reverse checked rest))
  ((item (cons head rest) checked)
   (delete item rest (cons head checked)))
  ((item () checked)
   (lists:reverse checked)))
```

Let’s start with our delete function as we expect it to be called from the outside world.

``my-lists:delete/2`` is the nice API function that just calls ``delete/3`` which is a “private” function (not exported) so the consumer doesn’t have to worry about passing in the accumulator for the items we have checked so far, which we pass as an empty list for the initial value.

```lfe
(defmodule my-lists
  (export (delete 2)))
 
(defun delete (item list)
  (delete item list []))
```

The first function clause of ``delete/3`` uses pattern matching to check if the item we want to delete is also the first item in the rest of the list to check. Note that we have to explicitly test if the item is the head of the list using a guard which is done with ``when``. If the pattern match succeeds, we have found the first occurrence of the item to remove! We can stop processing the list and return the result, which is the reverse of the list of items we have checked so far combined with the rest of the items we never got around to checking[^2].

```lfe
  ((item (cons head rest) checked) (when (=:= item head))
   (++ (lists:reverse checked) rest))
```

The second clause "knows" that the item we are wanting to delete and the first item in the rest of the list don't match. How does it "know"? Because if they did match, the first clause would have matched and this clause would not have been evaluated. As we haven't found the item to remove, we add the item held by ``head`` to the list of ``checked`` items, and then continue calling ``delete/3``. The fact that we are passing a new list of the checked items by prepending ``head`` to the list in ``checked`` is why we need to reverse ``checked`` in the first and third function clauses.

```lfe
  ((item (cons head rest) checked)
   (delete item rest (cons head checked)))
```

The third, and final, clause of ``delete/3`` has reached the end of the list and not found the item, so we just return the list we have reversed it.

```lfe
  ((item () checked)
   (lists:reverse checked)))
```

We will also present an alternative implementation which directly returns the resulting list without keeping a reversed list of items checked so far.

```lfe
(defmodule my-list
  (export (delete 2)))

(defun delete
  ((item (cons head rest)) (when (=:= item head))
   rest)
  ((item (cons head rest))
   (cons head (delete item rest)))
  ((item ())
   ()))
```

For a discussion on the relative merits of the two different ways of implementing ``delete/2`` see [myth about tail-recursive functions](http://erlang.org/doc/efficiency_guide/myths.html#id59389).

-Proctor, Robert

[^1]: Naive because this is not optimized for performance, or exhaustively tested for completely accurate behavior of ``lists:delete/2``.

[^2]:  This can be more efficiently written as ``(lists:reverse checked rest)`` as ``lists:reverse/2`` is a function which reverses its first argument and appends its second argument.
