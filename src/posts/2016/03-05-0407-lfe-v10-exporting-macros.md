---
layout: post.liquid
title: "Coming in LFE v1.0: exporting macros"
description: ""
permalink: "/blog/tutorials/2016/03/05/0407-lfe-v10-exporting-macros"
categories: ["tutorials"]
tags: ["v1.0"]
published_date: 2016-03-05 04:07:00 +0000
is_draft: false
data:
  author: robert-virding
  written_for:
    lfe: "0.10"
    erlang: "18"
  last_validated: null
  cover_image: "/images/tutorials/LFE_Tutorial_00361_.png"
  cover_alt: "Vigdís — LFE tutorial, a retro-futurist study aboard a spaceship"
  math: false
---
<a href="/blog/assets/images/posts/LFE-signal.jpg"><img class="right thumb" src="/blog/assets/images/posts/LFE-signal.jpg" /></a>Here are some more new things which are coming in LFE v1.0. These can also be tested on the develop branch.
<br /><br />

The next new feature is being able to export macros from a module in the same way as exporting functions. Until now macros have had to be defined locally within a module to access them. The standard way of importing macros has been to include a file defining these macros. The exported macros can be "called" from other modules in the same way as functions but they are macros and it is their expansion which is inserted into the code.

As an example we will take the record ``foo`` which could be defined as ``(defrecord foo a b)`` but instead define it in a module which exports (a subset of) the standard record access macros for ``foo``. The module basically mirrors the expansion of the ``defrecord`` macro into access macros and internal functions. [^1]

```lfe
;; Define the record foo as a module and export the macros.
;; (defrecord foo a b)

(defmodule foo
  (export-macro make match is a set-a b set-b))

;; Functions defining and accessing the foo record internals.

(defun field-index
  (['a] 2)
  (['b] 3)
  ([f] (erlang:error (tuple 'undefined-field 'foo f))))

(defun field-loop
  ([(cons field (cons val fields)) i]
   (field-loop fields (setelement (- (field-index field) 1) i val)))
  ([(list f) _]
   (: erlang error (tuple 'missing_value 'foo f)))
  ([() i] i))

(defun field-update (field-inits def)
  (let ((i (field-loop field-inits (list_to_tuple def))))
    (tuple_to_list i)))

;; The foo record access macros.

(defmacro make fields
  (let ((def (list ''undefined ''undefined)))
    `(tuple 'foo ,@(field-update fields def))))

(defmacro match fields
  (let ((def (list '_ '_)))
    `(tuple 'foo ,@(field-update fields def))))

(defmacro is (rec)
  `(is_record ,rec 'foo 3))

(defmacro a (rec) `(element 2 ,rec))

(defmacro set-a (rec new)
  `(setelement 2 ,rec ,new))

(defmacro b (rec) `(element 3 ,rec))

(defmacro set-b (rec new)
  `(setelement 3 ,rec ,new))
```

We can now compile the module ``foo`` and call the macros. First we will call the ``foo:make`` macro to create an instance of ``foo`` and also show its expansion.

```lfe
> (c "foo")
(#(module foo))
> (macroexpand '(foo:make b 15 a 14))
(tuple 'foo 14 15)
> (set foo-rec (foo:make b 15 a 14))
#(foo 14 15)
```

While creating instances can be done with functions matching ``foo`` records must be done with macros. The ``foo:match`` macro does this for us. We will now use it and again also show its expansion.

```lfe
> (macroexpand '(foo:match b b-val))        
(tuple 'foo _ b-val)
> (set (foo:match b b-val) foo-rec)         
#(foo 14 15)
> b-val
15
```

The final example is the ``foo:is`` macro which tests whether its argument is a foo record.

```lfe
> (macroexpand '(foo:is foo-rec))   
(is_record foo-rec 'foo 3)
> (foo:is foo-rec)               
true
> (foo:is #(bar 34 35))
false
```

This will present a much more unified interface to modules, also making it easier to use macros as functions of a variable number of arguments. Including files containing macros will of course still be supported.

## ADDENDUM

LFE actually allows you to redefine the predefined macros. However, if you have done this you can still access the original predefined macros by calling them in the ``lfe`` module. For example ``(lfe:cond ...)`` will expand using the predefined ``cond`` macro even if it has been locally defined.

You can also define macros with the same names as the core forms but these will silently be ignored. There are limits to how much harm we allow you to do to yourself.

[^1]: To see the full expansion of the ``defrecord`` macro do ``(macroexpand '(defrecord foo a b))``.
