---
layout: page.liquid
title: Learn
permalink: "/learn/"
data:
  long_description: From first taste to deep mastery — follow your curiosity at the pace that suits you.
  long_title: Learn LFE
---


## What does LFE look like?

You saw the basics on the [home page](/) — pattern matching, data types, macros.
Here’s what those ideas look like when they start working together:

```lisp
;; Erlang tuples + Lisp pattern matching = expressive dispatch
(defun handle
  ((`#(ok ,result))    (io:format "Success: ~p~n" `(,result)))
  ((`#(error ,reason)) (io:format "Failed: ~p~n" `(,reason)))
  ((‘shutdown)         (io:format "Shutting down.~n" ‘())))

lfe> (handle #(ok 42))
Success: 42
ok
lfe> (handle #(error "timeout"))
Failed: "timeout"
ok
```

Four lines of code, three clauses, zero conditionals. The shape of the data
*is* the control flow.

* [The LFE Machine Manual](https://cnbbooks.github.io/lfe-manual/) — the comprehensive guide to LFE
* [About LFE](https://cnbbooks.github.io/lfe-manual/part1/intro/about.html) — what it is and where it comes from

## Getting started

From zero to a running REPL in under five minutes.

```lisp
lfe> (defun hello (name)
       (io:format "Hello, ~s!~n" `(,name)))
hello

lfe> (hello "world")
Hello, world!
ok

lfe> (lists:map #’hello/1 ‘("LFE" "Erlang" "OTP"))
Hello, LFE!
Hello, Erlang!
Hello, OTP!
(ok ok ok)
```

Pick the path that suits you:

* [Quick Start with rebar3](https://cnbbooks.github.io/lfe-manual/part1/intro/setup.html) — Erlang 21+ and `rebar3` are all you need
* [The LFE Tutorial](https://cnbbooks.github.io/lfe-tutorial/) — Erlang’s "Getting Started" translated into LFE
* `docker run -it lfex/lfe` — no install, instant REPL

## The language

Types, records, and pattern matching aren’t isolated features — they compose.

```lisp
(defrecord person name age role)

(defun greet
  (((match-person name n role ‘admin))
   (io:format "Welcome back, ~s (admin)~n" `(,n)))
  (((match-person name n))
   (io:format "Hello, ~s~n" `(,n))))

lfe> (greet (make-person name "Ford" age 234 role ‘admin))
Welcome back, Ford (admin)
ok
```

Records generate constructor and accessor functions. Pattern matching
destructures them directly in function heads. No casting, no null checks.

* [Casting SPELs in LFE](https://cnbbooks.github.io/lfe-casting-spels/) — learn by building a game, from records through to `gen_server`
* [LFE Examples](https://github.com/lfe/lfe/tree/develop/examples) — dozens of working examples in the LFE repo
* [LFE on Rosetta Code](https://rosettacode.org/wiki/Category:LFE) — 99+ problems solved in LFE
* [The LFE Machine Manual — Part II: Data Types](https://cnbbooks.github.io/lfe-manual/part2/README.html) — the full treatment

## OTP

This is what makes the Erlang VM worth the trip. Processes that supervise
each other, restart on failure, and scale to millions of concurrent
connections.

```lisp
;; The gen_server from the home page, now in action:
lfe> (my-server:start_link)
#(ok <0.42.0>)

lfe> (my-server:get-amount)
0
lfe> (my-server:deposit 100)
ok
lfe> (my-server:get-amount)
100
```

Behind those four calls: a supervised process, a message queue, serialized
state updates, and crash recovery. OTP gives you all of it.

* [Learn You Some Erlang for Great Good!](https://learnyousomeerlang.com/) — the best (and most fun) introduction to OTP
* [Designing for Scalability with Erlang/OTP](https://www.oreilly.com/library/view/designing-for-scalability/9781449361556/) — the thinking person’s OTP book
* [Erlang and OTP in Action](https://www.manning.com/books/erlang-and-otp-in-action) — builds an OTP application piece by piece
* [The LFE Machine Manual — Part V: OTP](https://cnbbooks.github.io/lfe-manual/part5/README.html) — OTP in LFE specifically

<div class="content-card">

### The Erlang heritage

* [A History of Erlang](/papers/%5B2007%5D%20Armstrong%20-%20HOPL%20III%20A%20History%20of%20Erlang.pdf) — Joe Armstrong’s HOPL III paper
* [The Erlang Rationale](/papers/%5B2008%5D%20Virding%20-%20The%20Erlang%20Rationale.pdf) — Robert Virding on the "why" of Erlang
* [Making reliable distributed systems in the presence of software errors](/papers/%5B2003%5D%20Armstrong%20-%20Making%20reliable%20distributed%20systems%20in%20the%20presence%20of%20software%20errors.pdf) — Joe Armstrong’s PhD dissertation
* [Adopting Erlang](https://adoptingerlang.org/) — taking Erlang/LFE into production
* [Stuff Goes Bad — Erlang in Anger](https://erlang-in-anger.com/) — debugging production BEAM systems

</div>

## Going deeper

Macros, metaprogramming, the Lisp heritage that shaped LFE, and the
operational wisdom to run it in production.

```lisp
;; The home page showed a variadic `mean` macro.
;; Here’s one that writes a gen_server call wrapper:
(defmacro defcall
  ((name args body)
   `(defun ,name ,args
      (gen_server:call (whereis ‘my-server) ,body))))

;; One line generates a complete API function:
(defcall get-amount () ‘amount)
(defcall deposit (n) `#(add ,n))
```

Macros run at compile time — the generated code is exactly what you’d
write by hand, with zero runtime overhead.

<div class="content-card">

### The Lisp heritage

LFE derives much of its character from Robert Virding’s experiences with
[Franz Lisp](https://en.wikipedia.org/wiki/Franz_Lisp) (which itself was based in large part upon [MACLISP](https://en.wikipedia.org/wiki/Maclisp)) at Stockholm University'a physics department and then again later at the Ericsson Computer Science Lab. Due to the MACLISP influence, The [Moonual](http://www.softwarepreservation.org/projects/LISP/MIT/Moon-MACLISP_Reference_Manual-Apr_08_1974.pdf) (David Moon’s MACLISP reference), the [Chineual](https://www.bitsavers.org/pdf/mit/cadr/chinual_3rdEd_Mar81.pdf), and the [Common Lisp HyperSpec](http://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm) remain constant companions in LFE’s development.

For more details on Lisp macros, be sure to check out the following:

* [Lisp in Small Pieces](https://www.cambridge.org/core/books/lisp-in-small-pieces/66FD2BE3EDDDC68CA87D652C82CF849E) — the definitive account of Lisp implementation
* [Let Over Lambda](https://letoverlambda.com/) — 50 years of Lisp, focused on the power of macros

</div>

## Next steps

Ready to build something? The [Use](/use) page has the reference materials
and tooling you’ll need. And the [community](/community) is always happy
to help.
