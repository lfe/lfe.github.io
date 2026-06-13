---
layout: post.liquid
title: "Bringing Types to LFE: Lessons from Haskell, the Typed Lisps, and Gleam"
description: "Why we started building a gradual type system for Lisp Flavoured Erlang — a tour through how Haskell, Go, Rust, Typed Racket, Coalton, and Typed Clojure each spell the same small idea, the ergonomic quibbles that pushed us, and the strategic moves we borrowed from Gleam."
permalink: "/blog/languages/2026/06/12/1600-bringing-types-to-lfe"
categories: ["languages"]
tags: [lfe, types, type-systems, gradual-typing, lykn, gleam, haskell, rust, racket, coalton, clojure, adts]
published_date: 2026-06-12 16:00:00 +0000
is_draft: false
data:
  author: oubiwann
  written_for: null
  last_validated: null
  cover_image: "/images/LFE_AbstractRabitHole_OTH_00257_.png"
  cover_alt: "Vigdís — abstract painting with a hint of a gateway in the near-distance"
  math: false
---

In the process of exploring a typing solution for LFE, something surfaced as a concept ... and this was that LFE and Erlang programmers
already do most of the work a type system would check for them — they just do it by
hand, by convention; without a net. We tag our tuples. We dispatch on
multi-clause function heads. We pattern-match on the shape of our data and trust that
the shapes line up. The discipline is *there*; it simply isn't *checked*. Recently, I've been building something that *does* check this convention, and I want to explain why —
and what I learned by observing the related works of others.

This realisation came as  someone of a relief. There's been a historical tendency for typing to be resisted or outright rejected in certain quarters of the Erlang world. The fact that, with the right frame of reference and the proper amount of squinting, the BEAM community already does this? Well, that makes working on a library like `lfe/typed` much easier :-D (And thanks to Gleam and other efforts, easier still!)

## A spark from a different language

The immediate spark came from somewhere unexpected: [Lykn](https://lykn.pl), a small
Lisp I built earlier this year that compiles a kernel language of S-expressions to clean JavaScript. After the first few releases of Lykn, it grew a macro-powered surface language, which in turn quickly
grew a feature I'd wanted for a long time — *contract-style function definitions*. This is
where a function's types live right at its boundary, woven into the same form as the
function itself rather than floating somewhere else in the file. Writing Lykn code, I
kept thinking: this is the feel I want when I write LFE. The types should sit at the
door, greeting you as you walk in, not pinned to a noticeboard down the hall.

So `lfe/typed` was born — an experiment in **gradually typed LFE with algebraic data
types, checked at compile time and enforced at runtime.** Gradual, because you opt in
where you want types and leave the rest of your LFE untouched; algebraic, because sum
types are the part of the type-theory toolbox the BEAM is already secretly built
around. But before writing a line of it, I went and read the neighborhood.

## One small idea, many spellings

Here is the idea I'll use as a measuring stick all the way through: a **sum type** with
two cases — a circle and a rectangle — and a single function that computes an area by
matching on the cases. It is the smallest example that exercises the two things a type
system has to be good at: *describing data that comes in alternatives*, and *making
sure you handled all of them*.

**Haskell** is so concise, it is almost *too* good:

```haskell
data Shape = Circle Double | Rectangle Double Double

area :: Shape -> Double
area (Circle r)      = pi * r * r
area (Rectangle w h) = w * h
```

The sum type is one line. The function's type is one line. The compiler will refuse to
build this if you forget a case. This is the platonic version; everything else is a
negotiation with the realities of a particular language. Mathematical ❤️.

**Rust** says the same thing with more punctuation and an `enum`:

```rust
enum Shape {
    Circle(f64),
    Rectangle(f64, f64),
}

fn area(s: &Shape) -> f64 {
    match s {
        Shape::Circle(r)       => std::f64::consts::PI * r * r,
        Shape::Rectangle(w, h) => w * h,
    }
}
```

Same shape, same exhaustiveness guarantee. The `match` is checked; drop the
`Rectangle` arm and the compiler stops you. Though I'm not a native curly brace speaker, I've grown to love Rust quite well over the past 8 years or so. I find this formulation very clear, very easy to read.

**Go** is the instructive example, though an odd one out, because Go *doesn't have sum types*. The
idiomatic move is an interface and a method per concrete type:

```go
type Shape interface{ Area() float64 }

type Circle struct{ Radius float64 }
func (c Circle) Area() float64 { return math.Pi * c.Radius * c.Radius }

type Rectangle struct{ Width, Height float64 }
func (r Rectangle) Area() float64 { return r.Width * r.Height }
```

This works, and Go programmers will tell you — correctly — that it's fine. But notice
what's quietly missing: there is no single place that says "a Shape is *either* a
circle *or* a rectangle, and nothing else." The set of shapes is open. Add a third
implementer in another package and nothing forces you to revisit the code that
consumes shapes. The compiler can't help you be exhaustive because, structurally, the
question "did I handle every case?" has no answer. Keep that in mind ...

## The same idea, in the Lisp world

The Lisp family of languages has been doing serious type-system work for years, and three efforts
are worth putting side by side.

**[Typed Racket](https://docs.racket-lang.org/ts-guide/)** is arguably the most mature gradually
typed Lisp (maybe even by a wide margin). Here's the same example:

```racket
(struct circle ([radius : Real]))
(struct rectangle ([width : Real] [height : Real]))
(define-type Shape (U circle rectangle))

(: area (-> Shape Real))
(define (area s)
  (match s
    [(circle r)      (* pi r r)]
    [(rectangle w h) (* w h)]))
```

It's all here: products (`struct`), a real union type (`U`), an exhaustive `match`, and
a checker that genuinely rejects programs. Typed Racket is excellent, and it taught the
whole field a great deal about how sound gradual typing really aught to work.

**[Coalton](https://coalton-lang.github.io/)** — the closest thing to "Typed Common
Lisp" — takes a different tack: it embeds a full, Hindley-Milner-typed, ML-flavored
language *inside* Common Lisp:

```lisp
(coalton-toplevel
  (define-type Shape
    (Circle    Double-Float)
    (Rectangle Double-Float Double-Float))

  (declare area (Shape -> Double-Float))
  (define (area s)
    (match s
      ((Circle r)      (* pi (* r r)))
      ((Rectangle w h) (* w h)))))
```

Its `define-type` is an actual algebraic data type, and the inference is the genuine
article. Coalton is gorgeous work — if you want ML's guarantees with access to the CL
ecosystem, it's remarkable.

**[Typed Clojure](https://typedclojure.org/)** (core.typed) is gradual typing as an
annotation layer over a language that has neither native sum types nor pattern
matching, so the example has to be assembled from records, a union alias, and explicit
dispatch:

```clojure
(t/defalias Shape (t/U Circle Rectangle))

(t/ann area [Shape -> Number])
(defn area [s]
  (cond
    (instance? Circle s)    (* Math/PI (:radius s) (:radius s))
    (instance? Rectangle s) (* (:width s) (:height s))))
```

It's a real, ambitious system — checking idiomatic Clojure can be tricky; typing on top of that is genuinely hard, but
Typed Clojure did an incredible job.

## Our (small, respectful) objections

I want to be careful here, because every one of these systems is more battle-tested
than ours. We have definitely *learned* from all of them, and formed opinion about them, but we have not mastered them. Our opinions and objections aren't much more sophisticated than an armchair enthusiast's, and are almost entirely matters of *ergonomics*,
not of soundness. But, they're exactly the itch Lykn had already scratched. And, as UX, they can make or break one's early experience of the system.

Caveats aside: in each Lisp case above, **the type information lives apart from the code it describes.** Typed
Racket's `(: area ...)` is a separate form that floats above the `define`; the name is
repeated, and the signature and the body can drift apart. Coalton's `declare` is
likewise detached from its `define`, and the whole typed world lives inside a
`coalton-toplevel` island — you are either *in* Coalton or *in* Common Lisp, and the
seam shows. Typed Clojure's `t/ann` is an annotation bolted on from the outside,
necessarily, because the host language has no place to put a type. (LFE has a nearly identical problem + solution, in fact).

None of that is wrong. But coming off of Lykn, I wanted the opposite reflex: the types
should feel like a *part of the definition*, at the boundary where the values cross, with no
separate noticeboard and no separate world to step into. A function's contract should
read like part of the function.

Okay, that's the ergonomics - on to some other interesting bits.

## What we wanted to avoid — and what Gleam got right

The other half of the problem I faced was strategic, not syntactic, and here the teacher was
[Gleam](https://gleam.run/). Gleam is a young, statically typed language for the BEAM
that has, in a remarkably short time, done almost everything right. Intrigued, we read. We learned. And ultimately we mostly tried to copy its judgment — as a template for good decision-making.

A few of its moves became principles for us:

- **Adoption follows interop, not rewrites.** Gleam compiles to the BEAM and calls
  Erlang and Elixir freely; you don't have to abandon your ecosystem to use it. We took
  this further: `lfe/typed` isn't even a new language. It's a library and a build step (echos of Typed Clojure).
  You keep writing LFE; the output is eventually ordinary BEAM bytecode, and anything on the
  BEAM can call it. There is no "different LFE" to adopt. (Not as a language implementation; tooling and static analysis have caused us to use a new file extension for now, `.lfet`; more on that in a future post)
- **Friendly errors are not polish; they're the product.** Gleam treats its compiler's
  output the way Elm does — as a teaching surface — and adoption may track that friendliness
  at nearly one-to-one. On general pedagogical groups, we made teaching-quality diagnostics one of our two
  non-negotiable goals: errors clear enough that a person *or* a language model can turn
  a mistake into correct, typed LFE just by reading them. By itself, that move is eccentric; with Gleam as the exemplar, that move is evidence-based :-)
- **Write the checker in a typed language you can evolve safely.** Gleam's compiler is
  Rust. So is our type checker. A type checker is nothing but metaprogramming, and metaprogramming
  in an untyped implementation language is a quiet way to accumulate the very bugs
  you're trying to help others avoid. (The cautionary tale here is
  [Alpaca](https://github.com/alpaca-lang/alpaca), an earlier and genuinely lovely
  statically typed BEAM language that lost momentum — a reminder that the *implementation*
  has to be as maintainable as the language is elegant.)
- **A type checker should *reject* programs.** This is the line that separates us from
  [Dialyzer](https://www.erlang.org/doc/apps/dialyzer/dialyzer.html), which is wonderful
  but, by design, *success-typed*: it only complains about code that can never succeed,
  and it tends to stay silent on key things that are important for static analysis. It describes; it does not prescribe. We wanted
  a checker that says *no* — and while saying that kindly, definitely very firmly!

Put those together and you get this version of the example we were after:

```lisp
(deftype/typed shape
  (Circle    (radius float))
  (Rectangle (width float) (height float)))

(defun/typed area
  :args ((s shape))
  :returns float
  :body (case/typed s
          ((Circle r)      (* (math:pi) (* r r)))
          ((Rectangle w h) (* w h))))
```

The data declaration names its alternatives. The function's contract — `:args`,
`:returns` — sits at the door, in the same form as the body. The `case/typed` is
checked for exhaustiveness; forget the `Rectangle` clause and the checker stops you,
by name. It reads, I hope, like LFE that simply learned to keep its promises.

## Where we are right now

This is an active experiment, not a product — but it has come surprisingly far. As of
today, `lfe/typed` does all of the following, end to end, from LFE source to BEAM:

- **Algebraic data types** with named-field constructors and type parameters, over
  *pluggable representations* — tagged tuples (the portable default), native records on
  OTP 29+, enum atoms for all-nullary sums, and zero-cost transparent newtypes — proven
  equivalent by a cross-backend test matrix.
- **Exhaustive pattern matching** that rejects non-exhaustive `case/typed` and names
  every missing constructor (we hope?).
- **Bidirectional contract checking** — body-against-`:returns`, call arguments, and
  constructor field values.
- **Always-on runtime guards** plus generated deep validators and a `decode` membrane
  for the boundary where untyped data crosses in, all producing structured, teaching-
  grade type errors.
- **Typed records** (`defrecord/typed`) with generated, type-aware constructors,
  accessors, and functional updaters.
- **Cross-module type references** (`mod:type` and `import-types`) with project-wide
  scanning.
- **Multi-clause typed functions** via a unified form where a function's argument
  *names* are simply the trivial case of *patterns* — so a single construct gives you
  both value dispatch and type dispatch, with every clause checked against its own
  contract and its `when` guards composed with the generated type guards.
- A faithful, independently-validated Rust port of LFE's macro expander running under the
  hood, so real LFE — `cond`, `let*`, `do`, `defrecord`, backquote — compiles correctly
  through the typed pipeline.

The diagnostic engine renders all of this as Gleam-grade errors with source spans and
carets, in both human-readable and JSON form. Today that's backed by 100 Rust tests (a small but growing number) and
ten suites of LFE Common Test. So far, it runs green.

## What's next

Two fronts are open, and both are the fun kind.

The first is **hardening against reality**: we're taking the type system to a real,
pre-existing LFE library — code we didn't write to flatter the checker — and letting it
tell us where the design is still too clever or too rigid. That's the moment a research
experiment either grows up or learns something humbling, and we want both outcomes on
the table. There's also a genuinely deep design thread we're pulling on around
*overloaded* typed functions — letting different clauses carry different type
signatures, which walks straight into the lovely, thorny territory of intersection
types. More on that when it's earned its keep.

The second is the one I'm most excited to show you: **a new REPL.** It runs ordinary
LFE exactly as you'd expect — but it will also speak Typed LFE, and when it does, it will bring
the type checker's teaching-grade error reporting *into the interactive session*.
Imagine fat-fingering a constructor at the prompt and getting back not a tangled stack
trace from somewhere deep in the runtime, but the same calm, span-and-caret explanation
the compiler would give you — pointing at exactly what you typed, naming exactly what
went wrong, suggesting exactly the fix. The REPL has always been where Lisp does its
best thinking out loud; we'd like it to be where Typed LFE does, too.

We've got a series of posts planned for diving into Typed LFE in detail, including one that goes over our architectural decisions in detail (most of which rest upon the single most important directive: keep LFE proper stable and functional). If you're interested have having a poke-about, our code is here: [github.com/lfe/typed](https://github.com/lfe/typed).
