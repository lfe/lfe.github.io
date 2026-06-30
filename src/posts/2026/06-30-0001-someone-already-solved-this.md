---
layout: post.liquid
title: "Teaching LFE to Format Itself, Part 2: Someone Already Solved This"
description: "Looking for a principled way to decide layout, we found Racket's raco fmt and the 2023 paper beneath it — a provably optimal pretty-printer. It was exactly what we needed, with exactly one problem: it is slowest precisely on the S-expressions a Lisp formatter sees most."
permalink: "/blog/languages/2026/06/30/0001-someone-already-solved-this"
categories: ["design"]
tags: [lfe, erlang, beam, formatter, pretty-printing, pretty-expressive, raco-fmt, racket, rust, research, languages]
published_date: 2026-06-30 00:01:08 -0500
is_draft: false
data:
  author: oubiwann
  last_validated: null
  cover_image: "/images/LFE_Formatter_00476_.png"
  cover_alt: "Vigdís — an oil painting of a lighthouse on a rocky outcropping sweeping it's light across a storm-tossed sea"
  math: false
---

> **Teaching LFE to Format Itself** — part 2 of a five-part series on the design
> and construction of an LFE code formatter. This part goes looking for a
> principled way to *decide* layout, finds Racket's `raco fmt` and the 2023 paper
> beneath it, and hits the one benchmark row that reframed the whole project. The
> series runs: a hand-written formatter outgrows itself (1), the search for a
> principled engine finds one at the research frontier (2), a deep-dive derives an
> original contribution of our own (3), the scattered work gets a single home (4),
> and we build the real thing and measure it (5).
>
> **Formatter Series:**
>
> * [1 — A Hand-Written Formatter for LFE](/blog/design/2026/06/28/1100-a-hand-written-formatter) · 2 · *(parts 3–5 forthcoming)*

[Part 1](/blog/design/2026/06/28/1100-a-hand-written-formatter) ended
on a cliff-hanger. Our story so far: the hand-written formatter inside `rebar3_lfe` worked, but it had
hit a wall: it was *deciding* layout — when to break a line, where to align the
continuation, how to spend the eighty columns it was given — greedily, one form at
a time, in a thousand-plus lines of Erlang conditionals. And the convention those
conditionals encoded, the thing that *is* LFE's house style, was trapped in code
when it wanted to be data.

So I went looking for the thing I was sure must exist: a *principled* engine. Not a
bag of heuristics. I was hoping for something that, when handed a document and a width, would make all
those layout decisions *optimally* — and would let me describe LFE's conventions
cleanly on top, as rules rather than as machinery. I didn't expect to find it
fully built. I certainly didn't expect to find it built for another Lisp, sitting
on top of a freshly-minted piece of programming-languages research, with a problem
attached that would eat the next few weeks of my evenings.

## Greedy isn't enough

The BEAM already *has* pretty-printers. Erlang's `prettypr`, the
algebra inside [`erlfmt`](https://github.com/WhatsApp/erlfmt) (`erlfmt_algebra`),
Elixir's `Inspect.Algebra` — these are real, battle-tested, and they all descend
from the same beautiful lineage: the Wadler/Leijen pretty-printing algebra. If
you've ever read Wadler's "A prettier printer," you know the story: you build
a document out of combinators, and the key one is `group`: a group says "lay this
out flat on one line if it fits; otherwise break it." The printer walks the
document and, at each group, makes a *local* decision — *does the flat version fit
in the space I have left?* — and commits to it.

That local, greedy decision is exactly why the family is fast (linear or near-it),
and it's exactly why it wasn't a perfect match for what I envisioned with LFE. A `group` offers the printer one
choice, and only one: this whole thing flat, or this whole thing broken. But the
layouts a Lisp formatter wants to choose between aren't always "all flat" versus
"all broken." Racket's formatter makes this point precisely: a single function
application has *three* distinct styles a human might want, not two. And the
decision of which to use can't always be made by looking at the current form in
isolation — the *right* break here might depend on what breaking does three lines
down. A greedy printer, by construction, doesn't usually see three lines down (it has already committed by then).

You *can* fight this ... the first `rebar3_lfe` formatter fought it
(exhaustingly), by hand-coding the fitting logic for every form. But that is the
1000+-line wall from Part 1. The clean, declarative knowledge layer I wanted — "a
`defun` looks like *this*; an `export` looks like *that*" — only stays clean if the
engine underneath can do three things the Wadler family does't:

* offer an **arbitrary choice** between *any* two layouts, not just flat-versus-broken of a single group;
* take its **optimality objective as data** I supply, rather than baking "fewest lines that fit" into the algorithm;
* and pick the layout that's **globally** cheapest across the *whole* document, not the one that looked best locally.

Strip any of those away and the per-form rules slump back into imperative fitting
decisions — back toward the formatter I was trying to retire. I didn't have a name
for the engine that did all three. It turned out someone had not only named it but
proved it correct.

## Expressive *and* optimal: a 2023 paper

The trail led to `raco fmt`, the code formatter for Racket. (`raco` is Racket's
command-line tool; `raco fmt` is its `gofmt`.) And underneath `raco fmt` sat a
paper I hadn't read:

> Sorawee Porncharoenwase, Justin Pombrio, and Emina Torlak. **"A Pretty Expressive
> Printer."** *Proc. ACM Program. Lang.* 7, OOPSLA2, Article 261 (October 2023).

The authors call their printer **Πₑ** ("Pi-e"), and its implementation
PrettyExpressive. The one-sentence pitch from the abstract is almost cheeky in its
confidence: Πₑ targets a document language "strictly more expressive than all
pretty printers in the literature" and "provably minimizes an optimality
objective" — and they back the "provably" with a full correctness proof in the
[Lean theorem prover](https://lean-lang.org/). This isn't a heuristic that
usually does the right thing. It's an algorithm that returns the cost-optimal layout and has a machine-checked proof that it does.

It was, point for point, the three things on my list.

**Arbitrary choice.** Πₑ's document language — the paper calls it Σₑ — has just
seven core constructors, and everything else is sugar built from them:

```
d ::= text s          -- a literal string, no newline
    | nl              -- a newline (becomes a single space when flattened)
    | d <> d          -- concatenate (the right starts where the left ended)
    | nest n d        -- indent by n, relative
    | align d         -- set the indent to the current column
    | flatten d       -- turn every nl inside d into a space
    | d <|> d         -- choose the cheaper of two layouts
```

That last one, `<|>`, is the whole game. Where Wadler's `group` is a *fixed* choice
("flat or broken"), `<|>` lets you offer the printer *any* two documents and say
"pick whichever ends up cheaper." Wadler's `group d` is then just a special case —
you can define it as `d <|> flatten d`, "the document, or the document with every
break squashed flat." Once you have arbitrary choice, the familiar combinators fall
out as one-liners, and so do the ones the old formatter never had a clean way to
express.

**The objective as data.** Πₑ doesn't have "fewest lines" wired into it. Instead it
takes a *cost factory*: you tell it what "cost" means, and it minimizes that. The
factory is a little algebra — give me the cost of placing a string of length `l`
starting at column `c`, give me the cost of a newline, tell me how to add two costs
and how to compare them — and Πₑ does the rest. The default factory the Racket
formatter ships is the one most people would reach for: cost is a pair, *(badness,
height)*, where *badness* is the sum of the squared overflow past the width and
*height* is the number of lines, compared so that you never overflow if you can help
it and, among layouts that don't, you take the shortest. But that's a *choice*,
expressed as data, that the caller can swap. Want a soft width limit, or to count
lines only, or to gently prefer one of two equally-wide layouts? That's a different
factory, not a different algorithm. This is the same idea as Part 1's punchline,
relocated into the cost domain: the thing that varies — here, the very definition of
"pretty" — wants to be *data the engine consumes*, not logic baked into the engine.

**Global optimality.** And it minimizes that cost over the *entire* document. Not a
local "does this fit?" check at each group, but the genuinely cheapest layout of the
whole tree.

If you'd handed me a wishlist a month earlier (and I'd done the reading then), this would be it, down to the punctuation.

## How it can possibly be fast: Pareto frontiers

The obvious objection — the one you're forming right now — is that this sounds
catastrophically expensive. A document with choices in it has exponentially many
possible layouts; `<|>` everywhere means an astronomically branching search. If the
engine actually renders every candidate to find the cheapest, it dies on contact.

The clever idea at the heart of Πₑ — and the part worth carrying with you even if
you forget everything else — is that *it never renders during the search.* Instead,
it summarizes each sub-document by a small set of **measures**. A measure captures
just enough about a candidate layout to keep comparing it: the width of its last
line, and its cost. The trick is that most candidate layouts are simply *worse* than
some other candidate on both counts — wider last line *and* higher cost — and those
can be thrown away immediately, because nothing you build on top of them will ever
rescue them. What survives is the set of layouts where you can't improve one
dimension without sacrificing the other: the **Pareto frontier**.

So as Πₑ walks the document, each sub-tree resolves not to a layout but to its
Pareto frontier of measures — typically a handful of entries — and combining two
sub-trees means combining two small frontiers and pruning the losers. At the very
top, the cheapest measure on the final frontier *is* the optimal layout, and only
then does the engine reconstruct the actual text. The exponential search collapses
into bookkeeping over short lists.

This is what `raco fmt` is built on, and reading its conventions file is a small
joy after the old Erlang formatter: form definitions express themselves in `alt`
(choice), `cost` (a nudge to the objective), and even a `fail` document — a layout
that can never be chosen, used to *forbid* an option — composed together
declaratively. The layout knowledge reads like a description of the style, because
the engine underneath is carrying the weight the old formatter carried by hand.

I want to be careful to credit this properly: Πₑ, Σₑ, the cost factory, the Pareto
construction, the proof — all of that is Porncharoenwase, Pombrio, and Torlak's
work. What I'm narrating is the experience of *finding* it and realizing
it was the engine LFE needed. The original contribution in this series, when it
comes (Part 3), is a much smaller and much less certain thing built in a gap this paper leaves open for us.

## A Rust aside, for later

One detail I filed away for later, and I'll plant it here so it pays off in Part 5.
The ideas in this paper don't only live in Racket. The authors' own reference
implementations are in Racket and OCaml, and there's a faithful **Rust** port of the
algorithm published as the [`pretty-expressive`](https://crates.io/crates/pretty-expressive)
crate. Two independent implementations of the same proved-correct algorithm is a
gift to anyone building a *third*, because it means I can render the same document
through someone else's engine and check that mine agrees — not on the bytes
necessarily, but on the one thing the two are specified to share: the reported
optimal *cost*. When we get to building the BEAM engine, that Rust port becomes a
differential oracle keeping us honest, and Rust is where that oracle lives. (More on this later.)

## The benchmark row

And then I read the evaluation section, and our project got a cold shower.

Πₑ's time complexity is **O(n·W⁴)** — where `n` is the size of the document and `W`
is a "computation width limit" roughly tied to the page width. On paper that's
fine; it's *polynomial*, better than several classic printers, and the authors
rightly present it as a strength. But buried in their own benchmark table is a row
that stopped me cold. The paper measures every printer against a battery of
documents, and among them is a family called **SExpFull** — complete binary trees
printed *as S-expressions.* Here is how PrettyExpressive does on the two largest,
against the greedy Wadler/Leijen-style printers, at the default width:

| Document | PrettyExpressive | Greedy (Wadler/Leijen) |
|---|---|---|
| SExpFull15 (≈4,100 lines) | **3.03 s** | 0.045 s |
| SExpFull16 (≈8,200 lines) | **5.26 s** | 0.091 s |

Crank the width limit up to 1000 and PrettyExpressive's numbers climb to 5.4 s and
14.2 s; the greedy printers don't move. The paper says it plainly, in a sentence I
read through tears: PrettyExpressive "performs poorly on SExpFull relative to other
printers."

Now, to not dwell inordinately on the drama, Lisps aren't *strictly* s-expressions. ASTs are, XML (with parens instead of brackets) is. Those can be very deeply nested and thus VERY wide. For almost every language a
formatter targets, deeply-nested S-expressions are a pathological corner case — a
stress test you run to find the cliff, not a thing real source code looks like. The
authors include it precisely *because* it's a worst case. I just needed to prove to myself that LFE wasn't a pathological case.

## The reframe: is the W-factor even real?

So that's where it gets interesting, and, as fate would have it, where this post hands off to the next two. But, before this part draws to a close:

I went back to that complexity bound — O(n·W⁴) — and asked the naïve question:
*where does the W⁴ actually come from?* And the paper, to its great credit, half-
answers this itself. In a footnote to the very evaluation that worried me, the
authors note that the W⁴ worst case "happens only if Pareto frontiers are always
full." In practice they aren't: increasing W tenfold, they observe, does *not*
multiply the running time by ten-thousand — "on the contrary, increasing W does not
affect the running time at all on most benchmarks." The slowness on SExpFull, they
say, is mostly memory pressure from memoization, not the W⁴ term biting.

Read that twice and a door opens. The W-factor isn't a law of the problem. It's an
artifact of a *representation choice.* Πₑ keeps each Pareto frontier as a list of
concrete measures, in effect *sampling* the relationship between "how much room is
left" and "what it costs" at up to `W`-plus-one discrete columns. The frontier can
hold up to `W+1` entries because it's a table of samples, one per possible column.
But a Pareto frontier of *cost versus width* isn't an arbitrary scatter of points —
it has shape. What if, instead of sampling that shape at every column, you carried
it *symbolically* — as the function it actually is? And — the load-bearing, scary
part — what if real Lisp source is structurally *tame* enough that this symbolic
form stays small, so the page-width factor mostly evaporates for the documents we
actually emit?

That question wouldn't leave me alone, and chasing it became a piece of research of
its own — which is **Part 3** of this series. I'll say this clearly now, because it
matters: **Part 3 is an optional deep-dive.** It's the long, math-heavy one, a slow
derivation of that symbolic frontier with worked numbers and disclaimers at every
step. If you skip it, the series will still make sense (and you might be better off). If you do, here's the bit you need for the rest: *for the narrow fragment of layouts that real
LFE actually emits, the width factor appears to vanish — but that's a bet we later
put to the test (Part 5), not a theorem.* Everything after Part 3 treats it exactly
that way -- as a hypothesis with a measurement pending, not a settled result.

## What's next

So the discovery left me standing at a fork, with two roads from which to select. Having adored the "Choose Your Own Adventure" books as a kid, I did what came naturally: both forks!

The **pragmatic** road: just port Πₑ to the BEAM as the authors designed it,
width-sampling and all, and *measure* whether it's fast enough on real LFE — real
modules of a few hundred lines, not 8,000-line synthetic blow-ups. Maybe the scary
benchmark never bites in practice. The only way to know is to build the simple thing
and put a stopwatch on it. That road is Parts 4 and 5.

The **research** road: chase the symbolic frontier, try to make the W-factor
disappear by construction, and find out whether "real Lisp is tame" is true or just
a comforting story I told myself at midnight. That road is next :-)
