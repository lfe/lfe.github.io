---
layout: post.liquid
title: "Teaching LFE to Format Itself, Part 1: A Hand-Written Formatter"
description: "The origin of LFE's code formatter — the hand-written engine that shipped inside rebar3_lfe, the one genuinely hard BEAM-specific obstacle (comments), and the maintainability wall that taught us we'd been writing the formatting convention as code when it wanted to be data."
permalink: "/blog/design/2026/06/28/1100-a-hand-written-formatter"
categories: ["design"]
tags: [erlang, beam, formatting, pretty-printing, rebar3, tools, languages]
published_date: 2026-06-28 11:00:00 +0000
is_draft: false
data:
  author: oubiwann
  written_for:
    lfe: "2.2.1"
    erlang: "29"
  last_validated: null
  cover_image: "/images/LFE_Formatter_00424_.png"
  cover_alt: "Vigdís — a typesetter's bench buried under hand-cut type and an overgrown rulebook, oil and digital"
  math: false
---

> **Teaching LFE to Format Itself** — part 1 of a five-part series on the design
> and construction of an LFE code formatter. This part is the origin story: the
> hand-written formatter that lived inside `rebar3_lfe`, what it got right, and
> the wall it hit. The series runs: a hand-written formatter outgrows itself (1),
> the search for a principled engine finds one at the research frontier (2), a
> deep-dive derives an original contribution of our own (3), the scattered work
> gets a single home (4), and we build the real thing and measure it (5).
> *(Links to parts 2–5 will appear here as they publish.)*

Yesterday I tagged [`lfe/fmt`](https://github.com/lfe/fmt) 0.4.0. This afternoon
I released `rebar3_lfe` 0.5.6, which pulls that release in as a dependency — and
with it, for the first time, LFE can format its own source code from the command
line, outside of Emacs:

```shell
rebar3 lfe format
```

That's a small sentence with a long history behind it. The release that turned
the feature on is also the release that *deleted* the formatter we'd been living
with for the past several weeks — close to three thousand lines of Erlang,
quietly retired in a single commit, swapped out for a one-line call into the new
engine. Watching a pile of code you wrote get removed is a strange feeling. It's
also, when the replacement is genuinely better, one of the most satisfying things
in this line of work.

So before this post series gets to the new engine, I want to spend a post on the old
one: there are some good lessons there. Every formatter
starts as a pile of special cases, and ours worked well enough to ship and badly
enough to show us *exactly* what we actually needed.

## Why a Lisp wants a formatter at all

There's a reflexive objection to formatting a Lisp, and it goes like this: *the
editor already indents it for you.* Lisp's whole shape is its parentheses; a
half-decent editor matches them and indents the body, and you're done. Why build
a `cargo fmt` or a `gofmt` for a language that's basically pre-formatted?

The objection misses what a formatter is *for* in a shared codebase. Editor
indentation is a personal convenience — it helps *you*, while you type, in your
configuration. A formatter is a *social* tool. It exists so that a diff in code
review shows only what changed in meaning, not where somebody's editor decided to
put a paren. It exists so a project can put a `--check` step in CI and have the
machine, not a reviewer, enforce house style. It exists so that two contributors
with two different editors and two different opinions about how a `let` should
break produce byte-identical output. The lineage here is `gofmt`, then
[`erlfmt`](https://github.com/WhatsApp/erlfmt) for our nearer neighbours on the
BEAM — and, much older than either, the indentation table that has shipped in
[`lfe-mode`](https://github.com/lfe/lfe)'s `lfe-indent.el` for as long as LFE has
had Emacs support. Hold onto that last one. It comes back at the end of this
post, and it turns out to be the most important character in it.

LFE had the Emacs half of that story for years. What it didn't have was the
command-line half — the canonical, automatic, CI-checkable layout that the rest
of the BEAM world takes for granted. The `format` provider in `rebar3_lfe` was
the first serious attempt to close that gap.

## The first design: full reflow

The original ambition was the most ambitious one available: **full reflow.** The
formatter would own the layout completely. Whatever you typed — however you'd
broken your lines, however you'd indented — it would throw all of that away,
reduce your code to its abstract structure, and re-impose canonical shape from
scratch. The way you wrote it didn't matter; only what it *meant* did.

This is the `gofmt` dream, and it's a good enough place to start, because it
makes a promise that nothing else does: there is exactly *one* correct layout for
any given program, and the formatter will produce it. No negotiation, no
preserved idiosyncrasies, no "well, it depends." Feed in any of the infinitely
many ways to type a function and get back the one true form.

We built toward that. And then reality made us walk part of it back — which turns
out to be the most interesting design decision in the whole first attempt.

## The knowledge-gated model

Here's the idea I'm still fond of:
The formatter treats every form one of two ways, depending on whether it
*understands* that form.

For **known forms** — the special forms (`if`, `let`, `case`, `cond`, `receive`,
`try`, …), the def-forms, maps, match clauses — the formatter **owns the layout**.
It knows what these are supposed to look like, so it imposes the canonical shape
and freely adds or fixes line breaks. An `if` you wrote flat on one line gets
broken across several, because the house style is that `if` always breaks:

```lfe
;; as you might type it
(if (> x 0) 'positive 'non-positive)

;; as the formatter renders it
(if (> x 0)
  'positive
  'non-positive)
```

A `let` you broke in some idiosyncratic way gets re-laid-out into the canonical
one. Your line breaks inside a known form are *not* preserved — they're
re-derived from the rules. This is full reflow, and for the forms the formatter
understands, it works.

For **unknown forms** — plain function calls, the macros that records expand into
(`make-person`, `match-person`, and friends), user-defined macros — the formatter
does something humbler. It **preserves the breaks you wrote.** It will normalise
your indentation (continuation arguments align under the first argument) and it
will wrap anything that runs over 80 columns, but it will not second-guess *where*
you chose to break the form.

Why the retreat from full reflow? Because the grouping inside an unknown form
often encodes *intent the formatter cannot recover.* Consider a record-style
constructor that takes alternating key/value pairs:

```lfe
(make-config host "localhost"
             port 8080
             tls  false)
```

To a formatter that doesn't expand macros, that's just a seven-element call with a
symbol head. It has no idea that the arguments come in pairs, that `host` goes
with `"localhost"` and `port` with `8080`. Full reflow would happily "canonicalise"
it into something that destroys the very structure that makes it readable —
maybe one argument per line, maybe greedily packed to the margin, but in any case
*wrong*, because the formatter was reasoning about a shape it couldn't actually
see. Without macro expansion and a populated compile-time environment, that intent
is simply unavailable.

So the design draws a line it can defend: *impose canonical layout exactly as far
as you genuinely understand the form, and preserve the author's intent everywhere
you don't.* The cost is honest and stated plainly in the design doc — output is
canonical for known forms and author-dependent for unknown ones — but one
invariant holds across the whole thing: it's **idempotent.** Run the formatter on
its own output and nothing moves. That property is the floor under everything; a
formatter that isn't idempotent isn't a formatter, it's a random walk.

## The one hard BEAM-specific obstacle: comments

If you want to know where the real work in a formatter hides, the answer is almost
always: comments. And on the BEAM, comments handed us the single hardest
obstacle of the whole project — one that's worth slowing down for, because it's a
perfect illustration of how a formatter's job differs from a compiler's.

Here's the problem. LFE's own scanner, `lfe_scan`, **throws comments away.** This
is entirely correct behaviour for a *compiler*. A comment has no runtime meaning;
it's whitespace with opinions. By the time the compiler is building a parse tree,
the comments have done their job for the human reader and are simply noise to the
machine. So the scanner drops them, the parser never sees them, and the abstract
syntax tree the rest of the toolchain works with has no idea they ever existed.

For a compiler, that's a feature. For a formatter, it's a crisis. A
formatter that "preserves" your code by silently deleting every comment is not a
tool anyone will run twice. The entire point is to reshape the layout while
keeping everything that matters — and comments matter enormously.

This is the fundamental divide. A compiler is a *lossy* reader on purpose: it
keeps meaning and discards everything else. A formatter must be a *lossless*
reader: it has to retain the things that carry no meaning to the machine but
carry plenty to the human — comments, yes, but also blank lines, and the
distinction between the brackets and parens you chose. The standard reader is
built for the compiler's job, which means a formatter can't use it.

So we couldn't reuse `lfe_scan`. We had to write our own. The result was
`r3lfe_format_lexer.erl`, a tokenizer whose whole reason for existing is in its
opening lines:

```erlang
%%%% Lossless, comment-preserving tokenizer for the LFE source formatter.
%%%% Unlike lfe_scan, this keeps every comment and every whitespace character so
%%%% the formatter can preserve them.
```

Its token vocabulary tells the same story. Where a compiler's scanner has no
concept of a comment, ours makes them first-class:

```erlang
-type kind() :: lparen | rparen | lbracket | rbracket
              | tuple_open | map_open | binary_open | eval_open
              | quote | quasiquote | unquote | unquote_splicing | fun_ref
              | symbol | qsymbol | number | char
              | string | bstring | tqstring | tqbstring
              | line_comment | block_comment
              | whitespace | newline
              | dot.
```

`line_comment`, `block_comment`, `whitespace`, `newline` — every one of those is
trivia a compiler discards and a formatter must keep. And keeping the tokens
isn't enough; you need somewhere to *put* them in the tree. A bare AST has no
slot for "the comment that floated above this function" or "the inline note
trailing this clause." So on top of the lexer we built a CST — a *concrete*
syntax tree, `r3lfe_format_cst.erl` — whose job is precisely to hold the trivia
the abstract tree throws away, attaching each comment to the form it belongs near
so the printer can set it back down in the right place.

That lexer-plus-CST layer was about eight hundred lines before we wrote a single
rule about how anything should actually *look*. It is the tax a formatter pays
that a compiler never sees — and it's the most BEAM-specific, least glamorous, and
most necessary part of the whole exercise.

## The shape of the thing

With the lossless front end in place, the rest of the formatter was a rebar3
provider and an engine. The provider, `r3lfe_prv_format.erl`, is the part you
actually run. It wires `rebar3 lfe format` into the build tool, discovers your
source files, and offers the flags that make it useful in practice:

```erlang
Opts = [
    {dry_run, $n, "dry-run", boolean,
     "Do not write; print the formatted result to stdout"},
    {check, $c, "check", boolean,
     "Do not write; exit non-zero if any file is not already formatted"},
    {path, $p, "path", string,
     "Format only this file or directory, ignoring configured source dirs"}
].
```

That `--check` flag is a nice bit from the early work: it's one of the things that makes the formatter a *discipline* rather than a
convenience. It writes nothing; it just exits non-zero if any file isn't already
formatted. Drop `rebar3 lfe format --check` into CI and "is this code in house
style?" stops being a thing reviewers argue about and starts being a thing the
build either passes or fails. Honesty enforced by a machine is a different
quality of honesty than honesty enforced by good intentions.

Behind the provider sat the engine, `r3lfe_formatter.erl`, and a test suite that
left no doubt that this was not a toy: the formatter suite alone
ran past three thousand lines of Common Test, with separate suites exercising the
lexer and the CST. People were running this on real code. It worked.

## The wall

The formatter worked. It also became, in my own notes at the time, *supremely
awkward and really hard to maintain* — and the reason why is the seed of this
entire series.

Look at how the engine knew which forms to break. There were really two halves to
that knowledge. One half was a table — and a lovely one, lifted almost verbatim
from `lfe-indent.el`, mapping each special form's name to the number of
"distinguished" leading arguments that stay on the head line:

```erlang
specform_table() ->
    #{
        ":"                 => 2,
        "case"              => 1,
        "if"                => 1,
        "lambda"            => 1,
        "let"               => 1,
        "let*"              => 1,
        "receive"           => 0,
        "try"               => 0,
        "progn"             => 0,
        %% … and a few dozen more
    }.
```

That's *data.* It's a clean, declarative description of LFE's indentation
convention — the same shape Emacs has carried for decades. If that were the whole
story, the formatter would have been easy to maintain: to teach it a new form,
you'd add a line to a table.

But that was only half the knowledge. The *other* half — the rules about which
forms must always break onto multiple lines even when they'd fit, which stay flat,
how clauses indent, where guards go — lived not as data but as *code*, scattered
through the engine as conditionals. Here's the predicate that decides whether a
form is in the always-break set:

```erlang
is_always_break_head(Node) ->
    case r3lfe_format_cst:children(Node) of
        [Head | _] ->
            case r3lfe_format_cst:type(Head) of
                symbol ->
                    Text = r3lfe_format_lexer:text(r3lfe_format_cst:open(Head)),
                    Text =:= "let"     orelse Text =:= "let*"
                    orelse Text =:= "case"    orelse Text =:= "cond"
                    orelse Text =:= "if"      orelse Text =:= "progn"
                    orelse Text =:= "receive" orelse Text =:= "try"
                    orelse Text =:= "maybe"   orelse Text =:= "match-lambda";
                _ -> false
            end;
        [] -> false
    end.
```

There's nothing *wrong* with that function. But notice what it is: LFE's
formatting convention, written as an `orelse` cascade. Every new form is a new
clause. Every refinement — the way `try` lays out its sections, the way `export`
sorts and indents its entries, the special handling for guards — meant more
Erlang, fused into the layout logic, until the engine had grown to roughly
**1,900 lines** where the *rules* and the *machinery that applies them* were
welded together. The convention that *is* LFE's formatting style was trapped
inside an Erlang program, recoverable only by reading the program.

And here's the asymmetry that finally bothered me enough to do something about it.
`lfe-indent.el` has carried LFE's indentation convention for *decades* — and it
carries it as **data**, a small table of `(form N)` entries that a human can read
at a glance and a tool can consume. We had quietly re-derived the same knowledge
and written it down as **code**: a thousand-plus lines of conditionals that only
the engine could interpret and only by running. Emacs had understood, in the
1990s, something we'd talked ourselves out of. The convention is data. We'd built
a machine that insisted on treating it as logic.

That's the wall. Not "the formatter is buggy" — it wasn't, particularly. The wall
is that *the approach had a ceiling*. Every form you teach a hand-written
formatter costs another clause in another conditional, and the thing the formatter
exists to encode — a language's layout convention — ends up imprisoned in the one
representation that's hardest to inspect, hardest to share, and hardest to reuse
for any *other* language.

## What's next

So the old formatter did its job and then asked a good question: where does the formatter engine end and the rules begin? A formatter
that owns layout has to *decide* layout — when to break, where to align, how to
spend a line — and we were making every one of those decisions greedily, by hand,
in Erlang. This does not *scale*, and it certainly doesn't generalise
past the one language whose rules you happened to hard-code.

Which raised the obvious thought: surely someone has built a *principled* engine
for this — something that, given a document and a width, makes the layout
decisions *optimally* rather than greedily, with the language-specific knowledge
kept cleanly to one side as the data it always wanted to be. I went looking. It
turns out someone had built exactly that, and not for a Lisp on the BEAM but for a
Lisp on a very different runtime — and the paper underneath it sent me down a far
deeper rabbit hole than I expected.

That's where part 2 begins.
