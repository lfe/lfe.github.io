---
layout: post.liquid
title: "Graph Theory in LFE: A Knowledge Graph of Erlang Itself"
description: "A deep, hands-on tour of graph theory in LFE — built on a real corpus of 1,664 Erlang concept cards, driven by the graffeo library, and unfolded slowly enough to teach the mathematics underneath each query."
permalink: "/blog/tutorials/2026/06/05/1008-graph-theory-in-lfe-graffeo-erlang-knowledge"
categories: ["tutorials"]
tags: [graffeo, graphs, graph-theory, erlang, otp, knowledge-graph, dijkstra, mathematics, maths, libraries]
published_date: 2026-06-05 10:08:00 +0500
is_draft: false
data:
  author: duncan-mcgreggor
  written_for: null
  last_validated: null
  cover_image: "/images/LFE_Graffeo_00409_.png"
  cover_alt: null
  math: true
---

There is a small, stubborn idea behind this post: that working Erlangers and LFEers have most of the intuition they need to *do* graph theory, but almost none of the language for it — because every introduction to the subject seems to assume either no programming background, or a Python/Java/C++ one. Not nearly as much fun. Also,  while a working BEAM programmer already understands graph theoretic pragmatics via supervision trees, message-passing topologies, distribution maps, and dependency graphs in `*.app` files, none of that intuition is being claimed by textbooks.

This tutorial is an attempt to claim it. We are going to build a real graph — not a toy with seven vertices and five edges, but a directed multilabelled graph over **1,664 Erlang "concept cards"** distilled from a dozen canonical Erlang/OTP sources — and then ask interesting graph-theoretic questions of it, in LFE, with [graffeo](https://github.com/erlsci/graffeo) as our library. Every algorithm used in this post was written and tested against an independent implementation (a small Rust oracle that lives alongside the example which uses the excellent `petgraph` library), so the numbers are real and can be reproduced exactly.

The underlying example is the Erlang `examples/erlang-concepts` directory in the [graffeo repository](https://github.com/erlsci/graffeo). The original README is itself a guided tour, in Erlang. What you are reading is the LFE rewrite of that tour — but with the mathematics opened up. Where the Erlang README says *"this is what condensation does,"* the LFE version is going to say *what condensation is, why it works, what theorem you are leaning on, and how the algorithm sees the graph from the inside.*

## What you'll need

- Erlang/OTP 27+ and `rebar3`.
- LFE 2.2 or later (via the `rebar3_lfe` plugin).
- About 13 MB of disk space for the corpus, fetched as a shallow git clone.

You will not need any prior graph theory. You will not need to read the Erlang version of the example. We'll meet every term as it comes up.

## 1. Clone, set up the LFE toolchain

Start with a clone of `graffeo` and step into the example directory:

```shell
git clone https://github.com/erlsci/graffeo.git
cd graffeo/examples/erlang-concepts
make fetch-cards
```

`make fetch-cards` does one job: it pins and shallow-clones `billosys/ai-engineering@0.1.0` into `../../workbench/ai-engineering`. The cards live under `knowledge/erlang/concept-cards/` once the clone is done. We won't vendor them — they're 13 MB of Markdown with typed front matter, and they want to be a separately versioned thing.

Now teach `rebar3` to compile LFE alongside Erlang. Open `rebar.config` and add the `rebar3_lfe` plugin and the LFE dependency:

```erlang
{erl_opts, [debug_info]}.

{deps, [
    {lfe, "2.2.0"}
]}.

{project_plugins, [
    erlfmt,
    rebar3_lint,
    rebar3_lfe
]}.

{provider_hooks, [
    {pre, [{compile, {lfe, compile}}]}
]}.

{project_app_dirs, ["apps/*", "lib/*", ".", "../.."]}.
```

The `provider_hooks` line is the one that matters: it tells `rebar3` to run the LFE compiler before the Erlang one, so any LFE module that exports functions can be called from Erlang in the same project (and vice versa). The Erlang modules already in `src/` won't go anywhere; we will sit a parallel set of LFE modules next to them, and prove they can live together.

Make a clean spot for the new code:

```shell
touch src/lfeerlcpt-parser.lfe \
      src/lfeerlcpt-ingest.lfe \
      src/lfeerlcpt-queries.lfe \
      src/lfeerlcpt.lfe
```

`lfeerlcpt` is short for "**LFE** **Erl**ang **C**once**pt**s." If you'd rather name things `egc-parser`, `gc-parser`, or anything else, go ahead — the only thing that matters is that the prefix is the same across all four modules and doesn't collide with `erlc_*`.

A quick health check before we write a single line of LFE:

```shell
rebar3 compile
rebar3 lfe repl
```

If you get an `lfe>` prompt and `(graffeo:new)` returns a `#(...)` tuple, you're set. The rest of this post is the substance.

## 2. The shape of the graph

Before we write code that builds the graph, we have to be very precise about what the graph *is.* This is the part of graph work that is almost always glossed in tutorials, and it's the part that has the highest leverage downstream — a vague mental model leads to a vague API leads to vague queries.

A directed graph $G = (V, E)$ is a set of vertices $V$ together with a set of edges $E \subseteq V \times V$. That is the entire formal content of the term *directed graph*. Everything else — labels, weights, vertex attributes, edge attributes — is conventional decoration. Working programmers tend to over-import the decoration into the definition; the math is much barer than that. The bareness is *useful*: it lets us say things like "every directed graph has a unique condensation" without first having to specify what kind of decoration is on it.

Our graph here will not be bare. It will carry two distinct kinds of vertex and four distinct kinds of edge label, so we need a small notation for the decoration.

A **vertex** is one of two shapes:

- a **source-layer vertex**, written `(tuple SourceSlug Slug)` — for example, `#(#"programming-erlang" #"gen-server")`. This is "the `gen-server` card in *Programming Erlang*." A binary 2-tuple.
- an **abstract-layer vertex**, written as a bare `Slug` — for example, `#"gen-server"`. This is "the concept `gen-server`," without reference to any particular book.

Why two layers? Because the question *"what is gen-server related to?"* has two honest answers, and a single-layer graph would force you to pick one. *Programming Erlang's* take on `gen-server` is not literally the same as *Learn You Some Erlang's* take, and pretending otherwise loses information; but you also frequently want the *union* across books, and forcing every query to do that union by hand is exhausting. The two-layer model gives you both — at the cost of an extra kind of edge, which we'll call **membership**.

A **membership edge** runs from a source-layer vertex up to its abstract concept:

$$\{(s, c) \to c \;:\; \text{card } (s, c) \text{ exists}\}$$

We label it `instance_of`. There is exactly one membership edge per card; if six books all document `gen-server`, there are six membership edges pointing into the abstract `#"gen-server"` vertex.

Then there are four kinds of **relationship edge**, drawn from the corpus's typed front matter:

- `prerequisites` — *X needs Y first* (directed).
- `extends` — *X is a special case of Y* (directed).
- `related` — *see also* (symmetric in spirit; the corpus stores it directed but you can read it either way).
- `contrasts_with` — *don't confuse X with Y* (symmetric).

Crucially, both layers carry these four kinds. Within a single source layer, an edge `(s, x) → (s, y)` means "book $s$ says $x$ relates to $y$ in this way." On the abstract layer, an edge $x \to y$ is the **upward projection**: it exists if *any* book asserted that relationship between cards for $x$ and $y$. Edge metadata records which types were asserted (as a set) and which books asserted them (also a set), so the abstract layer is a *labelled multilabel projection* of the source layer.

This is the whole modelling decision, and once it's clear, the API gives itself: a graph operation that wants to ask a source-local question pattern-matches on tuple vertices; a graph operation that wants the cross-book view runs against binary vertices.

## 3. Writing the parser in LFE

The parser does the work of turning each card's YAML-ish Markdown front matter into a structured record. The cards themselves come in two slightly different dialect variants — some have `# === SECTION ===` comment headers interleaved with key-value lines, some are flat — so the parser has to be tolerant about both. Tolerant parsers are mostly about pattern-matching cleanly on lines, which is exactly the sort of thing LFE is good at.

A card is a map with atom keys:

```lfe
;; A card looks like:
;;   #M(slug           #"gen-server"
;;      concept        #"Generic Server"
;;      category       #"behaviour"
;;      tier           #"core"
;;      source         #"Programming Erlang"
;;      source_slug    #"programming-erlang"
;;      prerequisites  (#"behaviour" #"callback-module")
;;      extends        ()
;;      related        (#"otp-behaviours")
;;      contrasts_with ())
```

Here is the entry point:

```lfe
(defmodule lfeerlcpt-parser
  (export (parse-file 1)
          (parse-string 1)))

(defun parse-file (path)
  (case (file:read_file path)
    ((tuple 'ok bin) (parse-string bin))
    ((tuple 'error reason)
     (tuple 'error (tuple 'read-failed path reason)))))

(defun parse-string (bin)
  (let ((lines (binary:split bin #"\n" '(global))))
    (case (extract-frontmatter lines)
      ((tuple 'ok fm-lines)
       (let ((props (parse-fm-lines fm-lines)))
         (build-card props)))
      ((= (tuple 'error _) err)
       err))))
```

A short tour of the LFE idioms in just those ten lines is worth doing, because they're going to repeat through the rest of the code:

- `case` on tuples is the bread-and-butter LFE pattern for matching on Erlang return-shapes. `(tuple 'ok bin)` matches `{ok, Bin}`. The quote in `'ok` is important — without it, LFE would try to evaluate `ok` as a variable.
- `let` introduces local bindings; the form is `(let ((var expr) ...) body)`. We use it instead of nested `case` when the binding is unconditional.
- `binary:split` is just the Erlang stdlib function; LFE calls into Erlang modules with the `mod:fun` syntax and atoms naturally. The trailing `'(global)` is the options list.
- The `(= pattern name)` form is LFE's *alias pattern* — it matches `pattern` and binds the whole matched value to `name` as well. The Erlang equivalent is `Name = Pattern`; LFE puts the name on the right.

Now `extract-frontmatter`. Cards begin with `---` on a line by itself, then the front matter, then a closing `---`. Tolerant version: also accept lines that *start with* `---` but have trailing characters. We pattern-match on the leading binary bytes:

```lfe
(defun extract-frontmatter
  (((cons (binary "---" (_tail binary)) rest))
   (collect-until-closing rest '()))
  ((_)
   (tuple 'error 'no-opening-delimiter)))

(defun collect-until-closing
  ((() acc)
   (tuple 'error (tuple 'no-closing-delimiter (length acc))))
  (((cons (binary "---" (_tail binary)) _) acc)
   (tuple 'ok (lists:reverse acc)))
  (((cons line rest) acc)
   (collect-until-closing rest (cons line acc))))
```

Two LFE features carry their full weight here:

- **Multi-clause `defun` with patterns.** Each parenthesised group on the right is `(pattern body)`. The matcher picks the first clause whose pattern fits. This is the same as Erlang's `function/N` clauses, just written one level outward.
- **Binary patterns inside lists.** `(cons (binary "---" (_tail binary)) rest)` matches a list whose head is a binary starting with the literal bytes `---` followed by arbitrary binary content, and whose tail is `rest`. This is LFE's binary-pattern syntax doing exactly what `<<"---", _/binary>>` does in Erlang. The `_tail` prefix is a wildcard — LFE doesn't bind a name starting with `_`, so we can keep the form symmetric without worrying about an unused-variable warning.

The line classifier is where the dialect tolerance lives:

```lfe
(defun classify-line
  ((#"")             'empty)
  (((binary "# ===" (_rest binary))) 'section-header)
  (((binary "- " (item-raw binary)))
   (tuple 'list-item (unquote-bin (string:trim item-raw))))
  ((line)
   (case (binary:split line #": ")
     ((list key value)
      (parse-kv (string:trim key) (string:trim value)))
     ((list maybe-kv)
      (case (binary:split maybe-kv #":")
        ((list key #"") (tuple 'kv key '()))
        (_ 'empty))))))
```

That last clause — falling through to a single colon with empty value, or giving up — is *exactly* the kind of place where tolerant parsers earn their keep. The card corpus has a handful of lines of the form `prerequisites:` with no value following, meaning the empty list; we want those to land as `(kv key ())` rather than as parse errors.

I won't reproduce the rest of the parser inline; the `build-card` and small helpers (`unquote-bin`, `get-bin`, `get-list`) are direct translations of the Erlang originals and don't introduce any LFE idiom we haven't already met. The full file is in the example repo.

The shape worth carrying forward into the next module is that a parsed card is a map, its values are binaries (for the scalar fields) or lists of binaries (for the relationships), and its keys are atoms.

## 4. Building the two-layer graph

The ingest module is the one that turns a list of cards into a `graffeo` graph. This is the module where most of the graph-construction idioms appear, so it's worth working through.

The top-level entry point sets the architecture in plain sight:

```lfe
(defmodule lfeerlcpt-ingest
  (export (build 1)
          (build-from-dir 1)
          (source-vertices 1)
          (abstract-vertices 1)))

(defun build (cards)
  (let* ((sorted (lists:sort
                  (lambda (a b)
                    (=< (mref a 'slug) (mref b 'slug)))
                  cards))
         (g0 (graffeo:new))
         (g1 (add-source-layer sorted g0))
         (g2 (add-abstract-layer sorted g1))
         (g3 (add-membership sorted g2))
         (g4 (add-source-edges sorted g3))
         (g5 (add-abstract-edges sorted g4)))
    (map 'graph g5
         'source_count (length sorted)
         'abstract_count (length
                          (lists:usort
                           (lc ((<- c sorted)) (mref c 'slug)))))))
```

There is no cleverness in `build`; the value comes from saying *out loud* that the graph is constructed in five strictly ordered phases. `let*` lets each phase name its input by hand, which is more verbose than threading would be, but the trade is worth it: a beginning reader can stop at any line and know what's in the graph so far.

The sort is for **determinism**. graffeo's value-tier backend is a map-of-maps, so insertion order can influence the order in which vertices are returned from `vertices/1`. If we want builds to be reproducible — and we do, because the example is gated by an oracle that compares results byte-for-byte against a Rust re-implementation — every nondeterministic dial gets locked down. Slug-sorting cards is the cheapest such dial.

The source layer is a simple fold:

```lfe
(defun add-source-layer (cards g)
  (lists:foldl
   (lambda (card acc)
     (graffeo:add_vertex acc (source-vertex card)))
   g cards))

(defun source-vertex (card)
  (tuple (mref card 'source_slug) (mref card 'slug)))
```

`lists:foldl` over a list of cards, threading the graph through, accumulating vertices. Two things to notice:

- We can pass `(lambda (card acc) ...)` straight to `lists:foldl` and the call works because `add_vertex/2` is *value-tier-pure* — it returns a new graph rather than mutating an opaque handle. This matters for reasoning, and it's why graffeo's default backend is the value tier in the first place. (graffeo also has a `digraph`-backed handle tier; the same algorithm code runs over both, which is graffeo's headline commitment: "one algorithm layer over many backends.")
- We don't worry about duplicate vertices. `graffeo:add_vertex` on an existing vertex is a no-op. Most idiomatic graph code on the BEAM relies on this: it lets you describe what *should be there* rather than carefully bookkeeping what *is* there.

The abstract layer is the same shape, but folds over the deduplicated slug set:

```lfe
(defun add-abstract-layer (cards g)
  (let ((slugs (lists:usort
                (lc ((<- c cards)) (mref c 'slug)))))
    (lists:foldl
     (lambda (slug acc) (graffeo:add_vertex acc slug))
     g slugs)))
```

The 1,664 cards collapse to roughly 1,394 distinct slugs because some concepts (`gen-server`, `pattern-matching`) are documented in many books at once.

Membership edges glue the layers together:

```lfe
(defun add-membership (cards g)
  (lists:foldl
   (lambda (card acc)
     (let ((from (source-vertex card))
           (to (mref card 'slug)))
       (graffeo:add_edge acc from to (map 'label 'instance_of))))
   g cards))
```

`graffeo:add_edge/4` takes the graph, a "from" vertex, a "to" vertex, and an edge metadata map. The `'instance_of` label is our domain vocabulary; graffeo doesn't interpret it, but our queries will key on it later.

The source-edge phase is the first non-trivial bit. For each source $s$, for each card $C$ in $s$, for each relationship type $T$, for each target slug $t$ listed under $T$ in $C$, we want to add an edge `(s, C.slug) → (s, t)` *if and only if* $t$ also has a card in $s$ — i.e., we only honour intra-source edges in the source layer. (The cross-source edges live on the abstract layer; we'll get there.)

That's four nested loops, which sounds terrible, but the data is small enough that the time is dominated by the parsing, not the edge build. Three nested folds:

```lfe
(defun add-source-edges (cards g)
  (let ((by-source (group-by-source cards)))
    (maps:fold
     (lambda (src src-cards g-acc)
       (let ((all-slugs (sets:from_list
                         (lc ((<- c src-cards)) (mref c 'slug))
                         '(#(version 2)))))
         (lists:foldl
          (lambda (card g-acc2)
            (add-card-source-edges src card all-slugs g-acc2))
          g-acc src-cards)))
     g by-source)))

(defun add-card-source-edges (src card all-slugs g)
  (let* ((from-slug (mref card 'slug))
         (from (tuple src from-slug))
         (rel-types (list (tuple 'prerequisites    (mref card 'prerequisites))
                          (tuple 'extends          (mref card 'extends))
                          (tuple 'related          (mref card 'related))
                          (tuple 'contrasts_with   (mref card 'contrasts_with)))))
    (lists:foldl
     (lambda (type-and-targets g-acc)
       (let (((tuple type targets) type-and-targets))
         (lists:foldl
          (lambda (target-slug g-acc2)
            (case (sets:is_element target-slug all-slugs)
              ('true
               (add-typed-edge g-acc2 from (tuple src target-slug)
                               type src))
              ('false g-acc2)))
          g-acc (lists:sort targets))))
     g rel-types)))
```

This is a faithful port of the Erlang code, which means it is unapologetically nested. There are tidier formulations in LFE — you can flatten the rel-types into a single list comprehension over `(type . target)` pairs, for example — but the nested form has one virtue: each level of nesting corresponds to one level of the data hierarchy (source / card / relationship type / target slug), and a reader who understands the data can step through the loops without a sticky note.

`add-typed-edge` is where the edge metadata's *set semantics* live:

```lfe
(defun add-typed-edge (g from to type src)
  (case (graffeo:edge_meta g from to)
    ((tuple 'ok (map 'label (map 'types types 'asserted_by asserted-by)))
     (let ((new-types (lists:usort (cons type types)))
           (new-asserted (lists:usort (cons src asserted-by))))
       (graffeo:add_edge g from to
                         (map 'label (map 'types new-types
                                          'asserted_by new-asserted)))))
    ('error
     (graffeo:add_edge g from to
                       (map 'label (map 'types (list type)
                                        'asserted_by (list src)))))))
```

Two different books asserting the same edge with different relationship types should not produce two edges — graffeo is a *simple* directed graph (at most one edge per ordered pair). What you do instead is merge the metadata. Here, types and asserters are both sets, stored as sorted unique lists; merging is just `lists:usort` over the cons.

This is where the model's two-layer nature pays off in a subtle way. On the *source* layer, an edge between `(s, x)` and `(s, y)` will only ever have `[s]` as its asserter list, because the source-edge phase confines itself to intra-source links. On the *abstract* layer, an edge $x \to y$ accumulates asserters from every book that drew it. So when you later ask "*how many books say `gen-server` extends `behaviour`?*", the answer is `(length asserted_by)` of the abstract edge — and that number is itself a kind of confidence.

The abstract-edge phase has two pieces. First, **project** the source-layer edges upward — every $(s, x) \to (s, y)$ becomes an $x \to y$ candidate, and we merge metadata across all the candidates:

```lfe
(defun project-source-edges (g)
  (let ((src-only (graffeo:filter_edges
                   g
                   (lambda (from to _meta)
                     (andalso (is_tuple from) (is_tuple to))))))
    (let ((contracted (graffeo:contract
                       src-only
                       (lambda (v)
                         (let (((tuple _src slug) v)) slug))
                       (lambda (a b) (merge-type-meta a b)))))
      (merge-contracted-into g contracted))))
```

There are three graffeo operations doing real work here, and each is worth a moment.

`filter_edges/2` returns a subgraph (with the same vertices) keeping only edges that satisfy the predicate. Here we keep the edges whose both endpoints are tuples — i.e., the *source-only* edges, excluding membership and any pre-existing abstract edges. (At this stage there shouldn't be any pre-existing abstract edges, but the predicate is robust to having them.)

`contract/3` is the operation that does the actual upward projection: it takes a vertex-mapping function and a metadata-merge function, and quotients the graph by the equivalence relation induced by the mapping. Concretely, every vertex `(tuple _src slug)` is sent to `slug`, so all the source-layer copies of `gen-server` are collapsed into a single vertex `#"gen-server"`. Edges between collapsed vertices are unioned, and the metadata-merge function decides how to combine their metadata. This is *exactly* the [quotient construction](https://en.wikipedia.org/wiki/Quotient_graph) from elementary graph theory, written one line.

`merge-contracted-into/2` then merges the contracted graph back into the main graph, accumulating metadata when an abstract edge already exists.

The second piece adds **cross-only** edges — relationships a card asserts to a target that isn't in its own book, but which exists somewhere in the corpus. Those edges have no source-layer representative to project from, so we add them directly to the abstract layer. The code shape is parallel to `add-source-edges` and isn't worth reproducing line for line; what matters is that a target slug that exists in *no* book at all becomes what we'll call a **ghost** — we add it as an abstract vertex with no membership edges pointing in. We meet the ghosts again in the queries section.

## 5. The query catalog

This is the module where the graph theory will actually live. Every query in this section pairs a one-paragraph "what it computes" with several paragraphs of "why that's interesting and what the algorithm is doing under the hood." If you've read graph-theory tutorials before that started with definitions and ended with code, this one starts with code and ends with definitions; the order is deliberate.

The module preamble and the four "projection" helpers (which we lean on heavily) come first:

```lfe
(defmodule lfeerlcpt-queries
  (export (prerequisite-cycles 1)
          (learning-order 1)
          (related-components 1)
          (semantic-components 1)
          (top-concepts-by-degree 2)
          (ghost-concepts 1)
          (related-cheap 3)
          (related-extended 3)
          (gentlest-path 3)
          (coverage-of 2)
          (project-by-type 2)))

(defun project-by-type (g type)
  (graffeo:filter_edges
   g
   (lambda (from to meta)
     (andalso (is_binary from)
              (is_binary to)
              (has-type? meta type)))))

(defun project-abstract-relations (g)
  (graffeo:filter_edges
   g
   (lambda (from to _meta)
     (andalso (is_binary from) (is_binary to)))))

(defun has-type? (meta type)
  (case meta
    ((map 'label (map 'types types))
     (lists:member type types))
    (_ 'false)))
```

`project-by-type/2` returns the subgraph of the abstract layer carrying only edges of a single relationship type. `project-abstract-relations/1` returns the abstract layer carrying *all* relationship edges. The queries pick one or the other depending on what question they're asking.

### 5.1 What did we just load?

The simplest possible query: ask graffeo how big the graph is.

```
lfe> (set r (lfeerlcpt-ingest:build-from-dir
             "../../workbench/ai-engineering/knowledge/erlang/concept-cards"))
lfe> (set g (mref r 'graph))
lfe> (graffeo:no_vertices g)
3061
lfe> (graffeo:no_edges g)
14341
```

3,061 vertices breaks down as 1,664 source vertices + 1,397 abstract vertices, the latter being 1,394 *carded* abstract concepts plus 3 *ghost* concepts (referenced as prerequisites but never written up). We meet the ghosts in §5.7.

Note: if you are getting zero cards and if this returns an empty list:

```lfe
lfe> (filelib:wildcard "../../workbench/ai-engineering/knowledge/erlang/concept-cards/*")
()
```

The you might have forgotten to perform the following in the `erlang-concepts` directory:

```shell
make fetch-cards
```

The abstract-edge breakdown is worth pulling out, because it tells you something about the *texture* of how the corpus is annotated:

| type | count | reading |
|------|------:|---------|
| `related` | 3917 | "see also" |
| `prerequisites` | 2158 | "you need X first" |
| `contrasts_with` | 591 | "don't confuse X with Y" |
| `extends` | 299 | "X is a special case of Y" |

So `related` is half of all assertions, `prerequisites` is a quarter, and the structural types together (`contrasts_with` + `extends`) are 14%. The corpus is associatively rich and structurally sparse — exactly what you'd expect from material distilled from twelve different books each of which made its own decisions about what to formally lay out.

The graph $G$ on disk is now a single graffeo value. Every query below operates on it.

### 5.2 Degree: who is the centre of OTP?

The simplest **centrality** measure in graph theory is also the one a working programmer already knows by another name: degree. In a directed graph, every vertex $v$ has an *out-degree* $d^+(v)$ (the number of edges leaving $v$) and an *in-degree* $d^-(v)$ (the number entering); the *total degree* is $d(v) = d^+(v) + d^-(v)$. graffeo's `top_k_by_degree/2` ranks vertices by total degree:

```lfe
(defun top-concepts-by-degree (g k)
  (let ((ag (project-abstract-relations g)))
    (graffeo:top_k_by_degree ag k)))
```

Run it on the abstract layer:

```
lfe> (lfeerlcpt-queries:top-concepts-by-degree g 5)
(#(#"gen-server" 118)
 #(#"pattern-matching" 92)
 #(#"otp-application" 86)
 #(#"message-passing" 84)
 #(#"process" 71))
 ```

Anyone who has shipped Erlang reading that list nods: `gen_server`, pattern matching, message passing, applications, supervisors. The graph has reconstructed the OTP **syllabus** from nothing but cross-references, with no human ranking anything by hand.

Why does this crude measure already work? The answer is structural: in a knowledge graph distilled from many independent authors, ideas that *many other ideas* depend on are inevitably ones the authors think a learner has to internalise. Authors echo each other on the load-bearing pieces. So in a corpus that is honest about what relates to what, you get for free a *democratic vote* on what matters — counted by edge mass.

There is a long bench of richer centralities — **betweenness** (how often a vertex sits on shortest paths between other pairs), **closeness** (the reciprocal of the average shortest-path distance to all other vertices), **eigenvector** centrality (a vertex is important if it's connected to other important vertices), **PageRank** (eigenvector with a damping term). Each fixes a specific weakness of degree: degree doesn't notice that a vertex with two well-placed neighbours can be more central than one with twenty peripheral ones; degree can't tell a *hub* from a *bridge*; degree is fooled by trivially-popular things. graffeo will grow some of these (PageRank is on the roadmap), but it's worth being honest about the trade: each of them costs $O(V \cdot E)$ or worse, and on a corpus like this the cheapest measure is already so accurate that the marginal information from running PageRank is small. Reach for the heavier measures when degree disagrees with your intuition, not before.

### 5.3 Neighbourhoods, and the radius dial

Degree counts; the neighbours themselves are more interesting. graffeo's `out_neighbours/2` and `in_neighbours/2` give you the immediate forward and backward neighbours:

```
lfe> (graffeo:out_neighbours g #"gen-server")
(#"behaviour" #"callback-module" #"client-server-model"
 #"generic-server" #"message-passing" #"otp-behaviours"
 #"process-state-loop" ...)
```

These are the abstract-layer concepts `gen-server` *points at* — the concepts it depends on, extends, contrasts with, or is associatively related to (the four edge types all union here). To get the typed neighbourhood for a single relationship, project first:

```lfe
(defun out-neighbours-by-type (g v type)
  (graffeo:out_neighbours (project-by-type g type) v))
```

The open neighbourhood $N^+(v) = \{u : (v, u) \in E\}$ and its closed counterpart $N^+[v] = N^+(v) \cup \{v\}$ are the most natural local measure of "what's near a vertex." But on a two-layer graph, "local" has *two* honest scales, and this is where the model really starts to earn its keep.

The cheap (source-local) view stays inside one book:

```lfe
(defun related-cheap (g source-slug concept-slug)
  (let* ((src-v (tuple source-slug concept-slug))
         (nbrs (graffeo:out_neighbours g src-v))
         (src-nbrs (lc ((<- n nbrs)
                       (andalso (is_tuple n)
                                (=:= (element 1 n) source-slug)))
                      n)))
    (lists:usort
     (lc ((<- pair src-nbrs)
          (let (((tuple _ target-slug) pair))
            (case (graffeo:edge_meta g src-v (tuple source-slug target-slug))
              ((tuple 'ok (map 'label (map 'types types)))
               (lists:member 'related types))
              (_ 'false))))
         (element 2 pair)))))
```

The expensive (cross-book) view walks up through membership, sideways to other books' cards for the same concept, and gathers each book's local `related` neighbours:

```lfe
(defun related-extended (g source-slug concept-slug)
  (let* ((local (related-cheap g source-slug concept-slug))
         (in-nbrs (graffeo:in_neighbours g concept-slug))
         (sibling-cards (lc ((<- v in-nbrs)
                             (andalso (is_tuple v)
                                      (=/= (element 1 v) source-slug)
                                      (=:= (element 2 v) concept-slug)))
                            v))
         (sibling-related (lists:flatmap
                           (lambda (v)
                             (let (((tuple sib-src _) v))
                               (related-cheap g sib-src concept-slug)))
                           sibling-cards)))
    (lists:usort (lists:append local sibling-related))))
```

The smaller a concept is, the more dramatic the difference. `anonymous-variable` appears in two books; *Erlang/OTP in Action* links it only to "variable":

```
lfe> (lfeerlcpt-queries:related-cheap g
                                 #"erlang-otp-action"
                                 #"anonymous-variable")
(#"variable")
lfe> (lfeerlcpt-queries:related-extended g
                                    #"erlang-otp-action"
                                    #"anonymous-variable")
(#"list" #"single-assignment-variable" #"tuple"
 #"underscore-prefixed-variables" #"variable")
```

The extended query traverses *forward* along membership to the concept, then *backward* down to sibling cards. That backward walk is graffeo's first-class reverse traversal — graffeo gives you `in_neighbours` directly, no auxiliary "reverse graph" to construct, no $O(E)$ index inversion at query time. This is the kind of operation that, on a less considered API, becomes painful: in raw `digraph`, you'd build a reverse index yourself, and you'd remember to invalidate it after edits. The fact that graffeo treats the reverse direction as a peer of the forward direction is what makes the extended query writable in the eight lines above rather than fifty.

The composite operation — *forward along type A, then backward along type A, then forward along type B* — is exactly a [relational join](https://en.wikipedia.org/wiki/Relational_algebra#Joins_and_join-like_operators) projected through a graph. graph theory and relational algebra are the same subject seen from different angles; if you've ever written a SQL self-join, you've already done this. The graph view tends to be more compact when there are more than two joins in flight.

### 5.4 Is the corpus one body, or an archipelago?

Here is the first question that depends on a non-trivial graph-theoretic *theorem* rather than just a definition. The question is: *which concepts can you reach from which others, ignoring direction?*

The technical name for the answer is **connected components**. A connected component of an *undirected* graph is a maximal set of vertices such that for any two vertices $u, v$ in the set there is a path between them; in the directed setting we read every edge as undirected and the same definition applies. The components of a graph form a partition of the vertex set; equivalently, *"is in the same component as"* is an equivalence relation. (Reflexivity: trivially. Symmetry: paths can be reversed. Transitivity: paths can be concatenated.)

graffeo computes the components in $O(V + E)$ via a single pass of breadth-first search (BFS):

```lfe
(defun related-components (g)
  (let* ((rg (project-by-type g 'related))
         (components (graffeo:components rg))
         (sorted (lists:sort (lambda (a b)
                               (>= (length a) (length b)))
                             components))
         (giant-size (case sorted
                       ((cons giant _) (length giant))
                       (() 0))))
    (tuple (length components) giant-size sorted)))
```

Ask it of the `related` projection:

```
lfe> (set (tuple count giant _) (lfeerlcpt-queries:related-components g))
lfe> (tuple count giant)
#(19 1151)
```

**Nineteen components, one of them a giant of 1,151 concepts.** That giant is the connected core of Erlang knowledge: the long-running argument that the language is *one* thing with one canon. The eighteen smaller islands are intellectual neighbourhoods that happen not to "see also" the core.

What are they? The largest satellite is 121 concepts of **coding-style** material — `avoid-case-catch`, `camelcase-variables`, `100-column-line-limit`, drawn mostly from the Inaka guidelines and the OTP programming rules. It floats free because style guides cite style guides; their authors didn't tend to write `related: gen-server` lines from inside style-guide cards. The next island is **tracing and diagnostics** — `recon-trace`, `match-specification`, `tracing-principles`, `dbg`. Then smaller pockets around distinct subdomains.

This is a *finding*, not a parameter: the graph has automatically sorted the corpus into its natural neighbourhoods. If you handed someone a stack of 1,400 cards and asked them to draw the natural clusters, they wouldn't be done by the end of the week. The graph does it in $O(V + E)$.

Now widen the relation. Run the same query on the *union* of all four edge types, not just `related`:

```lfe
(defun semantic-components (g)
  (let* ((ag (project-abstract-relations g))
         (components (graffeo:components ag))
         (sorted (lists:sort (lambda (a b)
                               (>= (length a) (length b)))
                             components))
         (giant-size (case sorted
                       ((cons giant _) (length giant))
                       (() 0))))
    (tuple (length components) giant-size sorted)))
```

```
lfe> (set (tuple c2 g2 _) (lfeerlcpt-queries:semantic-components g))
lfe> (tuple c2 g2)
#(10 1212)
```

Nineteen components have fallen to **ten**; the giant has grown from 1,151 to 1,212. The delta isn't decoration — it's measurement. Nine islands were bridged once you let `prerequisites`, `extends`, and `contrasts_with` count, not just `related`. That tells you something important about how the corpus is annotated: the structural relationships do work that the associative ones don't. Many concepts that nobody bothered to mark as "see also" are nonetheless *needed* by something in the core.

There's a beautiful corollary of how connected components are computed: the standard algorithm has the same structure as **union-find** (also called the *disjoint set forest*). Each vertex starts in its own component; each edge unions the components of its endpoints; the answer is the partition you end up with. With *path compression* and *union by rank*, the amortised cost per operation is effectively constant — formally, $O(\alpha(n))$ where $\alpha$ is the inverse Ackermann function, which is bounded by 4 for any input size you will ever actually meet. graffeo's `components/1` uses the BFS form rather than union-find, because BFS gives you each component as a *list* directly, which is what you want for downstream queries.

The fact that the giant component dominates is itself a structural signature. In Erdős–Rényi random graphs, there is a sharp threshold at average degree 1 below which there is no giant and above which there is exactly one, taking up an asymptotic fraction of all vertices. Real-world graphs almost never look exactly like Erdős–Rényi — they tend to be much more clustered, with a heavier-tailed degree distribution — but the *giant-component-plus-small-islands* pattern is so universal across human-curated graphs that it's almost a tautology. What's interesting here isn't *that* there's a giant, but *what the islands are*.

### 5.5 Reachability: the whole shadow of a concept

Components ignore direction. Prerequisites do not — "you need X first" is an arrow. Following those arrows transitively answers: *everything you must understand before you can claim to understand this concept.* That's **reachability**, and it's a different question from connectivity.

A vertex $v$ is **reachable** from a vertex $u$ in a directed graph if there is a directed path $u = w_0 \to w_1 \to \dots \to w_k = v$. The set of all vertices reachable from $u$ is the *forward transitive closure* of $\{u\}$; graffeo exposes it as `reachable/2`. Hand it the prerequisite projection and ask for the transitive closure of `gen-server`:

```
lfe> (set pg (lfeerlcpt-queries:project-by-type g 'prerequisites))
lfe> (length (graffeo:reachable pg (list #"gen-server")))
57
```

Now do the same for `supervisor` and `supervision-tree`:

```
lfe> (length (graffeo:reachable pg (list #"supervisor")))
57
lfe> (length (graffeo:reachable pg (list #"supervision-tree")))
57
```

Three different starting points, one identical prerequisite universe. That is *not* a coincidence and it is *not* a bug. To explain what is happening we need one more idea, but for now: the graph is telling you that these three concepts are mutually entangled tightly enough that whichever you start from, the prerequisite universe is the same. They are, in some technical sense, the same concept seen from three angles. To name that technical sense is the work of the next section.

Reachability's algorithm under the hood is BFS or DFS — pick one — and the cost is $O(V + E)$ per starting set. On graphs much larger than ours, computing the *full transitive closure* (every pair $(u, v)$ such that $v$ is reachable from $u$) is the $O(V^3)$ Floyd–Warshall problem, which is one of those algorithms whose constant factors are nice enough to be practical out to a few thousand vertices and useless beyond. graffeo doesn't compute full TC; it computes the *forward closure of a given starting set*, which is what you actually want 95% of the time and which costs $O(V + E)$ regardless of how many other vertices you don't care about.

A quick proof of the asymmetry between the "forward closure of one vertex" and "full TC": for a vertex $v$ with out-degree $0$, the forward closure of $\{v\}$ is $\{v\}$, computed in $O(1)$. Full TC has to discover this for every such vertex, of which there can be many. Conversely, if every vertex is reachable from every other vertex (a *strongly connected* graph), then the forward closure of any one vertex is already the entire vertex set — the cost is $O(V + E)$, but the answer is everything. Full TC in that case is $V^2$ pairs, all of them present. The work is bounded by the *answer size* in either case; for one-vertex closure the answer can be much smaller, and graffeo lets it be.

### 5.6 Cycles, SCCs, condensation, and the order that survives them

If "prerequisite" meant what it says, the prerequisite graph would be *acyclic*. There would be no path that starts at $X$, follows prerequisite arrows, and returns to $X$ — because that would say $X$ is a prerequisite of itself.

Test it:

```lfe
(defun prerequisite-cycles (g)
  (let* ((pg (project-by-type g 'prerequisites))
         (is-cyclic (not (graffeo:is_acyclic pg)))
         (cscs (graffeo:cyclic_strong_components pg))
         (cycles (lc ((<- c cscs) (>= (length c) 2))
                     (lists:sort c))))
    (tuple is-cyclic (lists:sort cycles))))
```

```
lfe> (set (tuple is-cyclic cycles) (lfeerlcpt-queries:prerequisite-cycles g))
lfe> (tuple is-cyclic (length cycles))
#(true 12)
```

Twelve **strongly connected components** of size greater than 1. Some of them are mutual pairs (`event-handler ⇄ gen-event-behavior`, `ct-test-case ⇄ ct-test-suite`). One is a ten-concept knot: `supervisor`, `gen-server`, `child-specification`, `behaviour`, `supervision-tree`, `worker-process`, and four others, all mutually entangled. *That* is why §5.5's three closures were identical — those three concepts live in this single SCC, so they share one forward closure by definition.

We need to define a few things precisely.

A **strongly connected component** of a directed graph $G$ is a maximal set of vertices $S \subseteq V$ such that for every pair $u, v \in S$ there is both a directed path from $u$ to $v$ *and* a directed path from $v$ to $u$. The SCCs partition $V$ (it's again an equivalence relation, by exactly the same three-step argument as before — reflexivity, symmetry now requiring directed paths in both directions, transitivity by concatenation).

A directed graph is **acyclic** if and only if every SCC is a singleton. Proof: if every SCC is a singleton, no two distinct vertices are mutually reachable, so no directed path can return to its start (it would have to involve some other vertex by which the start could be reached); conversely, if any SCC has $\geq 2$ vertices, pick any two distinct $u, v \in S$, take a path $u \to v$ and a path $v \to u$, concatenate, and you have a cycle through $u$.

The clever bit: even if $G$ has cycles, you can still extract a useful order from it by collapsing each SCC to a single super-vertex. The result is called the **condensation** of $G$, written $G^{\mathrm{SCC}}$ or $C(G)$. Its vertex set is the set of SCCs; its edges are *"there exists an edge from a vertex in $S$ to a vertex in $T$"*, for distinct SCCs $S$ and $T$.

The theorem that makes condensation worth caring about: **The condensation of any directed graph is a DAG (directed acyclic graph).** Proof: suppose for contradiction that $C(G)$ contains a cycle $S_1 \to S_2 \to \dots \to S_k \to S_1$ on distinct SCCs. Pick representatives $v_i \in S_i$. Then there are paths $v_i \rightsquigarrow v_{i+1}$ in $G$ for each $i$ (by definition of an edge in $C(G)$), and within each $S_i$ there are paths between any two vertices. Concatenate: there's a path from $v_1$ to itself through $v_2, \dots, v_k$. So $v_1$ is in the same SCC as $v_2$, which contradicts $S_1$ and $S_2$ being distinct SCCs. $\square$

That theorem is the load-bearing piece: any directed graph admits a canonical DAG built from its cycles, and any DAG admits a **topological order**. The order on the original graph that you get back is partial — within an SCC, no concept truly precedes another — but across SCCs it's total and consistent.

graffeo wraps this up as:

```lfe
(defun learning-order (g)
  (let* ((pg (project-by-type g 'prerequisites))
         (condensed (graffeo:condensation pg)))
    (graffeo:topsort condensed)))
```

```
lfe> (set (tuple 'ok order) (lfeerlcpt-queries:learning-order g))
lfe> (length order)
1204
```

The 34 entangled concepts (the union of the twelve cyclic SCCs) have collapsed into 12 super-vertices, leaving a 1,204-node DAG that sorts cleanly. 1,204 + 34 − 12 ≠ 1,397 because the prerequisite projection drops the concepts that are *involved in no prerequisite relationship at all*; the topsort is over the active subgraph only.

One precision point that always trips careful readers: in this corpus, the prerequisite edge runs *concept → prerequisite*. (A card for `gen-server` lists `behaviour` as a prerequisite, so the edge is `gen-server → behaviour`.) `graffeo:topsort/1` puts the tail of each edge before its head, so the returned `order` lists `gen-server` *before* `behaviour`. A learning sequence (foundations first) is that order reversed. The mathematics doesn't care which way you read a total order; you do.

A word on the algorithm, because it's one of the most beautiful in classical algorithmics. Two correct algorithms for SCCs exist. **Kosaraju's** is the easier one to explain: run a DFS over $G$, push each vertex onto a stack when its DFS subtree is finished, then run a second DFS over the *transpose* $G^T$ (every edge reversed) in stack order, and each DFS tree from the second pass is one SCC. The reason this works is the lovely interaction between the DFS finishing-order on $G$ and the reachability structure on $G^T$ — vertices with the latest finishing time are in *sink* SCCs of $G$, which are *source* SCCs of $G^T$. The second pass discovers them one at a time without cross-contamination.

**Tarjan's** algorithm does it in one pass, using a stack and a low-link number to detect when a DFS subtree is fully enclosed within an SCC. It is more compact, has tighter constants, and has the lovely property that SCCs come out in reverse topological order. graffeo uses Tarjan's. Both are $O(V + E)$, which is asymptotically optimal — you can't find SCCs without looking at every edge at least once.

Whether each cyclic SCC in our corpus is a real modelling defect (someone wrote the card poorly) or an honest statement that two ideas are *co-requisite* (you genuinely can't have one without the other) is a judgement call, and the graph doesn't make it for you. What the graph does is **localise** all twelve cycles for human adjudication, which is the hard part done.

### 5.7 Ghost concepts

A small but instructive query: which abstract concepts are *referenced* but have no card written for them?

```lfe
(defun ghost-concepts (g)
  (let ((abs-vs (lfeerlcpt-ingest:abstract-vertices g)))
    (lists:sort
     (lc ((<- slug abs-vs)
          (let ((in-nbrs (graffeo:in_neighbours g slug)))
            (not (lists:any (lambda (n) (is_tuple n)) in-nbrs))))
         slug))))
```

A "ghost" is an abstract vertex with no incoming *tuple-shaped* edges — that is, no membership edge from a source card. It exists in the graph because *some other* card listed it as a prerequisite or related concept, but no book actually documents it.

```
lfe> (lfeerlcpt-queries:ghost-concepts g)
(#"erlang-ports" #"os-monotonic-time" #"os-system-time")
```

Three of them. They are obvious gaps in the corpus — `os:system_time/0` is a standard library function that several cards depend on but nobody wrote a card for. This is a different kind of finding from anything we've seen so far: it's a *data quality* finding, surfaced as a structural property of the graph. "*Vertices with no incoming membership edges*" is the structural restatement of *"concepts I forgot to write up."* The graph has done the bookkeeping that a human reviewer would otherwise have to do by hand.

### 5.8 Weighted shortest paths: the minimum-surprise learning path

Everything we've done so far is structural — edges are present or absent. graffeo also carries weighted shortest paths (`dijkstra/2,3`, `astar/3,4`) with a pluggable cost function. That opens a qualitatively different question: not *can* you get from concept $X$ to concept $Y$, but what's the *gentlest* route — the path through the most-trusted relationships in the corpus.

The cards assert no explicit numerical weights, so we have to design them out of what the corpus *does* tell us. Every abstract-layer edge carries an `asserted_by` set — the list of books that drew that relationship. An edge asserted by six books is consensus; an edge asserted by one is a single author's opinion. A natural cost function:

$$w(u \to v) = \frac{1}{|\mathrm{asserted\_by}(u \to v)|}$$

A well-asserted edge has a low cost; an idiosyncratic one has a high cost. Then a shortest path from "a concept I know" to "a concept I don't" minimises the total *idiosyncrasy* — it routes through the well-trodden middle of the corpus, the edges every author drew.

This is also a beautifully self-contained cost function: it reads off the edge's own metadata and needs nothing else. That matters, because graffeo's `dijkstra` is specified to call its cost function as `cost(meta) -> number` — only the metadata of the edge being relaxed, not the endpoints. If we wanted "cost from the target's properties" (e.g. coverage breadth of the destination vertex), we'd have to pre-bake those properties into edge metadata with a graph rewrite before running Dijkstra. The assertion-count cost avoids that detour.

Here it is:

```lfe
(defun gentlest-path (g from-concept _to-concept)
  (let* ((pg (project-by-type g 'prerequisites))
         (cost (lambda (meta)
                 (case meta
                   ((map 'label (map 'asserted_by asserters))
                    (/ 1.0 (length asserters)))
                   (_ 1.0)))))
    ;; Returns a map of target -> #(distance predecessor).
    ;; Caller looks up `_to-concept` to reconstruct the path.
    (graffeo:dijkstra pg from-concept (map 'cost cost))))
```

`graffeo:dijkstra/3` takes the graph, a source vertex, and an options map; `'cost` is a function `meta -> non-negative number`. The result is a map of `target → {distance, predecessor}`, from which you can reconstruct the actual path.

(For completeness: if you wanted the original "1 / coverage(target)" weighting, you'd write a `weight-by-coverage` helper that walks every edge of the prerequisite projection, looks up the *target's* in-degree from source-layer cards, and re-adds the edge with a `weight` field set on the meta. Then graffeo's `default_cost` — which reads `weight` if present — picks it up automatically. The exercise is left to the curious reader; `coverage-of` from below gives you the per-target number.)

```lfe
(defun coverage-of (g slug)
  (let ((in-nbrs (graffeo:in_neighbours g slug)))
    (length
     (lists:usort
      (lc ((<- v in-nbrs) (is_tuple v))
          (element 1 v))))))
```

`coverage-of` isn't called by `gentlest-path` itself, but it's exported because it's the natural building block for the alternative weighting — and a useful query in its own right ("how widely is this concept documented?").

Why **Dijkstra** for this and not BFS? BFS gives you shortest paths *measured in number of edges*; Dijkstra gives you shortest paths *measured in total weight*, provided every edge weight is non-negative. The two coincide if every edge weight is 1. The non-negativity precondition is essential — Dijkstra's correctness proof leans on the fact that the first time a vertex is finalised, its tentative distance cannot be improved by going through a *further-away* vertex, which would have to add non-negative cost. With negative edges (which our weights don't have, but it's worth knowing why) you need Bellman–Ford, which is $O(V \cdot E)$ instead of Dijkstra's $O((V + E) \log V)$ with a binary heap.

For one-to-one queries (you have a specific source and a specific destination), **A***improves on Dijkstra by directing the search using a heuristic estimate of the remaining distance. graffeo's `astar/4` takes both a `'cost` and a `'heuristic` option, the latter being a function `target -> non-negative number` that estimates the remaining cost from `target` to the goal. If the heuristic is *admissible* (never overestimates the true remaining cost) and *consistent* (the triangle inequality holds), A* finds the optimal path while expanding far fewer vertices than Dijkstra. For our learning-path use, a useful admissible heuristic is the **graph-theoretic distance** (edge count) from `target` to the goal, scaled by the *minimum* edge cost in the graph — that gives a lower bound on the remaining weighted cost.

A real run, asking for the gentlest path from `pattern-matching` (well-known) to `recon-trace` (deep diagnostics territory):

```
lfe> (set result (lfeerlcpt-queries:gentlest-path g
                                                  #"pattern-matching"
                                                  #"recon-trace"))
;; reconstruct the path from `result` (a map of target -> {dist, pred})
#(#M(#"atom" 0.6666666666666666 #"erlang-term" 0.8333333333333333
     #"list" 0.3333333333333333 #"pattern-matching" 0
     #"single-assignment" 1.0 #"single-assignment-variable" 1.0
     #"term" 1.0 #"tuple" 0.3333333333333333 #"variable" 1.0
     #"variables" 2.0)
  #M(#"atom" #"tuple" #"erlang-term" #"list"
     #"list" #"pattern-matching"
     #"single-assignment" #"pattern-matching"
     #"single-assignment-variable" #"pattern-matching"
     #"term" #"pattern-matching" #"tuple" #"pattern-matching"
     #"variable" #"pattern-matching"
     #"variables" #"single-assignment"))
```

You'll find a short route weighted toward edges many books agreed on — well-trodden stepping stones. Compare with an unweighted shortest path (BFS-distance, edge-count optimal), and you'll often find a *longer* route — measured in edges — but a *gentler* one — measured in consensus mass. The point of the weighting is to spend more hops on familiar ground to land softer at the end.

This kind of pluggable cost function — bring your own metric, graffeo runs the search — is what makes the library suitable as a substrate for downstream domain reasoning. Curriculum design, dependency-aware build ordering, message routing in a known-topology cluster, even queuing-theoretic models on top of process graphs: all of them are shortest-path queries on top of a weight function that the domain author writes.

## 6. The runner

The runner ties the catalog into a single CLI entry point:

```lfe
(defmodule lfeerlcpt
  (export (main 1)
          (cards-dir 0)))

(defun cards-dir ()
  "../../workbench/ai-engineering/knowledge/erlang/concept-cards")

(defun main (_args)
  (let* ((base (cards-dir))
         (_ (io:format "=== Erlang Concepts Knowledge Graph (LFE) ===~n~n"))
         (_ (io:format "Building graph from ~s ...~n" (list base)))
         (result (lfeerlcpt-ingest:build-from-dir base))
         (g (mref result 'graph)))
    (print-summary g result)
    (print-prerequisite-cycles g)
    (print-learning-order g)
    (print-related-components g)
    (print-semantic-components g)
    (print-top-concepts g)
    (print-ghost-concepts g)
    (print-tunable-relatedness g)
    'ok))
```

Each `print-*` function is a thin display wrapper over a query — the same shape as the Erlang runner, line for line.

To drive the whole catalog from the shell, we need one tiny thing. `rebar3 lfe run` is built on top of `lfescript`, which expects a **script-style** LFE file: a top-level `(defun main (args) ...)` with *no* `(defmodule ...)` wrapper. Our `lfeerlcpt.lfe` is a proper module (so it can be called from anywhere in the project), which means it can't be fed straight to `lfescript`. The fix is a one-line shim:

```lfe
;; scripts/run-lfe-example.lfe
(defun main (args)
  (lfeerlcpt:main args))
```

That's the whole script — no module, no exports, just a top-level `main/1` that delegates to the real runner. Now this works:

```shell
rebar3 lfe run --main scripts/run-lfe-example.lfe
```

If you'd rather drop the `--main` flag, set the script as the project's default main once in `rebar.config`:

```erlang
{lfe, [{main, "scripts/run-lfe-example.lfe"}]}.
```

And then `rebar3 lfe run` works on its own.

From the REPL — no script needed — the call is direct:

```
lfe> (lfeerlcpt:main '())
```

The example's `make example` target still runs the Erlang version. If you want a shortcut for the LFE version, add to your `Makefile`:

```makefile
example-lfe: fetch-cards compile
 @rebar3 lfe run --main scripts/run-lfe-example.lfe
```

## 7. Exercises

Now that the catalog is in your hands, here are five exercises in increasing depth. The first three are direct queries you can run today; the last two are genuinely open.

1. **Most central within one book.** Restrict the abstract layer to just the cards in *Learn You Some Erlang*, run `top_k_by_degree`, and compare against the OTP Design Principles. Are LYSE's central concepts the same as OTPDP's, or do they differ? *Hint:* `graffeo:subgraph/2` lets you build a sub-view from a vertex list.

2. **Lonely concepts.** Of the 1,394 carded concepts, 1,213 appear in exactly one book. Find the ones that are *also* graph leaves (out-degree zero in the abstract layer) — the genuinely isolated ideas. These are candidates for either consolidation or removal.

3. **The ghosts.** `ghost-concepts/1` returns three. For each, look at the cards that *referenced* it and decide: should the corpus add a card for it, or is the reference a misnomer that should be rewritten?

4. **Longest chain.** In the *condensed* prerequisite graph, find the longest directed path. The length of that path is the deepest prerequisite stack in all of Erlang/OTP. *Hint:* on a DAG, longest path can be computed in $O(V + E)$ by dynamic programming over the reverse topological order. (On a general graph it's NP-hard. Condensation buys you the DAG.)

5. **Bridges and articulation points.** A **bridge** is an edge whose removal disconnects a connected component; an **articulation point** is a vertex with the same property. The classical algorithm finds them in one DFS pass, using low-link numbers analogous to Tarjan's SCC algorithm. graffeo doesn't ship this yet — but you have everything you need to write it. *Hint:* look at the small island components (§5.4) and see if any of them are held together by single vertices that, if removed, would split the island. Those are the concepts whose removal would most damage the connectivity of the local subgraph; they're worth knowing.

## 8. Where to go next

The graph algorithms we used — `components`, `cyclic_strong_components`, `condensation`, `topsort`, `top_k_by_degree`, `out_neighbours`/`in_neighbours`, `reachable`, `filter_edges`, `contract`, `dijkstra` — are the graffeo public surface. Every one is written against the read-half *behaviour* `graffeo_backend`, which means the same algorithm code runs over the value tier (the default, map-of-maps, immutable), the handle tier (`digraph`/ETS, mutable), and the forthcoming `dets` on-disk tier. Your queries above will run unchanged when you swap backends. This example is in fact the acceptance harness for the `dets` backend; the test is that all three backends return identical answers on the same corpus.

If you want to keep teaching yourself graph theory, three suggestions:

- Read **the [Tarjan papers](https://en.wikipedia.org/wiki/Robert_Tarjan)** on SCCs and on articulation points. They were short and full of insight when they were published in the 1970s; they still are. The interplay between DFS finishing order and low-link numbers is one of the small handful of genuinely beautiful results in algorithmics.

- Pick up **[Newman's *Networks* (2nd ed., 2018)](https://global.oup.com/academic/product/networks-9780198805090)** for the empirical, network-science end of graph theory — degree distributions, clustering coefficient, modularity, community detection. Most of what's in Newman applies directly to the kind of corpus-derived graph we built here.

- For the theorem-and-proof end, **[Diestel's *Graph Theory*](https://diestel-graph-theory.com/)** is the standard graduate text, with the lovely property of being free online from the author. The chapters on connectivity, flows, and matchings are particularly worth the time.

And if you want to keep using graffeo, the [project README](https://github.com/erlsci/graffeo) lists the rest of the API — `bfs`, `degree_centrality`, `astar`, `is_tree`, `is_arborescence`, `loop_vertices`, `get_short_path` — most of which we didn't touch in this tour. The shape is the same: a function on the graph, a clear definition behind it, a place in the literature you can chase if you want the math.

The corpus we used is real, the questions are real, and the answers are reproducible. The next graph you build will be different — perhaps a process-supervision graph, perhaps a node-distribution graph, perhaps a cluster-topology graph for a queue system you're writing — but the patterns transfer. *Filter to project a subgraph; reach across a layer with a join; condense to escape cycles; topsort to read an order; weight to ask for the gentlest path.* Those five moves cover most of what working programmers ever need from graph theory, and graffeo gives you each of them in a line or two of LFE.

## Appendix A: the rest of the parser

The body of the post showed `parse-file`, `parse-string`, `extract-frontmatter`, `collect-until-closing`, and `classify-line`. To make the file compile, here are the helpers I waved at earlier — they're direct translations of the Erlang originals and don't introduce any LFE idiom we haven't already met.

```lfe
(defun parse-kv
  ((key #"[]") (tuple 'kv key '()))
  ((key value) (tuple 'kv key (unquote-bin value))))

(defun unquote-bin
  (((binary "\"" (rest binary)))
   (case (binary:last rest)
     (34 (binary:part rest 0 (- (byte_size rest) 1)))
     (_ rest)))
  ((bin) bin))

(defun parse-fm-lines (lines)
  (parse-fm-lines lines 'undefined (map)))

(defun parse-fm-lines
  ((() _current-key acc) acc)
  (((cons line rest) current-key acc)
   (let ((trimmed (string:trim line)))
     (case (classify-line trimmed)
       ('empty (parse-fm-lines rest current-key acc))
       ('section-header (parse-fm-lines rest current-key acc))
       ((tuple 'list-item value)
        (case current-key
          ('undefined (parse-fm-lines rest current-key acc))
          (key
           (let* ((existing (maps:get key acc '()))
                  (new-list (case (is_list existing)
                              ('true (lists:append existing (list value)))
                              ('false (list value)))))
             (parse-fm-lines rest key (mset acc key new-list))))))
       ((tuple 'kv key value)
        (parse-fm-lines rest key (mset acc key value)))))))

(defun build-card (props)
  (let ((required (list #"slug" #"concept" #"category"
                        #"tier" #"source" #"source_slug")))
    (case (check-required required props)
      ('ok
       (tuple 'ok
              (map 'slug           (get-bin #"slug" props)
                   'concept        (get-bin #"concept" props)
                   'category       (get-bin #"category" props)
                   'tier           (get-bin #"tier" props)
                   'source         (get-bin #"source" props)
                   'source_slug    (get-bin #"source_slug" props)
                   'prerequisites  (get-list #"prerequisites" props)
                   'extends        (get-list #"extends" props)
                   'related        (get-list #"related" props)
                   'contrasts_with (get-list #"contrasts_with" props))))
      ((= (tuple 'error _) err) err))))

(defun check-required
  ((() _props) 'ok)
  (((cons key rest) props)
   (case (maps:is_key key props)
     ('true (check-required rest props))
     ('false (tuple 'error (tuple 'missing-field key))))))

(defun get-bin (key props)
  (let ((v (maps:get key props #"")))
    (case v
      (b (when (is_binary b)) b)
      (l (when (is_list l)) (iolist_to_binary (lists:join #", " l)))
      (_ #""))))

(defun get-list (key props)
  (let ((v (maps:get key props '())))
    (case v
      (l (when (is_list l)) l)
      (b (when (andalso (is_binary b) (=/= b #""))) (list b))
      (_ '()))))
```

## Appendix B: the rest of the ingest

§4 showed `build`, `add-source-layer`, `add-abstract-layer`, `add-membership`, `add-source-edges`, `add-card-source-edges`, `add-typed-edge`, and `project-source-edges`. The remaining helpers — the cross-only edge pass, the metadata merge, and the public reader functions — are below.

```lfe
(defun group-by-source (cards)
  (lists:foldl
   (lambda (card acc)
     (let ((src (mref card 'source_slug)))
       (maps:update_with
        src
        (lambda (existing) (lists:append existing (list card)))
        (list card)
        acc)))
   (map)
   cards))

(defun merge-type-meta
  (((map 'label (map 'types t1 'asserted_by a1))
    (map 'label (map 'types t2 'asserted_by a2)))
   (map 'label (map 'types (lists:usort (lists:append t1 t2))
                    'asserted_by (lists:usort (lists:append a1 a2)))))
  ((_acc new) new))

(defun merge-contracted-into (g contracted)
  (let ((cvs (graffeo:vertices contracted)))
    (lists:foldl
     (lambda (from g-acc)
       (let ((out-nbrs (graffeo:out_neighbours contracted from)))
         (lists:foldl
          (lambda (to g-acc2)
            (case (graffeo:edge_meta contracted from to)
              ((tuple 'ok meta)
               (case (graffeo:edge_meta g-acc2 from to)
                 ((tuple 'ok ex-meta)
                  (graffeo:add_edge g-acc2 from to
                                    (merge-type-meta ex-meta meta)))
                 ('error (graffeo:add_edge g-acc2 from to meta))))
              ('error g-acc2)))
          g-acc out-nbrs)))
     g cvs)))

(defun add-abstract-edges (cards g)
  (let ((g1 (project-source-edges g)))
    (add-cross-only-edges cards g1)))

(defun add-cross-only-edges (cards g)
  (let ((cards-by-source (group-by-source cards))
        (all-card-slugs (sets:from_list
                         (lc ((<- c cards)) (mref c 'slug))
                         '(#(version 2)))))
    (lists:foldl
     (lambda (card g-acc)
       (let* ((src (mref card 'source_slug))
              (src-cards (maps:get src cards-by-source))
              (local-slugs (sets:from_list
                            (lc ((<- c src-cards)) (mref c 'slug))
                            '(#(version 2)))))
         (add-card-cross-edges card local-slugs all-card-slugs g-acc)))
     g cards)))

(defun add-card-cross-edges (card local-slugs all-card-slugs g)
  (let* ((from-slug (mref card 'slug))
         (src (mref card 'source_slug))
         (rel-types (list (tuple 'prerequisites
                                 (mref card 'prerequisites))
                          (tuple 'extends
                                 (mref card 'extends))
                          (tuple 'related
                                 (mref card 'related))
                          (tuple 'contrasts_with
                                 (mref card 'contrasts_with)))))
    (lists:foldl
     (lambda (type-and-targets g-acc)
       (let (((tuple type targets) type-and-targets))
         (lists:foldl
          (lambda (target-slug g-acc2)
            (let ((is-local (sets:is_element target-slug local-slugs))
                  (is-self  (=:= target-slug from-slug)))
              (case (orelse is-self is-local)
                ('true g-acc2)
                ('false
                 (case (sets:is_element target-slug all-card-slugs)
                   ('true
                    (add-typed-edge g-acc2 from-slug target-slug
                                    type src))
                   ('false
                    (ensure-ghost-vertex g-acc2 target-slug
                                         from-slug type src)))))))
          g-acc (lists:sort targets))))
     g rel-types)))

(defun ensure-ghost-vertex (g target-slug from-slug type src)
  (let ((g1 (case (lists:member target-slug (graffeo:vertices g))
              ('true g)
              ('false (graffeo:add_vertex g target-slug)))))
    (add-typed-edge g1 from-slug target-slug type src)))

(defun source-vertices (g)
  (lc ((<- v (graffeo:vertices g)) (is_tuple v)) v))

(defun abstract-vertices (g)
  (lc ((<- v (graffeo:vertices g)) (is_binary v)) v))

(defun build-from-dir (base-dir)
  (let* ((dirs (filelib:wildcard (++ base-dir "/*")))
         (files (lists:sort
                 (lists:flatmap
                  (lambda (dir) (filelib:wildcard (++ dir "/*.md")))
                  dirs)))
         (cards (lists:map
                 (lambda (f)
                   (let (((tuple 'ok c) (lfeerlcpt-parser:parse-file f)))
                     c))
                 files)))
    (build cards)))
```

(If you named your modules differently — `lfec-*` rather than `lfeerlcpt-*`, say — adjust the `lfeerlcpt-parser:parse-file` call inside `build-from-dir` to match.)

## Sources

- The original example: [`graffeo/examples/erlang-concepts`](https://github.com/erlsci/graffeo/tree/main/examples/erlang-concepts) and its [Erlang README](https://github.com/erlsci/graffeo/blob/main/examples/erlang-concepts/README.md), which this post expands and rewrites in LFE.
- The corpus: [`billosys/ai-engineering`](https://github.com/billosys/ai-engineering), the Erlang concept-card knowledge base.
- graffeo itself: [`erlsci/graffeo`](https://github.com/erlsci/graffeo).
- LFE: [lfe.io](https://lfe.io), and the [`rebar3_lfe`](https://github.com/lfe-rebar3/rebar3_lfe) plugin used throughout.
