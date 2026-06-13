# LFE Friday — Writing Style Guide

*Generated: 2026-06-12. For humans and LLMs writing new LFE Friday posts.*

This guide distills the voice of the 56 original LFE Friday posts
(2015–2016, translated from Steven Proctor's Erlang Thursday by Robert
Virding) and blends in a measured dose of the register established by the
2026 graffeo graph-theory post. The target mix is **80% classic LFE
Friday, 20% science-writer**. If a sentence could appear in either
register, prefer the classic one.

Companion documents: `lfe-friday-post-template.md` (the skeleton),
`lfe-friday-core-plan.md` / `lfe-friday-extended-plan.md` (what to write),
`lfe-friday-current-state.md` (what's already covered).


## 1. What an LFE Friday post is

A bite-sized, REPL-driven tour of **one function, one small function
family, or one focused concept** from Erlang/OTP, written in LFE. The
reader should be able to read it with their morning coffee, open a shell,
and reproduce every line. It is a weekly ritual, not a treatise.

- **Scope:** one `module:function/arity`, or 2–4 tightly related
  functions (a function plus its siblings), or one installment of a
  multi-week arc (à la the ETS and digraph arcs).
- **Length:** the classic standalone posts run **roughly 250–450 words
  including code**; arc installments run **~650–1,100**. (Measured
  against the corpus: `lists:any/2` is 318, `calendar:valid_date/3` is
  287, ETS intro part 1 is 724, the big select-with-limit post is
  1,012.) If you're past 1,300, you have two posts.
- **The REPL is the narrative spine.** Prose connects shell sessions;
  shell sessions carry the argument. Every claim about behavior is
  *demonstrated*, never merely asserted.


## 2. The canonical shape

Posts follow this sequence (the template encodes it):

1. **The ritual opener.** One sentence beginning "Today's LFE Friday…"
   that names the subject and links to the official Erlang docs:

   > Today's LFE Friday function of the week is
   > [lists:any/2](https://www.erlang.org/doc/apps/stdlib/lists.html#any/2).

   Variants used in the corpus: "…is on X", "…covers X", "…digs a little
   into the `queue` module…", "…continues the introduction to ETS…",
   "…takes a turn down a slightly different route…". Vary the verb, keep
   the ritual.

2. **The plain-words contract.** One short paragraph stating what the
   function takes and what it returns, in ordinary language. No
   typespecs, no formality:

   > `lists:any/2` takes a predicate function as the first argument, and
   > a list to iterate over as its second argument. `lists:any/2` returns
   > `true` if the predicate function returns `true` for any of the
   > elements in the given list, otherwise, `lists:any/2` returns `false`.

3. **The happy path.** A REPL block showing 3–7 straightforward calls
   with **real, unedited outputs**.

4. **Probing.** This is the soul of the series. Try to break it. Pass
   the wrong type, an empty list, a negative number, a list where a
   queue is expected. Show the actual exception. Narrate honestly:

   > Now let's try to break this a bit and test to see how it can
   > handle `0`'s and negative integer values.

   > If we try to pass a list in to `queue:cons/2`, we see that it does
   > want a queue, and will not do an implicit conversion.

5. **The deeper cut** *(the 20% — see §4)*. One or two paragraphs that
   name what is really going on underneath: the complexity class, the
   semantics, the data-structure choice, the OTP-version history, the
   honest trade-off. At most one of: a small table, a $\LaTeX$ formula,
   a definition set in bold.

6. **Siblings and neighbors.** Mention the companion functions and show
   at least one of them briefly: `lists:any/2` → `lists:all/2`;
   `queue:cons/2` → `queue:snoc/2`; the other arities. This is how the
   series teaches the *module*, not just the function.

7. **The sign-off.** Series-continuity line if in an arc ("Next week,
   we will…"), then the signature (§6).

Steps 3–6 can interleave — the corpus often probes, explains a little,
probes again. The order above is the default, not a straitjacket.


## 3. Voice — the classic 80%

- **First-person plural, present tense, thinking out loud.** "We", "us",
  "let's". The post reads like a live shell session annotated by a
  friendly colleague: "Let's see what this would look like." "Time to
  crash the process again." "So let's pass that module to `c:xm/1` and
  see what we get."
- **Honest discovery, including dead ends.** The corpus narrates
  surprise rather than editing it out: "Hrmm... Okay, looks like that
  vertex is in there." "Thinking this was odd for a Erlang API, I went
  to the LFE shell…" If the function's behavior surprised you, let it
  surprise the reader in the same order.
- **Plain claims, immediately demonstrated.** Never say "X is eager"
  without the `timer:tc` block that shows it.
- **Light humor, dry and structural** — a Latin heading ("HC SVNT
  DRACONES"), a wry aside ("as good \"engineers\"…"), an immutable
  bookmark metaphor. Never forced, never more than once or twice a post.
- **Short paragraphs.** One to three sentences. Whitespace is pacing.
- **Almost no headings** in a standard post. Headings (`##`) earn their
  place only when a post makes a genuine turn (the digraph post's
  dragons), or in long arc installments. Never use headings as
  decoration.
- **No bullet lists in the body** of a standard post. Prose and code.
  (Tables are permitted in the deeper cut; see §4.)
- **Practicality verdicts welcome.** The corpus is comfortable saying
  "the odds are low that you will need this in your day-to-day work,
  but…" — say when something is niche, and why it's still worth knowing.


## 4. The science-writer 20%

This is the graffeo-post register, used *sparingly* — roughly one to two
paragraphs per post, in the "deeper cut" slot. What to borrow:

- **Name the concept precisely, once.** Bold the term at first use, give
  the real definition in one sentence, then return to plain speech.
  "This is **amortized O(1)**: the occasional expensive rebuild is paid
  for by the cheap operations around it."
- **State costs.** If the function is $O(n)$, say so, and show what that
  means with one timing. If two siblings differ in complexity, that
  difference *is* the deeper cut.
- **Be honest about trade-offs.** "Reach for the heavier measure when
  the cheap one disagrees with your intuition, not before" is the model
  sentence. No tool-worship, no hedging mush.
- **Real numbers, reproducible runs.** Outputs are pasted from an actual
  shell, never invented or "tidied". If output is shortened, show the
  natural `...)` truncation the shell itself produces.
- **Version archaeology.** Note when a function arrived ("`maps:merge_with/3`
  is OTP 24+"), what it replaced, and what the modern alternative is if
  the subject is legacy. This grounds the revival posts the way the
  graffeo post grounds its theorems.
- **One pointer outward, at most.** A single "if you want to go deeper"
  link (a paper, a book chapter, the OTP Design Principles) at the end
  of the deeper cut. Not a reading list.

What **not** to borrow for a standard post: numbered section headings,
theorem–proof blocks, exercises, appendices, a Sources section, and
multi-table data analysis. Those live in big standalone tutorials, not
LFE Friday. If the deeper cut wants to be 40% of the post, the topic
wants to be a standalone tutorial — write the LFE Friday post as the
teaser and link forward.

**Math:** permitted, with `math: true` in the frontmatter, when a
formula genuinely says it shorter than a sentence ($O(V + E)$, a cost
function). One or two inline expressions; display math only in
exceptional posts.


## 5. Code conventions

- All shell blocks are fenced with ` ```lfe `; non-LFE shell commands
  (rarely needed) use ` ```shell `.
- REPL lines start with the prompt `>` followed by one space. Show the
  output exactly as printed, unindented, on the lines following.
- Inline code uses single backticks: `` `lists:any/2` ``. (The 2015
  corpus used ``double backticks``; new posts use the modern
  convention.)
- Refer to functions as `module:function/arity` on first mention, linked
  to the current docs at
  `https://www.erlang.org/doc/apps/<app>/<module>.html#<function>/<arity>`.
- Code that builds up state across blocks must actually work when typed
  in order — posts are replayed by readers, top to bottom. Use `set` for
  shell bindings exactly as the shell session did.
- Pin reality: run everything on the LFE/OTP versions declared in the
  post's `written_for` frontmatter, and re-paste outputs if you bump
  versions before publishing.


## 6. Attribution and signature

- The 2015–2016 posts each carried a header crediting Erlang Thursday
  and closed with the two-surname signature `- Proctor, Robert` (Steven
  Proctor, original author; Robert Virding, translator). **New original
  posts drop the per-post Proctor header** — the series' lineage is
  credited once, on the series/announcement page.
- **Keep the signature ritual.** Close every post with a signature line:
  - Single author: `- McGreggor`
  - Adaptation or co-written post: original surname first, adapter
    second, honoring the old form: `- <Original>, <Adapter>`
- The frontmatter `data.author` must name a key from
  `src/_data/authors.yml` (e.g. `duncan-mcgreggor`, `robert-virding`).
  Add new authors there before first use.


## 7. Frontmatter and file conventions

File path: `src/posts/YYYY/MM-DD-HHMM-<slug>.md`, where `<slug>` is the
title lowercased with punctuation stripped — note that `" - "` in the
title becomes `---` and `:` and `/` vanish, e.g.
`LFE Friday - lists:any/2` → `lfe-friday---listsany2`.

Required frontmatter (see the template for the full block):

- `title`: `"LFE Friday - module:function/arity"` or
  `"LFE Friday - <Topic>, part N"` for arcs.
- `permalink`: `/blog/tutorials/YYYY/MM/DD/HHMM-<slug>` — must agree
  with the file path and `published_date`.
- `categories`: `["tutorials"]`. Tags: always `[lfe friday, lfe,
  erlang]`, plus the module name and any concept tags.
- `data.written_for`: **fill it in** (this drives the site's version
  banner): the LFE and Erlang/OTP versions the post was tested against.
- `data.cover_image` / `cover_alt`: an LFE Friday Vigdís painting from
  `/images/fridays/` (warm Ghibli-grassroots register). Reuse from the
  existing pool until the post gets its own.
- `data.math`: `false` unless the post uses LaTeX.


## 8. Series mechanics

- **Arcs** are the series' strongest device: announce the arc in the
  first installment ("…starts the beginning of an intro to the `ets`
  module"), recap with a link in each subsequent one ("In
  [last week's LFE Friday](…) we…"), and trail the next ("Next week, we
  will…"). Standalone posts skip all three.
- **Bonus posts** (`LFE Friday Bonus - …`) are for off-cadence
  curiosities, usually performance investigations.
- Posts within an arc must each stand alone well enough that a reader
  arriving from a search engine isn't lost — one sentence of context
  buys this.


## 9. Checklist before publishing

1. Opener names the function and links the current Erlang docs.
2. Every output was produced by a real shell on the `written_for`
   versions, and blocks replay cleanly top-to-bottom.
3. At least one probe — something is broken on purpose and the
   exception is shown.
4. At least one sibling function appears.
5. Exactly one deeper cut, and it earns its length (§4).
6. No headings/bullets unless the post genuinely turned a corner.
7. Word count (incl. code): ~250–450 (standalone) / ~650–1,100 (arc
   installment).
8. Signature line present; arc continuity lines present if in an arc.
9. Frontmatter complete; permalink ↔ filename ↔ published_date agree;
   `written_for` filled; cover image set.


## 10. Calibration examples

**Too dry (0% Friday):**
> `lists:any/2` evaluates Pred over List, short-circuiting on the first
> true. Complexity is O(n) worst case.

**Too heavy (50% science-writer):**
> We now define the predicate-satisfaction problem formally. Let
> $P: T \to \mathbb{B}$… *(three paragraphs of formalism before the
> first REPL block)*

**Right (80/20):**
> `lists:any/2` is eager, and will return with a result of `true` as
> soon as it is found, ignoring the rest of the list. We can see the
> short-circuit pay off by timing both halves of the same question:
>
> ```lfe
> > (timer:tc 'lists 'any (list (lambda (x) (== (rem x 2) 1)) (lists:seq 2 200000 2)))
> #(171661 false)
> > (timer:tc 'lists 'any (list (lambda (x) (== (rem x 2) 0)) (lists:seq 2 200000 2)))
> #(19 true)
> ```
>
> That's the whole story of **short-circuit evaluation** in one tuple:
> the worst case walks all $n$ elements, the best case walks one.
