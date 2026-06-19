# LFE Friday — Post Template

*Generated: 2026-06-12. Pair with `lfe-friday-style-guide.md`.*

Copy the block below into `src/posts/YYYY/MM-DD-HHMM-<slug>.md` and
replace every `«…»`. Slug rule: title lowercased, punctuation stripped;
`" - "` → `---`, `:` and `/` deleted (`LFE Friday - maps:get/3` →
`lfe-friday---mapsget3`). Tag/category rule: no spaces in any tag or
category — use hyphens (`lfe-friday`, not `lfe friday`). HTML comments
are guidance — delete them all before publishing.

````markdown
---
layout: post.liquid
title: "LFE Friday - «module:function/arity»"
description: "«One sentence: what the function does and the one interesting thing this post shows about it.»"
permalink: "/blog/tutorials/«YYYY»/«MM»/«DD»/«HHMM»-lfe-friday---«slug»"
categories: ["tutorials"]
tags: [lfe-friday, lfe, erlang, «module», «concept-tags»]
published_date: «YYYY-MM-DD HH:MM»:00 +0000
is_draft: false
data:
  author: «authors.yml key, e.g. duncan-mcgreggor»
  written_for:
    lfe: "«e.g. 2.2»"
    erlang: "«e.g. 28»"
  last_validated: null
  cover_image: "/images/fridays/«LFE_Friday_XXXXX_.png»"
  cover_alt: "Vigdís — LFE Friday, «scene description»"
  math: false
---

<!-- 1. RITUAL OPENER — one sentence; vary the verb (is / is on / covers /
     digs into / continues / takes a turn). Arc installments add a
     one-line recap link: "In [last week's LFE Friday](…) we …" -->
Today's LFE Friday «is/covers» [«module:function/arity»](https://www.erlang.org/doc/apps/«app»/«module».html#«function»/«arity»).

<!-- 2. PLAIN-WORDS CONTRACT — what it takes, what it returns. No
     typespecs. One short paragraph. -->
`«module:function/arity»` takes «arguments in plain words», and returns «result in plain words».

<!-- 3. HAPPY PATH — 3–7 calls, real unedited output, prompt is "> ". -->
```lfe
lfe> («module»:«function» «args»)
«actual output»
lfe> («module»:«function» «args»)
«actual output»
```

<!-- Short observation sentence between blocks. Keep paragraphs 1–3
     sentences; let whitespace do the pacing. -->
«Observation about what we just saw.»

<!-- 4. PROBE — break it on purpose; show the real exception. Narrate
     honestly ("Now let's try to break this a bit…"). -->
«Lead-in: what we're about to try and why it might misbehave.»

```lfe
lfe> («module»:«function» «bad args»)
exception error: «actual error»
```

«What the failure tells us.»

<!-- 5. DEEPER CUT (the 20%) — one or two paragraphs max. Name the
     underlying concept precisely (bold at first use), state the cost
     (O(...) + one timing if it matters), note OTP-version history
     ("arrived in OTP «N»", "replaces «old thing»"), be honest about
     the trade-off. Optionally ONE outward link. If math is needed,
     flip `math: true` above. -->
«What is really going on underneath, in two paragraphs or fewer.»

<!-- 6. SIBLINGS — companion functions / other arities; demo at least
     one briefly. This is how the series teaches the module. -->
The `«module»` module also provides [«sibling/arity»](https://www.erlang.org/doc/apps/«app»/«module».html#«sibling»/«arity»), which «one-line contrast».

```lfe
lfe> («module»:«sibling» «args»)
«actual output»
```

<!-- 7. CLOSE — arc posts: trail next week ("Next week, we will …").
     Standalone posts: a one-sentence practicality verdict ("While the
     odds are low you'll reach for this daily, …"). No signature line —
     the author byline is rendered at the top of the post. -->
«Closing line.»
````

## Pre-publish checklist (from the style guide, §9)

1. Opener links the current Erlang docs.
2. All outputs from a real shell on the `written_for` versions; blocks
   replay cleanly top-to-bottom.
3. At least one probe with a real exception shown.
4. At least one sibling function demonstrated.
5. Exactly one deeper cut, ≤2 paragraphs.
6. No headings/bullets in the body unless the post genuinely turns a
   corner; ~250–450 words incl. code (~650–1,100 for arc installments).
7. No signature line (author byline is rendered at the top); arc recap +
   trail lines if in an arc.
8. Frontmatter: permalink ↔ filename ↔ published_date agree;
   `written_for` filled; cover image from `/images/fridays/`; guidance
   comments deleted.
