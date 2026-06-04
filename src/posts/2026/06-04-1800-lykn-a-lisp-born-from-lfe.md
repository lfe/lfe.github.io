---
layout: post.liquid
title: "Lykn: A Lisp Born from LFE"
description: "How fourteen years with Lisp Flavoured Erlang led to a new language — and how that language came home to power lfe.io."
permalink: "/blog/languages/2026/06/04/1800-lykn-a-lisp-born-from-lfe"
categories: ["languages"]
tags: [lfe, lykn, lisp, javascript, languages, release]
published_date: 2026-06-04 18:00:00 +0000
is_draft: false
data:
  author: duncan-mcgreggor
  written_for: null
  last_validated: null
  cover_image: "/images/LichenMoss_00183_.png"
  cover_alt: "Vigdís — lichen and moss on ancient stone, oil and digital"
  math: false
---

For years, the [lfe.io](https://lfe.io) front page has had a quiet little party trick: a tagline that cycles through LFE quotes (from various community members), powered by a `<script>` tag running [BiwaScheme](https://www.biwascheme.org/). BiwaScheme is a Scheme interpreter that runs in the browser — the kind of delightful hack that makes a Lisper smile. S-expressions on the LFE site, running live in the DOM. What's not to love?

Then, back in March, I gave BiwaScheme a hard look for use in another set of ECMAScript projects I needed to work on. [It turned out not to be a good fit](https://lykn.pl/posts/the-origin-of-lykn.html), and nor were any of the other Lispy JS alternatives. So naturally, two nights later an early version of [Lykn](https://lykn.pl) was released.

Lykn is a lightweight Lisp that compiles S-expressions to clean, readable JavaScript. No runtime dependencies in the compiled output. The first release that was usable in the browser was 10 times smaller than BiwaScheme (it's grown since then, but still much smaller).

## From BiwaScheme to Lykn

With the [0.5.0 release](https://lykn.pl/posts/the-road-to-lykn-0.5.0), Lykn was mature enough to replace BiwaScheme on the LFE site — and it felt right to celebrate the release by bringing it home.

Here's the old BiwaScheme code that powered the tagline cycler:

```scheme
<script src="/js/biwascheme.js">
  ;; Project: https://github.com/biwascheme/biwascheme
  ;; Docs: https://www.biwascheme.org/index.html
  (let* ([quotes (list "\"A proper Lisp.\""
                       "\"LFE combines the best of both Lisp and Erlang, at the SAME time!\""
                       "\"Taking the syntax out of distributed systems programming.\""
                       ...)]
          [num-quotes (length quotes)])
    (define get-quote (lambda ()
      (list-ref quotes (random-integer num-quotes))))
    (define set-quote (lambda ()
      (set-content! "#lfe-tagline" (get-quote))))
    (set-timer! set-quote 10))
</script>
```

BiwaScheme interprets Scheme at runtime — the entire interpreter ships to the browser, and the source runs inside the `<script>` tag's body. It worked well for years, but it meant bundling a 9,000-line interpreter for a ten-line script.

Here's the Lykn replacement:

```lykn
<script type="module" src="https://esm.sh/jsr/@lykn/browser@0.5.0"></script>
<script type="text/lykn">
  (bind quotes #a(
    "\"A proper Lisp.\""
    "\"LFE combines the best of both Lisp and Erlang, at the SAME time!\""
    "\"Taking the syntax out of distributed systems programming.\""
    ...))

  (genfunc cycle
    :args (:array items)
    :yields :string
    :body
    (while true
      (for-of item items (yield item))))

  (bind it (cycle quotes))
  (bind el (document:query-selector "#lfe-tagline"))

  (setInterval
    (fn ()
      (bind next (it:next))
      (set! el:text-content next:value))
    10000)
</script>
```

A few things changed. The random selection became a generator-based cycle — `genfunc` with `yield`, which compiles to a native JavaScript generator. The DOM mutation uses `set!` on properties instead of a library function. And Lykn's browser shim compiles the `<script type="text/lykn">` content to JavaScript on load, so there's no interpreter sitting in memory — just the compiled output running natively.

What's especially important to note: Biwa is an _actual Scheme_. Lykn is _a Lisp flavour_. Most of Lykn's language-level choices are based upon its ECMAScript 2025 core, and it never really strays far from that core. And don't let the Lykn macros fool you! They expand to what is essentially S-expression ECMAScript.

## What's Happening in 0.6.0

The 0.5.0 release was the "it works for real projects" milestone. The current 0.6.0 cycle is about making the language genuinely capable. A few highlights from what's been landing:

**ICU MessageFormat as a first-class surface form.** Lykn's `template` macro now supports full ICU MessageFormat — named parameters with plural, select, and interpolation forms. We're also going to extend this capablity to a pre-compiler step, allowing users to define all the language using i8n (which may get pushed to 0.7.0). As far as we can tell, this makes Lykn the first Lisp with built-in internationalization in a core language construct, not bolted on as a library.

For instance, these will be supported:

```lykn
;; lang: zh
(绑定 名字 "邓肯")
(函数 问候
  :参数 (:字符串 名字)
  :返回 :字符串
  :体 (模板 "你好，{名字}！" :名字 名字))

(控制台:日志 (问候 名字))
```

and

```lykn
;; lang: ru
(привязка имя "Дункан")
(функция приветствие
  :аргументы (:строка имя)
  :возвращает :строка
  :тело (шаблон "Привет, {имя}!" :имя имя))

(консоль:лог (приветствие имя))
```

The goal is that i18n isn't something you add later; it's something the language already knows how to do.

**Compiler overhaul.** The two compilers (JS and Rust) have been through a significant alignment pass. Conditionals (`if`, `match`, `if-let`, `when-let`) now compile position-aware — an `if` in expression position becomes a ternary, while the same form in statement position becomes an `if`-statement. Lisp-style identifier conventions (`valid?`, `swap!`) now map correctly to JavaScript naming (`isValid`, `swap`), with collision detection at compile time. The Rust compiler can now load JS-defined surface macros, closing a major gap between the two compilation paths.

**Packaging ecosystem for library authors.** The Lykn binary is now fully self-contained: `cargo install lykn-cli` just works, with the JS compiler embedded at build time and lazily materialized on first use. For library consumers, we've making big improvements in importing from jsr.io and NPM, including using Lykn macros from other projects/dependenceis. Additionally, Lykn can now generate TypeScript `.d.ts` definitions directly from `:type` annotations in function signatures, so published Lykn packages get full IDE autocomplete in downstream TypeScript projects.

There's more — the [Lykn blog](https://lykn.pl/blog/) has detailed write-ups of each release — but those three threads capture the shape of where the language is heading next.

## The LFE of It All

I should say plainly what's obvious to anyone who's looked at both languages: Lykn wouldn't exist without fourteen years of LFE.

The surface syntax, the way macros work, the instinct to reach for pattern matching, the preference for small composable forms over sprawling special syntax, the conviction that a Lisp can be a practical tool for real systems and not just an academic exercise — all of that is LFE. The approach to error handling (let it crash, but crash cleanly) came from Erlang through LFE. The idea that a language should compile to something readable and idiomatic in the host platform, rather than treating the target as an instruction set to be abused. And maybe most of all, the fact that Lykn transparently supports and respects the underlying "root" language — that's Robert Virding's design philosophy for LFE on the BEAM, applied to JavaScript.

To that point, Robert's influence on how I think about languages is hard to overstate. The technical decisions matter — LFE's macro system, its approach to Erlang interop, its refusal to paper over the host platform — but the deeper thing is a way of _thinking_ about what a language is for. A language is a tool for thought that happens to also run on a computer. That's what I learned from working with Robert, and it's the lens through which every design decision in Lykn gets made.

If you're curious about Lykn, the site is at [lykn.pl](https://lykn.pl) and the source is on [GitHub](https://github.com/lykn-lang/lykn). And if you visit [lfe.io](https://lfe.io) today, watch the tagline — those parentheses are Lykn now :-)
