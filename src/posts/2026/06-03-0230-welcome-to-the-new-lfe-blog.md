---
layout: post.liquid
title: "Welcome to the New LFE Blog"
description: "After many years on Jekyll, the LFE blog has moved into the same Cobalt-powered home as lfe.io itself. Here's what changed, what stayed, and what we learned along the way."
permalink: "/blog/tutorials/2026/06/03/0230-welcome-to-the-new-lfe-blog"
categories: ["announcements"]
tags: [site, web, news, cobalt, vigdis, liffybot]
published_date: 2026-06-03 02:30:00 +0000
is_draft: false
data:
  author: duncan-mcgreggor
  written_for: null
  last_validated: null
  cover_image: "/images/newsroom/LFE_Newsroom_00310_.png"
  cover_alt: "Vigdís — LFE news, a busy rotating space-station newsroom"
  math: false
---

Back in April, we announced [new build tools for the LFE website](/blog/tutorials/2026/04/20/0005-lfeio-50-rust-tooling-all-the-way-down). At that time we teased that we might be migrating the LFE blog, too -- something that has been on the same infra and tooling for 12 years. Twelve. Years. Yes, this is my same face. These are tears of shame.

So, we've now done something about that.

**The LFE blog has a new home: it's right here, at [lfe.io/blog](https://lfe.io/blog).** Same posts, same authors, same URLs (more on that in a moment), but a brand new build pipeline, a brand new look, and — finally — a brand new platform that the LFE community can hack on without first installing the right version of Ruby.

## Why We Moved

The old blog was built on Jekyll, on top of [Jekyll-Bootstrap](http://jekyllbootstrap.com/) with a heavily customised version of the Hooligan theme. When it was set up in 2014, that was a perfectly reasonable choice. By 2026, it had quietly turned into a museum:

* The CSS was [Twitter Bootstrap 2](https://getbootstrap.com/2.3.2/) (deprecated in 2013).
* The JavaScript was [jQuery 1.7.2](https://blog.jquery.com/2012/03/21/jquery-1-7-2-released/) (from 2012).
* The analytics were Universal Analytics, which Google sunsetted in [July 2024](https://support.google.com/analytics/answer/11583528).
* There was an IE 9 HTML5 shim still loaded in the page header.
* Every contributor needed Docker installed to build the site, because we'd long since stopped trusting anyone's local Ruby setup.

The old chrome wasn't *broken*, exactly. It still worked, and the posts still rendered. But it was very visibly from another decade, and the build was just enough friction that nobody wanted to write a post if they didn't have to. With [lfe.io v5.0.0](/news/2026/04/20/lfe-io-v5-0-0-from-zola-to-cobalt) freshly shipped on Cobalt, the gap between "the project home page" and "the project blog" had become deeply, genuinely awkward. So we decided to fold the two together.

There was also a deeper motivation, which I want to name because it ended up shaping a lot of the design. The old blog was built like a normal modern blog: newsletter signup at the bottom, share buttons, comments (well, comments were disabled, but the *shape* was still there), an analytics tracker watching every page view. Looking at it through 2026 eyes, all of that machinery felt like it was for someone else's blog. The LFE blog has always been a technical archive — long-tail tutorials and language notes that someone, someday, will find when they need them. **If one person reads one article in one year and somehow that helps them out: mission accomplished.** That's the whole game. So this migration was also an opportunity to strip out everything that wasn't serving that reader.

## Design Decisions

Here's what we settled on, in rough order of how much each shaped the rest:

**1. Fold the blog into `lfe.io/blog/`.** One repo, one CI, one nav, one theme system. Posts live in `src/blog/_posts/` in the main `lfe.github.io` repo. The blog inherits the v5.0.0 Tailwind setup, dark mode, fonts, and chrome.

**2. Keep `blog.lfe.io` alive forever as a redirect router.** Every old URL — every post, every category page, every tag page, every author page, the archive, the atom and rss feeds, all of them — still works. Hit `blog.lfe.io/tutorials/2019/05/13/1549-running-lfe-in-docker-updated` today and you'll land on the equivalent page on the new home, with a clean 301. RSS subscribers don't churn. Old StackOverflow links don't rot. Planet Erlang's archive doesn't sprout 404s. (For the curious: the old repo is now a tiny static site whose only job is to map old paths to new ones. We're rather pleased with it as a pattern.)

**3. Every post is a painting.** This one is the most fun design decision we've made in a long time, so it gets its own section below.

**4. The blog post just *ends.*** No newsletter CTA. No share buttons. No comments. No "you might also like" infinite scroll. No analytics. The post finishes, you see some metadata so you don't have to scroll to the top, and the page ends. No fanfare.

**5. Version banners for context.** Every post that shows the use of Erlang and/or LFE will display metadata about which version of LFE and Erlang/OTP for which it was written. When you land on an old post, you'll see — clearly, at the top — that it was written for (say) LFE 0.10 / OTP 17, with a small note that current LFE is 2.2. The escalation goes from gentle ("Tested against LFE 2.x") for fresh content, to visible info banners ("Some details may have changed"), to outright warnings on the older corpus. Every existing post lands in the warning tier on day one, which is correct — they really *are* for old versions. We've been wanting this for years. Note that this work is in progress, as we a currently curating a list of correct versions, times of release, and reviewing all the old posts.

**6. JSON Feed alongside Atom and RSS.** All three live at canonical paths, because giving people choice in how they subscribe costs us nothing and helps everyone.

**7. Pagefind for search.** I've spent more hours than I care to admit ⌘-F-ing my way through old posts trying to find something Robert wrote about ETS in 2015. [Pagefind](https://pagefind.app/) is a build-time search indexer that ships as a self-contained binary, which makes it a perfect fit for the "no Node, no npm, no Docker" toolchain. Try it.

## The Visual Direction: Vigdís

The painting thing deserves its own section because it's such a different way of thinking about a tech blog.

We wanted the blog to feel like a long-form reading site, with a visual register that hinted of scifi (because), of retro-futurism (because Lisp), and of something warm and painterly, maybe even calling back to the late-1970s register of PDP-11s and Lisp Machines and beautiful computer consoles. You know, the era LFE's two parent languages came up in.

So we did something weird and wonderful: every post on the new blog is illustrated by a fictional artist named **Vigdís Ljósadóttir** — a painter we invented as part of a separate project, with a defined style somewhere between Roger Dean, John Berkey, and Syd Mead. We feed her prompts through Stable Diffusion / ComfyUI, and out come paintings. We plan to keep adding more -- for visual freshness ... and wonder :-D

## Difficulties Encountered

Not everything was smooth, of course :-) A few of the gotchas, in case anyone else is doing a Jekyll→Cobalt migration:

**Every single one of our 116 posts had Liquid in its body.** Every one. There was a `{% raw %}{% include JB/setup %}{% endraw %}` at the top of every post (effectively a no-op from Jekyll-Bootstrap's variable-assignment machinery), 56 of them had a `{% raw %}{% include LFEFriday/setup %}{% endraw %}` (the translator-attribution paragraph for the LFE Friday series), two had a `{% raw %}{% include MathJax/setup %}{% endraw %}` (script tag for math), and 38 of them used `{% raw %}{{ site.base_url }}{% endraw %}` interpolation inside the markdown body for image paths. Migrating any one of those by hand is easy. Migrating 116 of them turns into a real corpus-rewrite job. We ended up building a new subcommand for `lfesite` called `blog-migrate` that did the whole sweep mechanically, which we recommend over the alternative.

**The permalink shape was specific.** Jekyll's `:title` placeholder strips only the `YYYY-MM-DD-` prefix from filenames, but our filenames had a `YYYY-MM-DD-HHMM-` shape, so the four-digit "HHMM" segment ended up baked into every URL slug. That looks quirky from the outside, but it's the URL contract every inbound link depends on, so we preserved it byte-for-byte. The Cobalt permalink template needed `{% raw %}{{ slug }}{% endraw %}` to behave the same way; configuration was a five-minute job once we understood what we were preserving.

**Code fence language hints were inconsistent.** A lot of the older posts used ` ```cl ` or ` ```lisp ` to mark LFE code, because at the time those felt like the closest available syntax hints. We did one corpus-wide rewrite to normalize all of them to ` ```lfe `, which lets [syntect](https://github.com/trishume/syntect) (Cobalt's syntax highlighter) do the right thing in one place.

**Twenty-five percent of posts had inline image floats baked into the body HTML.** `<a><img class="left thumb">` and friends, leaning on theme CSS class names from `lfetheme.css`. Rather than rewrite those, we ported the class names into the new chrome and added mobile-friendly degradation that drops the floats below a breakpoint. The 2014 image floats survive intact on desktop; on phones, the images go full-width and you get to read.

**Eighty out of 116 posts had `description: ""`** as empty front-matter. That's a thing to discover at template-rendering time, not at meta-tag-emission time. We made `description` fall back to first-paragraph excerpt for OG / Twitter card metadata, while leaving the visible field absent on the page.

The biggest surprise overall wasn't any one of these — it was how *much* of a 2014-era Jekyll site is invisible accumulation. Three dead theme directories. Six unused asset folders. CSS for a Bootstrap version nobody runs anymore. The migration was as much about *what to leave behind, buried in a deep hole* as what to carry forward.

## Unexpected Successes

Some of the wins came out of left field, which is always the best kind of win:

* **The visual canon is now a real thing.** When we sat down to design this, the worst-case outcome was "we'll throw a couple of stock images at it." The actual outcome was *every post becomes a commissioned painting in a defined style*. That's a different category of blog. Sites like Stratechery and Increment have done versions of this; nobody in the BEAM world has, as far as I know.

* **The redirect-router pattern is a keeper.** Keeping the old CNAME alive as a tiny 301-issuing static site costs almost nothing, preserves every old URL forever, and means we never have to think about link rot for inbound links to this blog again. We'll be using this pattern for other things.

* **Stripping out the engagement machinery felt great.** It is a strange and lovely thing to publish a blog that has no metrics. We won't ever know if you read this. We're choosing not to know. The work feels different already.

* **The bootstrap-doc workflow paid off twice.** When we did the main site migration in April, we left behind two documents — one capturing the high-level handoff plan for the next migration, one capturing the technical learnings — and reading them this round was like having a very thoughtful colleague hand us a packet at the start of the project. Recommended to anyone running a long multi-stage migration where the team rotates.

* **Pagefind landed in an afternoon.** Genuinely. The whole thing was simpler than I expected, with zero infrastructure beyond the build step.

* **The version-banner system might end up being the most useful thing this blog has ever shipped.** I'm including a long line about this so the metaphor lands: think about how many times you've landed on a five-year-old technical blog post and had to *guess* whether it still applied to the current version of whatever-it-is. The new banner means you don't have to guess on this blog. Ever. As soon as we've finished with the feature. Mmm-hmm. Take that for what it's worth.

## What's Next

A few things, in no particular order:

* More posts! Yes, really. The new toolchain has made this *radically* easier to contribute to. If you've been sitting on a tutorial draft, now is the moment.
* More paintings. We're working through the back catalog at a steady clip, and the masthead piece is just the start.
* Concretizing the version-banner thresholds — there's a separate piece of work coming that involves digging through actual LFE and Erlang/OTP release history to ground the "old" / "very old" tier boundaries in real version arithmetic.
* The 2020 rebar3_lfe drafts that have been sitting in `_drafts/` for six years :-) They are now visible in the new system and will get cleaned up and shipped soon.

Until then: welcome to the new place. Pull up a chair. Robert, myself, and he rest of the community are glad you're here.
