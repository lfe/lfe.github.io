---
layout: post.liquid
title: "The Hidden Origins of Lisp: Alonzo Church"
description: ""
permalink: "/blog/excerpts/2015/03/25/1108-the-hidden-origins-of-lisp-church"
categories: ["excerpts"]
tags: ["books", "documentation", "education", "history", "lisp", "mathematics", "sicp"]
published_date: 2015-03-25 11:08:00 +0000
is_draft: false
data:
  author: duncan-mcgreggor
  written_for: null
  last_validated: null
  cover_image: "/images/default/LFE_00255_.png"
  cover_alt: "Vigdís — LFE, retro-futurist digital painting"
  math: true
---
<a href="/blog/assets/images/posts/Alonzo-Church.jpg"><img class="right medium" src="/blog/assets/images/posts/Alonzo-Church.jpg" /></a> Alonzo Church: Creator of the λ&#8209;calculus and articulator of the Church–Turing thesis.

This is one of a series of posts tracing the origins of Lisp through four brief
biographical vignettes of individuals whose contributions to mathematics
ultimately supported the creation of Lisp:

 * [Introduction](/blog/excerpts/2015/03/22/1445-the-hidden-origins-of-lisp-introduction/)
 * [Giuseppe Peano](/blog/excerpts/2015/03/23/1008-the-hidden-origins-of-lisp-peano/)
 * [Bertrand Russell](/blog/excerpts/2015/03/24/0111-the-hidden-origins-of-lisp-russell/)
 * [Alonzo Church](/blog/excerpts/2015/03/25/1108-the-hidden-origins-of-lisp-church/)
 * [John McCarthy](/blog/excerpts/2015/03/26/1111-the-hidden-origins-of-lisp-mccarthy/)
 * [The Place of Lisp in the 21st Century](/blog/excerpts/2015/03/27/1101-the-hidden-origins-of-lisp-future)

The material presented in these
posts has been taken from early drafts of the new preface for the LFE edition
of *Structure and Interpretation of Computer Programs*
([book](http://lfe.gitbooks.io/sicp/content/index.html),
[source](https://github.com/lfe/sicp)) and shared here for the purpose of
community feedback and review -- as well as to expose Lisp's pre-history to a
wider audience!

If you find any issues or have questions, concerns, etc., about this preface
material, you may share these via the
[ticket](https://github.com/lfe/sicp/issues/6) which has been dedicated to
tracking feedback.

## The Hidden Origins of Lisp: Alonzo Church

Alonzo Church was born in Washington, D.C. in 1903.[^1] His great-grandfather (originally from Vermont) was not only a professor of mathematics and astronomy at the University of Georgia, but later became its president.[^2] Church graduated from a Connecticut prep-school in 1920 and then enrolled in Princeton to study mathematics. He published his first paper as an undergraduate and then continued at Princeton, earning his Ph.D. in just three years.

While a graduate student, Church was hit by a trolley car and spent time in a hospital where he met Julia Kuczinski[^3] -- they were married a year later and remained inseparable until her death, 51 years later. Church had a reputation for being a bit quirky: he never drove a car or typed; he was extremely neat and fastidious; he walked everywhere and often hummed to himself while he did so; he loved reading science fiction magazines;[^4] a nightowl, he often did his best work late at night. Though he had solitary work habits, his list of Ph.D. students is impressive, including the likes of Turing, Kleene, and Rosser.

Perhaps one of Church's more defining characteristics was his drive: he deliberately focused on prominent problems in mathematics and attacked them with great force of will. A few of the problems he had focused on in the early 1930s were:

1. Known paradoxes entailed by Bertrand Russell's theory of types [^5]
1. David Hilbert's *Entschiedungsproblem*, and
1. The implications of Gödel's completeness theorem.

These were some of the most compelling challenges in mathematics at that time. All of them ended up meeting at the cross-roads of the λ&#8209;calculus.

Church had started working on the λ&#8209;calculus when attempting to address the Russell Paradox [^6]. However, it was not that goal toward which the λ&#8209;calculus was ultimately applied. Instead, it became useful -- essential, even -- in his efforts to define what he called "calculability" and what is now more commonly referred to as *computability*.[^7] In this the λ&#8209;calculus was an unparalleled success, allowing Church to solve the *Entschiedungsproblem* using the concept of recursive functions.

Syntactically, Church's λ&#8209;-notation made a significant improvement upon that found in the *Principia Mathematica* [^8]. Given the *Principia* phrase $\phi x̂$ and the λ&#8209;calculus equivalent, $\lambda x \phi x$, one benefits from the use of the latter by virtue of the fact that it unambiguously states that the variable $x$ is bound by the term-forming operator $\lambda$. This innovation was necessary for Church's work and was a powerful tool put to use by John McCarthy when he built the first programming language which used the λ&#8209;calculus: Lisp.

---

[^1]: The majority of the material for this section has been adapted from the [Introduction](http://www.math.ucla.edu/~hbe/church.pdf) to the Collected Works of Alonzo Church, MIT Press (not yet published).

[^2]: This was when the University of Georgia was still called Franklin College.

[^3]: She was there in training to become a nurse.

[^4]: He would also write letters to the editors when the science fiction writers got their science wrong.

[^5]: These complications were known and discussed by Russell himself at the time of *Principia*'s publication.

[^6]: See [Russell's paradox](http://en.wikipedia.org/wiki/Russell%27s_paradox).

[^7]: "Computability" was the term which Turing used.

[^8]: See the discussion of "Propositional Functions" in the section "The Notation in Principia Mathematica":  http://plato.stanford.edu/entries/pm-notation/#4. Note that the section of the *Principia Mathematica* which they reference in that linked discussion on the Stanford site is at the beginning of "Section B: Theory of Apparent Variables" in the *Principia*.
