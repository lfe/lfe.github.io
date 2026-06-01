---
layout: post.liquid
title: "The Hidden Origins of Lisp: Giuseppe Peano"
description: ""
permalink: "/blog/excerpts/2015/03/23/1008-the-hidden-origins-of-lisp-peano"
categories: ["excerpts"]
tags: ["books", "documentation", "education", "history", "lisp", "mathematics", "sicp"]
published_date: 2015-03-23 10:08:00 +0000
is_draft: false
data:
  author: duncan-mcgreggor
  written_for: null
  last_validated: null
  cover_image: null
  cover_alt: null
  math: false
---
<a href="/blog/assets/images/posts/Giuseppe-Peano.jpg"><img class="right medium" src="/blog/assets/images/posts/Giuseppe-Peano.jpg" /></a>
Giuseppe Peano: Creator of the first recursive function definition.

This is one of a series of posts tracing the origins of Lisp through four brief
biographical vignettes of individuals whose contributions to mathematics
ultimately supported the creation of Lisp:

 * [Introduction](/excerpts/2015/03/22/1445-the-hidden-origins-of-lisp-introduction/)
 * [Giuseppe Peano](/excerpts/2015/03/23/1008-the-hidden-origins-of-lisp-peano/)
 * [Bertrand Russell](/excerpts/2015/03/24/0111-the-hidden-origins-of-lisp-russell/) 
 * [Alonzo Church](/excerpts/2015/03/25/1108-the-hidden-origins-of-lisp-church/)
 * [John McCarthy](/excerpts/2015/03/26/1111-the-hidden-origins-of-lisp-mccarthy/)
 * [The Place of Lisp in the 21st Century](/excerpts/2015/03/27/1101-the-hidden-origins-of-lisp-future)

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

## The Hidden Origins of Lisp: Giuseppe Peano

Giuseppe Peano was born 100 years before Lisp, in August of 1858 at his family's farmhouse in the north of Italy. At a young age he was recognized as having an exceptionally quick mind and, through the favour of an uncle, obtained a good early education in Turin where he not only stayed for university, but for the entirety of his career.

After graduating from the University of Turin with high honors, Peano was asked to stay on, providing assistance with the teaching responsibilities of the mathematics department. Within a few years, he began tackling problems in logic and exploring the foundations of the formal philosophy of mathematics. During this time, Peano introduced the world to his now-famous axioms.[^1]<sup>,</sup> [^2] In particular, the fifth axiom is considered the first definition of primitive recursive functions.[^3] In this same work Peano described the function of a variable with explicit recursive substitution.[^4] Both of these served as a great source of inspiration and insight to later generations.

From this point into the beginning of the 20th century, Peano was considered one of the leading figures in mathematical logic, alongside Frege and Russell. This was due to Peano's work on and advocacy for a unified formulation of mathematics cast in logic. Entitled *Formulario Mathematico*, it was first published in 1895, with multiple editions released between then and the last edition in 1908. Each subsequent edition was essentially a new work in its own right, with more finely honed formulas, presentation, and explanation wherein he also shared his symbols for logic, a new mathematical syntax.

In 1897 at the First International Congress of Mathematicians in Zurich, Peano co-chaired the track on "Arithmetic and Algebra" and was invited to deliver a keynote on logic. Between that event and its successor in 1900, he published more of his work on the *Formulario*. By these and other activities, when Peano arrived in Paris for the international congresses of both mathematics and philosophy, he was at the peak of his career in general, and the height of his development of mathematical logic in particular. At this event Peano along with Burali-Forti, Padoa, Pieri, Vailati, and Vacca were said to have been "supreme" and to have "absolutely dominated" the discussions in the field of the philosophy of sciences.[^5]

Bertrand Russell was present at the first of these congresses and was so completely taken with the efficacy of Peano's approach to logic that upon receiving from Peano his collected works, he returned home to study them instead of remaining in Paris for the Mathematical Congress. A few months later he wrote to Peano, attaching a manuscript detailing the assessments he had been able to make, thanks to his recent and thorough study of Peano's works. Peano responded to him the following March congratulating Russell on "the facility and precision" with which he managed Peano's logical symbols; Peano published Russell's paper that July. However, this was only the beginning for Russell: the baton had been firmly passed to him and the advance towards a theory of computation had taken its next step.

----

[^1]: This was in Peano's book of 1889 "Arithmetices principia, nova methodo exposita" (in English, *The principles of arithmetic, presented by a new method*).

[^2]: Furthermore, it was in this same period of time that Peano started creating various logic and set notations that are still in use today.

[^3]: See Robert I. Soare's 1995 paper entitled "Computability and Recursion", page 5.

[^4]: See the 2006 paper "History of Lambda-calculus and Combinatory Logic" by Felice Cardone and J. Roger Hindley, page 2.

[^5]: See page 91 of Hubert C. Kennedy's 1980 hardcover edition of "Peano: Life and Works of Giuseppe Peano", Volume 4 of the "Studies in the History of Modern Science."
