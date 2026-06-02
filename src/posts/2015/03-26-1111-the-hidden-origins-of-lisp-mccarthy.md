---
layout: post.liquid
title: "The Hidden Origins of Lisp: John McCarthy"
description: ""
permalink: "/blog/excerpts/2015/03/26/1111-the-hidden-origins-of-lisp-mccarthy"
categories: ["excerpts"]
tags: ["books", "documentation", "education", "history", "lisp", "mathematics", "sicp"]
published_date: 2015-03-26 11:11:00 +0000
is_draft: false
data:
  author: duncan-mcgreggor
  written_for: null
  last_validated: null
  cover_image: "/images/default/LFE_00255_.png"
  cover_alt: "Vigdís — LFE, retro-futurist digital painting"
  math: false
---
<a href="/blog/assets/images/posts/John-McCarthy.jpg"><img class="right medium" src="/blog/assets/images/posts/John-McCarthy.jpg" /></a> John McCarthy: Creator of the prefix-notation programming language Lisp and founder of AI.

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

## The Hidden Origins of Lisp: John McCarthy

John McCarthy was born in 1927, in the city of Boston. Due to difficulties
finding work during the Great Depression, the family moved to New York, then
finally settled in Los Angeles. Having established an early aptitude and
proficiency in mathematics, McCarthy skipped two years of math upon his
enrollment at Caltech. The year he received his undergraduate degree, he
attended the 1948 Hixon Symposium on Cerebral Mechanisms in Behavior. The
speakers at the symposium represented an intersection of mathematics,
psychology, and the nascent field of computing science. [^1]

At the symposium John von Neumann presented his paper "The General and Logical
Theory of Automata", [^2] where he covered the following topics:

1. Preliminary Considerations
2. Discussion of Certain Relevant Traits of Computing Machines
3. Comparisons Between Computing Machines And Living Organism
4. The Future Logical Theory of Automata
5. Principles of Digitalization
6. Formal Neural Networks
7. The Concept of Complication and Self-Reproduction

von Neumann's erudite tour de force stunned audience members as well as fellow
presenters. [^3] McCarthy was captivated. Now intrigued with the idea of
developing machines that could think as people do, he was set upon the
path which he would follow for the rest of his life.

McCarthy remained at Caltech for one year of grad school, but then pursed the
remainder of his Ph.D. at Princeton, considered by him to be the greater
institution for the study of mathematics. In a discussion with an enthusiastic
von Neumann during a visit to Princeton Institute for Advanced Study, McCarthy
shared his ideas about interacting finite automata -- ideas inspired by von
Neumann's talk at the Hixon Symposium. Though encouraged by von Neumann to
write these ideas up in a paper, McCarthy never did. However, these thoughts
continued to evolve over the course of the next 10 years and found their way --
in modified form -- to McCarthy's early AI papers and even Lisp itself.

After completing his Ph.D. dissertation, Claude Shannon invited McCarthy and
his friend Marvin Minsky to work at Bell Labs in New Jersey for the summer.
McCarthy and Shannon collaborated on assembling a volume of papers entitled
"Automata Studies," thought ultimately a bit of a disappointment to McCarthy
since so few submissions concerned the topic of his primary interest: machine
intelligence. A few years later, he had the opportunity to address this by
proposing a summer research project which he and the head of IBM's Information
Research pitched to Shannon and Minsky. They agreed, and a year later held the
first Artificial Intelligence workshop at the Dartmouth campus in New
Hampshire.

It was here, thanks to Allen Newell and Herb Simon, that McCarthy was exposed
to the idea of list processing for a "logical language" Newell and Simon were
working on (later named IPL). McCarthy initially had high hopes for this effort
but upon seeing that its implementation borrowed heavily from assembly, he gave
up on it. That, in conjunction with his inability to gain any traction with the
maintainers of FORTRAN for the support of recursion or conditionals, finally
pushed him to create a language that suited his goals of exploring machine
intelligence.

With the seeds of Lisp sown in 1956 at the workshop, it was two more years
before development of the programming language began in earnest. In 1958 a
special project was established to carry out Lisp development and AI work under
the auspices of the MIT Research Laboratory of Electronics -- which granted
McCarthy and his team one room, one secretary, two programmers, a key punch and
six grad students.[^4] The MIT AI project was founded and the work of creating
Lisp had begun.

By the end of the year, the group had written -- on paper -- subroutines for
reading and printing list structures as well as those to provide a Lisp
environment. Over the course of a few months, these were then hand-compiled to
SAP (*SHARE assembly program*), and eventually checked by running the SAP code
on the IBM 704 which had been made available to MIT. [^5]<sup>,</sup>
[^6] Within a few years, Lisp had it's first interpreter
(written in Lisp), and not too long after that saw the release of version 1.5
of the language.

At the 1980 Lisp conference held at Stanford, John McCarthy humorously
commented on the previous two decades of Lisp's survival: [^7]
<blockquote>
"On LISP's approximate 21st anniversary, no doubt something could be said about
coming of age, but it seems doubtful that the normal life expectancy of a
programming language is three score and ten. In fact, LISP seems to be the
second oldest surviving programming language after Fortran, so maybe we should
plan on holding one of these newspaper interviews in which grandpa is asked to
what he attributes having lived to 100."
</blockquote>

Lisp's 50th anniversary was celebrated in 2008, and despite the AI Winter of
the 1990s, it shows no signs of disuse or senescence. Quite to the contrary, it
continues to have a profound impact on multiple generations of computer
programmers meeting all number and variation of needs.

----

[^1]: The presenters at the *Hixon Symposium on Cerebral Mechanisms in Behavior*, per the notes in the Caltech publication *Engineering and Science*, [Volume 12:1, October 1948](http://resolver.caltech.edu/CaltechES:12.1.Pump2), were as follows:
      <ul><li>Professor Ward C. Halstead, University of Chicago</li>
      <li>Professor Heinrich Kluver, University of Chicago</li>
      <li>Professor Wolfgang Kohler , Swarthmore College</li>
      <li>Professor K. S. Lashley, Harvard University</li>
      <li>Dr. R. Lorente de No, Rockefeller Institute for Medical Research</li>
      <li>Professor Warren S. Mc Culloch, University of Illinois</li>
      <li>Dr. John von Neumann, Institute for Advanced Study</li>

[^2]: A transcript of "The General and Logical Theory of Automata" is available in Volume V of John von Neumann "Collected Works". The talk concluded with an intensive period of question and answer, recorded at the end of the article.

[^3]: At the beginning of the Q&A session for von Neumann's talk at the Hixon Symposium, one Dr. Gerard comments "I have had the privilege of hearing Dr. von Neumann speak on various occasions, and I always find myself in the delightful but difficult role of hanging on to the tail of a kite. While I can follow him, I can't do much creative thinking as we go along."

[^4]: Marvin Minsky and John McCarthy founded the MIT AI Lab together when McCarthy caught the acting head of the department, Jerome Weisner, in the hallway and asked him permission to do it. Weisner responded with "Well, what do you need?”. When McCarthy gave him the list, he asked "How about 6 graduate students?" as the department had agreed to support six mathematics students, but had yet to find work for them. See [On John McCarthy’s 80th Birthday, in Honor of his Contributions](http://www-formal.stanford.edu/leora/hayes_morgenstern_birthday_mccarthy.pdf), page 3.

[^5]: See the section "The implementation of LISP" in McCarthy's 1978 paper *History of Lisp*.

[^6]: See *Lisp Bulletin #3*, December 1979 in the article "Lisp History" by Herbert Stoyan, page 45. Stoyan's article aimed to fill in the missing details of McCarthy's earlier paper -- of the same name -- which McCarthy had presented at the SIGPLAN conference in June of 1979.

[^7]: See [Lisp: Notes on Its Past and Future](http://www-formal.stanford.edu/jmc/lisp20th/lisp20th.html) in the "Introduction."
