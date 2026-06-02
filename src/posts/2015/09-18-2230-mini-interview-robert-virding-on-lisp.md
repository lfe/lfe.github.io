---
layout: post.liquid
title: "An Interview with Robert Virding on Lisp"
description: ""
permalink: "/blog/interview/2015/09/18/2230-mini-interview-robert-virding-on-lisp"
categories: ["interview"]
tags: ["history", "lisp", "erlang", "lambda machine", "symbolics", "vms", "flavors", "oop", "parlog"]
published_date: 2015-09-18 22:30:00 +0000
is_draft: false
data:
  author: duncan-mcgreggor
  written_for: null
  last_validated: null
  cover_image: "/images/default/LFE_00241_.png"
  cover_alt: "Vigdís — LFE, retro-futurist digital painting"
  math: false
---
<a href="/blog/assets/images/posts/Robert-Train.png"><img class="left medium" src="/blog/assets/images/posts/Robert-Train.png" /></a>If
you've been watching the
[LFE mail list closely](http://groups.google.com/group/lisp-flavoured-erlang),
then you will have noticed that we're working on LFE's first book. That's
super-exciting, but for a future post or two all of their own. Several months
ago I was getting chapter 1 ready for the reviewers, and I needed some
clarification on a few points from Robert Virding regarding some of LFE's
historical details.

So I did what was natural: asked a bunch of completely unrelated questions!
Well, to be honest, they were related tangentially and they helped put together
a more complete picture of LFE's early history. It ended up being a bit of a
mini-interview, and I enjoyed Robert's responses so much, I asked him if it was
okay to share with the wider LFE community -- he agreed and the plan was
back-burnered for when I had a chance to create a post with the questions and
answers.

Robert and I had a few more related conversations around the same time -- I've
taken a bit of artistic liberty in weaving the additional email conversations
into a single narrative.

I hope you enjoy this as much as I did :-)

<hr />

Duncan: **When did you first encounter Lisp? Where were you working
and/or going to school?**

**Robert**:
I first encountered Lisp somewhere around 1980-81. I was in the Physics
department at Stockholm University and had started taking my PhD in theoretical
physics there.

Duncan: **What was your initial reaction to Lisp?**

**Robert**:
They were using it at the department for algebraic calculations in theoretical
physics which is why I came into contact with it. I was (not yet) a part of
that work so, while it was cool, I didn't really see the beauty of it right
away.  At that time I was doing programming of more "normal" types of
applications, tools for student labs and micro-processor programming in labs,
using more traditional languages like Pascal and various assemblers.

Duncan: **What where the primary hurdles you had to overcome when initially
learning Lisp?**

**Robert**:
My major difficulty was in grasping that the parentheses defined a data
structure and were not just for grouping as in other languages. Why wasn't
``(foo (1) (2) (3))`` the same as ``(foo 1 2 3)``. Once I got that, it was
easy. The prefix notation was no problem as I had done some work with Forth
(and had implemented one), so I was used to not having infix operators.

Duncan: **What projects did you work on where Lisp was a major aspect of the
work?**

**Robert**:
I didn't really use Lisp deeply at the Physics department. They had a VAX 780
running VMS and had
[Eunice](https://en.wikipedia.org/wiki/Eunice_%28software%29), so I tested
[Franz Lisp](https://en.wikipedia.org/wiki/Franz_Lisp).  When I left and joined
Ericsson they were also using Vax/VMS so I borrowed Eunice to continue with
Lisp. I thought it would be interesting to see if I could port Franz Lisp to
straight-VMS. It was much more UNIX/BSD dependent than I originally thought,
which resulted in me having to implement more of BSD on top of VMS than I had
planned. Maybe I can share those details with you later, they are not really
interesting here, for this Lisp discussion.  As far as I know no-one except me
ever used it, but it did wonders for my C and understanding of how OSes work!

I did more work with Lisp when I was a member of the
[Computer Science Lab](http://web.archive.org/web/20000817053822/http://www.ericsson.se/cslab/admin/jubileumsskrift.html)
in Ericsson where we were looking at various ways of implementing telecom
applications. Lisp was one the languages we were investigating, and as part of
that work I studied implementing Lisps. I also implemented the Lisp Machine
Flavors system as a study of implementing a relatively complex system in and on
Lisp ... and for the fun of it as well.

We later moved on to look at logic languages and these became the base on which
Erlang was built, even though Erlang itself became functional.

Duncan: **Can you tell me more about the Flavors project?**

**Robert**:
Well, it's probably not well-known that the name LFE -- Lisp Flavoured Erlang
-- was partly inspired by the work I did on porting Flavors to VMS, with the
spelling changed to the English "Flavours" as a joke.

Duncan: **Were you using an LMI Lambda? If it wasn't a
Symbolics machine, then it probably wasn't ZetaLisp ... so were you using the
Maclisp-derived Lisp for the Lambda?**

**Robert**:
No, it was other way around. I was using PSL
([Portable Standard Lisp](https://en.wikipedia.org/wiki/Portable_Standard_Lisp))
from Utah which was quite a good Lisp implementation running on UNIX, amongst
other systems. I had heard about Flavors and thought it would be interesting,
and fun, to see if I could implement it on PSL. I managed to get hold of the
Lisp Machine Flavors documentation and got going.

Flavors was an extremely feature-filled system; someone had really gone to town
with it. It had multiple inheritance, mix-in flavors, before and after
handlers, and wrapper macros. It also had a set of rules describing which were
to be called and when. It was wonderful. Fortunately, Flavors had a few
features which made it easier. For example, there was an explicit ``send``
function so I didn't need to hack PSL itself.

All in all, it worked. But I never really had a proper use-case. Unfortunately,
I think the code has disappeared -- at least, I haven't been able to find it.
It would almost be doable on LFE except for updating object state; you would
have to modify it to return the new object instead. Or use processes, but then
they may be a little heavy.

Duncan: **After gaining experience in school and work with Lisp, what features
did you feel made Lisp an excellent tool or even just fun to work with?**

**Robert**:
I liked working with functional languages and I found Lisp to be very versatile
when building systems. The combination of functions and macros -- and the
homoiconicity which makes working with macros easy -- makes Lisp a very
powerful tool. This makes Lisp and the concurrency from Erlang a very good
combination.  One wonders how it would have gone if we had used Lisp as a base
for Erlang.

Duncan: **In 2007, when you started working on what would become LFE, when was
the last time you had implemented a language? Was it in 1986 when you had
implemented the variants of the parallel logic programming languages?**

**Robert**:
In addition to the work on the parallel programming languages, I did a number
of implementations of Erlang to study different techniques as well as to study
the possibilities of using different memory management and garbage collection
mechanisms like reference-counting and a real-time copying collector. These
presented no significant benefit over the separate-process-heap model used in
the "standard" Erlang implementations, so the work was discontinued.

I did some work with the BEAM implementation, amongst other things, the
compiler, and also parts of the basic system.

Duncan:
**Coming back to the parallel programming, I saw a reference to the 1986 paper
you co-authored, entitled
*The Phoning Philosopher's Problem or Logic Programming for Telecommunications Applications*,
where [Parlog](https://en.wikipedia.org/wiki/Parlog) was used to create what
looked like an Erlang prototype. Was this the first version of Erlang?**

**Robert**:
No, the Erlang language did not exist in 1986; using the name "Erlang"
pre-dates the current Erlang language. The 1986 paper was describing a
language/system based on concurrent logic which we called "Erlang". It
was mainly done by a guy called Nabiel Elshiewy and myself. Actually this path
was abandoned and has nothing to do modern Erlang. Which I think is a great
shame, as I liked the concurrent logic languages. I still do.

Work on what became Erlang didn't really start until '87/'88. Before that
almost all the members of the lab had written telecom applications in a variety
of languages and systems -- basically everything that ran on BSD or UNIX -- to
experiment with ways of solving the problem.
