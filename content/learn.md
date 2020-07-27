+++
title = "Learn"
in_search_index = true

[extra]
long_title = "Resources for Learning LFE"
long_description = "Learning LFE must be taken in two tracks: learning the syntax particular to a Lisp on the Erlang VM, with all its support for pattern matching, Erlang-style arities, etc., and then learning the ins-and-outs of BEAM languages and OTP. This two-pronged approach is the path to success."

+++

# [LFE](#lfe)

## [Quick Start Guides](#quick-start-guides)

[Quick Start with rebar3](https://lfe.io/books/rebar3-quick-start/) - This will get you up and running with LFE, requiring _only_ that you have a modern Erlang installed (version 19+) and `rebar3`.

## [In-Depth](#in-depth)

[The LFE Tutorial](https://lfe.io/books/tutorial/) - The Erlang "Getting Started" translated into LFE!

[Casting SPELs in LFE](https://lfe.io/books/casting-spels/) - The famous "Casting SPELs in Lisp" translated into LFE, but taking things further than the original with state management via `defrecord`, creating a custom game server, and then dipping the toes into OTP with a conversion of the custom game server to a `gen_server`.

[LFE Examples](https://github.com/rvirding/lfe/tree/develop/examples) - For those that learn by watching and playing, the `./examples` directory in the LFE repo may be quite useful.

LFE on [Rosetta Code](https://rosettacode.org/wiki/Category:LFE) - If those examples aren't enough for you, there are 99 others to choose from on Rosetta Code!

## [More LFE](#more-lfe)

Once you've learned the syntax and the underlying principles of the Erlang VM, you're going to want to actually _use_ this beautiful Lisp! Be sure to check out our [reference materials, how-tows, etc.](/use).

[The community](/community) will be an invaluable resource in your journing of learning, to be sure to stop in whatever medium makes you happy, say "hi" and ask us lots of questions!

# [Erlang](#erlang)

There are some phenomenal materials available for a self-paced Erlang/OTP education. Some of the classics are given below. For those with bigger budgets, remember that formal training is also an option! (In fact, there are not only classses offered for Erlang, but also LFE ...)

## [The Language](#the-language)

[Learn You Some Erlang for Great Good!](https://learnyousomeerlang.com/) - Probably one of the best (and _definitely_ the most fun) books available for learning Erlang. (Also avialable [in print](https://nostarch.com/erlang).)

## [OTP](#otp)

[Designing for Scalability with Erlang/OTP](https://www.oreilly.com/library/view/designing-for-scalability/9781449361556/) - The thinking person's OTP book: where to go when you really want to understand OTP.

[Erlang and OTP in Action](https://www.manning.com/books/erlang-and-otp-in-action) - This book is another fantastic resource, building up an OTP application piece at a time as you move through the chapters.

## [In Production](#in-production)
Once you've learned how to write Erlang/LFE/OTP applications, is time to get them into production, and you'll find the following invaluable references for that:

* [Adopting Erlang](https://adoptingerlang.org/) - _"Adopting Erlang is an ongoing effort to gather all the resources that will help you use Erlang in a business. The booksite is divided in three sections focusing particularly on Erlang/OTP’s higher level concepts in the current open source ecosystem, how to use it in production (while setting up a pipeline for continuous development and delivery), and how to build a team when you’re starting from scratch."_
* [Stuff Goes Bad - Erlang in Anger](https://erlang-in-anger.com/) - _"This book intends to be a little guide about how to be the Erlang medic in a time of war. It is first and foremost a collection of tips and tricks to help understand where failures come from, and a dictionary of different code snippets and practices that helped developers debug production systems that were built in Erlang."_
