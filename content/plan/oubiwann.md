+++
title = "oubiwann's .plan"
in_search_index = true
template = "plan/page.html"

[extra]
long_title = "LFEX .plan"
long_description = "oubiwann"
+++

[![dot-plan-screenshot][screenie-src]][screenie-link]

## LFE

* Verion metadata support
* More command line options and config support
* Headless REPL

## Documentation and Content

* The LFE Chinual
* Rebar3 LFE plugin reference updates
* More howto and OTP blog posts
* Work with Robert to finish LFE tutorial
* Add a new chapter to _Casting SPELs in LFE_ for OTP releases and writing your own syntax
* Continue to move old docs into new, useful home
* Decommission old docs site (lots of redirect work)
* Migrate old blog content to the same content generator used by lfe.io

## Examples

* Pull more of the ~100 examples I wrote on Rosetta Code into the [LFE examples dir][lfe-examples-dir]
* Or the [LFE examples repo][lfe-examples-repo]
* Update the Nova LFE example once routes support for Nova has landed in [lanes][lanes]
* Update the [10,000 things][lao-tzi] demo

## Tooling

* Continue working through `rebar3_lfe` [feature tickets, etc.][rebar3-lfe-tickets]
*

## Games

* Continue hacking on the [LFE MUSH framework][lfe-mush]

## The LFE Machine

* Experiment with buildroot and Nerves for SoC installations of LFE
* Continue explorations of window manager components (a la Lisp Machines Genera); see the [tv project][tv]

## Sound and Music

* Finish up the simple sound [generator example][lfe-examples-repo]
* Get the TCP server working for [undertone][undertone]

## Web

* Update YAWS examples in the lfex Docker files/images
* Get YAWS working with [lanes][lanes] (the web router successor to lfest)
* Land Cowboy support in lanes
* Implement gopher [client and server][goldy]
* Resume work on lmug projects

## Databases

* Finish the [Mnesia tutorial][]
* Add more functions to the Redis library, [ledis][]

## AI

* Pick up the GP tutorials again
* Implement Eliza bot in LFE
* Do more general planner work

# Old .plan File

### Web

* Get URL-parsing into lhc library
* Get LFE OAuth library working
* Create LFE Twitter client
* [LFE + YAWS + WebSockets][] & related blog post
* Continue with [lrootes][], a composable rewrite of [lfest][]

### Books and Docs

* Updates to Getting Started Guides:
  * walk through them with a recent LFE and ensure everything works as
    advertised
  * add information for non-Debian/Mac OS X systems (people have been having
    problems running through the guides on Red Hat-based systems)
* Create install guide(s) for LFE (in particular, Red Hat-based, Windows, *BSD)
* Get more writing done on LFE book
* Finish chapter 2 of [SICP][] (LFE Edition) and start chapter 3
* Continue converting the PyGTK tutorial to an [LFE GTK tutorial][] book for building
  GUI apps
* Finish [LFE Hyperpolyglot][]
  * Also: update layout to use Bootstrap for beter mobile experience
* Continue work on [try.lfe.io][]

### Tools

* Continue writing [rebar3 plugins][]
* Get [ltool][] to feature-parity with lfetool
* Continue migrating the [clj][] library to be a module that's part of LFE-proper

### Distributed and Parallel Systems

* [Lasp][] & LFE Code/Blog Post
* Continue work on [wrapping OTP behaviours with LFE macros][]
* Update the LFE [distributed/parallel python][] project
  * use latest [ErlPort][] code from [@hdima][]
  * update to use rebar3
  * fixes for community member usage
* Continue exploring McCarthy and Gabriels QLISP for para

### GUI

* Finish GTK LFE example
  * update it to use the new [ℓGTK][] library
* Build out [ℓGTK][]
  * continue wrapping GTK functions
  * add ``gen_server`` or ``gen_event`` to help track object state
* Convert nascent Clojure Twitter Linux client ([meep][]) to LFE + [ℓGTK][]

### Databases

* Finish the [Mnesia tutorial][]
* Add more functions to the Redis library, [ledis][]
* Help out with the Riak client [lric][]

### AI

* Pick up GP tutorial again
* Continue exploring porting the award-winning [SHOP2][] Common Lisp planner to LFE
  * get latest code from University of Maryland: <https://github.com/oubiwann/shop2/issues/1>
  * update github SHOP2 repo with UMD's latest
* Continue working through code in [Handbook of Neuroevolution Through Erlang][] and adapting to LFE

[//]: ---Named-Links---

[@hdima]: https://github.com/hdima
[Lasp]: https://github.com/lasp-lang/lasp
[LFE + YAWS + WebSockets]: https://github.com/oubiwann/docker-lfe-yaws-websocket-app
[clj]: https://github.com/lfex/clj/labels/Clojure%20Lib%20for%20LFE
[LFE GTK tutorial]: https://lfe.gitbooks.io/gtk2-tutorial/content/
[SICP]: https://lfe.gitbooks.io/sicp/content/
[LFE Hyperpolyglot]: http://lfex.github.io/hyperpolyglot/
[rebar3 plugins]: https://github.com/lfe-rebar3
[ltool]: https://github.com/lfe-rebar3/ltool
[ℓGTK]: https://github.com/oubiwann/lgtk
[distributed/parallel python]: https://github.com/lfex/py
[ErlPort]: http://erlport.org/
[meep]: https://github.com/oubiwann/meep
[try.lfe.io]: https://github.com/lfex/try.lfe.io
[lrootes]: https://github.com/oubiwann/lrootes
[lfest]: https://github.com/lfex/lfest/
[wrapping OTP behaviours with LFE macros]: https://github.com/oubiwann/behaviour-macros
[SHOP2]: https://www.jair.org/media/1141/live-1141-2152-jair.pdf
[Handbook of Neuroevolution Through Erlang]: http://www.springer.com/us/book/9781461444626
[Mnesia tutorial]: http://docs.lfe.io/tutorials/mnesia/1.html
[ledis]: https://github.com/lfex/ledis
[lric]: https://github.com/lfex/lric
[screenie-src]: /images/lfe-dotplan-screen.png
[screenie-link]: https://tools.ietf.org/rfc/rfc742.txt
[tv]: https://github.com/lfe-machine/tv
[lfe-examples-dir]: https://github.com/lfe/lfe/tree/develop/examples
[lfe-examples-repo]: https://github.com/lfe/examples/
[rebar3-lfe-tickets]: https://github.com/lfe-rebar3/rebar3_lfe/issues
[lanes]: https://github.com/lfex/lanes
[undertone]: https://github.com/lfex/undertone
[lfe-mush]: https://github.com/hexagram30/mush/issues
[goldy]: https://github.com/oubiwann/goldy
[lao-tzi]: https://github.com/oubiwann/laotzi-demo
