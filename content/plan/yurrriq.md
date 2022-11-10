+++
title = "yurrriq's .plan"
in_search_index = true
template = "plan/page.html"

[extra]
long_title = "LFE .plan"
long_description = "yurrriq"
+++

[![dot-plan-screenshot][screenie-src]][screenie-link]

### Blogging

* Re: [simple_cache][] from [Erlang and OTP in Action][] (ported to LFE)
* Re: Property-based testing with [PropEr][] and [PropL][]
  * How it helped with [Lodox][]

### Community

* Add more exercises to [xLFE][]
  * Get more contributors
  * Spread the word somehow
  * Get more people [on track][xLFE stats]
* Once [Lodox][] gets (more) stable, generate documentation for projects like:
  * [exemplar][]
  * [clj][] (as part of LFE proper)
  * [ltest][]
* Help ensure common lfex projects are using rebar3 and modern .travis.yml
* Modernize the Clojure code in [lfecljapp][]
* Help [oubiwann](/plan/oubiwann) et al. write/edit documentation as needed

### Tools

* Some [Clojure mappings][clj] like [`condp`][]
* [Lodox][]: Documentation generator like [Codox][] for LFE
* [PropL][]: LFE wrapper for [PropEr][]
  * Consider adding support in [ltest][]
  * Make shell "property-tests" "behaviour as in ltest
* Further improvements to [Emacs integration][]
* [lfedn][]: lfe <-> edn
  * Polish and maybe use it in [lfecljapp][]
* Clean up [pynchon][]
  * Document it
  * Come up with some practical examples

[//]: ---Named-Links---

[clj]: https://github.com/lfex/clj/issues
[`condp`]: https://github.com/lfex/clj/pull/19
[PropEr]: http://proper.softlab.ntua.gr
[PropL]: https://github.com/quasiquoting/propl
[Lodox]: https://github.com/quasiquoting/lodox
[Codox]: https://github.com/weavejester/codox
[simple_cache]: https://github.com/yurrriq/simple_cache
[Erlang and OTP in Action]: https://www.manning.com/books/erlang-and-otp-in-action
[Emacs integration]: https://github.com/rvirding/lfe/tree/develop/emacs
[xLFE]: http://exercism.io/languages/lfe
[xLFE stats]: http://exercism.io/stats/lfe
[exemplar]: https://github.com/lfex/exemplar
[ltest]: https://github.com/lfex/ltest
[lfecljapp]: https://github.com/lfex/lfecljapp
[lfedn]: https://github.com/quasiquoting/lfedn
[pynchon]: https://github.com/quasiquoting/pynchon
[screenie-src]: /images/lfe-dotplan-screen.png
[screenie-link]: https://tools.ietf.org/rfc/rfc742.txt
