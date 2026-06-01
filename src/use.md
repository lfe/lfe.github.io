---
layout: page.liquid
title: Use
permalink: "/use/"
data:
  long_description: Reference materials, tooling, and practical resources for working LFE developers.
  long_title: Using LFE
---


## Development environment

```bash
# In a project directory (Erlang 21+ and rebar3 required):
$ rebar3 lfe repl

# From the LFE source repo:
$ make && ./bin/lfe

# No install needed:
$ docker run -it lfex/lfe
```

Once in the REPL, `(help)` prints available commands and double-tap
`<TAB>` after a module prefix (e.g., `(lists:`) lists its exported
functions.

* [Development Setup](https://cnbbooks.github.io/lfe-manual/part1/intro/setup.html) — installing Erlang + rebar3 + LFE
* [The LFE REPL](https://cnbbooks.github.io/lfe-manual/part1/repl/) — features, readline, job control, file evaluation
* [Creating LFE Projects](https://cnbbooks.github.io/lfe-manual/part3/projects/) — project layout and rebar3 conventions
* [Emacs lfe-mode](https://github.com/lfe/lfe/tree/develop/emacs) — syntax highlighting and REPL integration, bundled with LFE

<div class="content-card">

### Language reference

* [The LFE Machine Manual](https://cnbbooks.github.io/lfe-manual/) — the comprehensive reference (in progress)
* [The LFE Guide](https://github.com/lfe/lfe/blob/develop/doc/src/lfe_guide.7.md) — core language guide (man page source)
* [Data types](https://github.com/lfe/lfe/blob/develop/doc/src/lfe_types.7.md) — the LFE type system
* [REPL reference](https://github.com/lfe/lfe/blob/develop/doc/src/lfe.1.md) — functions, variables, environment
* [LFE formatting](https://github.com/lfe/lfe/blob/develop/doc/src/lfe_io.3.md) — I/O and format strings
* [Common Lisp compatibility](https://github.com/lfe/lfe/blob/develop/doc/src/lfe_cl.3.md) — the `cl` module
* [Clojure compatibility](https://github.com/lfe/lfe/blob/develop/doc/src/lfe_clj.3.md) — the `clj` module
* [Style Guide](https://cnbbooks.github.io/lfe-manual/part7/style-guide/) — naming, formatting, data representation conventions
* [All LFE books](/books) — the complete collection

</div>

## OTP reference

The modules you'll reach for every day.

```lisp
;; Calling into OTP is just calling Erlang modules:
lfe> (application:which_applications)
((lfe "Lisp Flavoured Erlang" "2.1.4")
 (compiler "ERTS  CXC 138 10" "8.4.1")
 (kernel "ERTS  CXC 138 10" "9.2.1")
 (stdlib "ERTS  CXC 138 10" "5.2.1"))
```

* [ERTS Reference Manual](http://erlang.org/doc/apps/erts/index.html) — the runtime system
* Key modules: [erlang](http://erlang.org/doc/man/erlang.html), [application](http://erlang.org/doc/man/application.html), [supervisor](http://erlang.org/doc/man/supervisor.html)
* Behaviours: [gen_server](http://erlang.org/doc/man/gen_server.html), [gen_event](http://erlang.org/doc/man/gen_event.html), [gen_statem](http://erlang.org/doc/man/gen_statem.html)
* Data: [lists](http://erlang.org/doc/man/lists.html), [maps](http://erlang.org/doc/man/maps.html), [proplists](http://erlang.org/doc/man/proplists.html)
* [The LFE Machine Manual — Part V: OTP](https://cnbbooks.github.io/lfe-manual/part5/) — OTP in LFE

## Ecosystem & interop

LFE calls Erlang modules directly — no wrappers, no FFI, no overhead.

```lisp
;; Any Erlang library is an LFE library:
lfe> (lists:reverse
       (erlang:integer_to_list
         (lists:foldl #'*/2 1 '(1 2 3 4))))
"42"
```

* [Calling Erlang from LFE](https://cnbbooks.github.io/lfe-manual/part8/erl-int/call-erl.html) — the interop model
* [Ports and Port Drivers](https://erlang.org/doc/reference_manual/ports.html) — interfacing with external programs
* [Jinterface](http://erlang.org/doc/apps/jinterface/jinterface_users_guide.html) — Java interop
* [rebar3_lfe plugin](https://github.com/lfe-rebar3/rebar3_lfe) — the LFE compiler plugin for rebar3
* [Community libraries](/community#repos-code-container-images) — the LFE ecosystem on GitHub

<div class="content-card">

### Deployment

* [Releases](https://cnbbooks.github.io/lfe-manual/part5/rels/) — building OTP releases
* [LFE Docker images](https://hub.docker.com/u/lfex/) — a large collection of ready-to-use images
* [Adopting Erlang](https://adoptingerlang.org/) — taking BEAM applications into production
* [Stuff Goes Bad — Erlang in Anger](https://erlang-in-anger.com/) — debugging production systems
* [rebar3 — Getting Started](https://www.rebar3.org/docs/getting-started), [Configuration](https://www.rebar3.org/docs/configuration), [Using Plugins](https://rebar3.org/docs/configuration/plugins/), [Writing Plugins](https://rebar3.org/docs/tutorials/building_plugins/)

</div>
