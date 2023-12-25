+++
title = "Use"
in_search_index = true

[extra]
long_title = "Using LFE"
long_description = "LFE may be used as a shell, for scripting; as a development tool via its REPL; as a library; and as a full programming language in its own right, with a compiler that generates `.beam` files for use by the Erlang VM. This page aims to bring to your fingertips the resources you will need most in your daily use of LFE in one or more of these."

+++

## The REPL

As you get to know LFE, you'll find that you'll do a lot of your prototyping in the REPL. There are three primary ways you can do this:

1. With Erlang and `rebar3` installed, running `rebar3 lfe repl` in a project directory will start up the LFE REPL and provide access to all your project modules, include files, and dependencies.
1. If you `git clone`d the LFE repo to a machine, then running `make` and then `./bin/lfe` will start up the LFE REPL.
1. Lastly, you can run the LFE REPL with only docker on your machine via `docker run -it lfex/lfe` (no Erlang, no `rebar3` necessary); for many people, this is the quickest, most pain-free way of trying out LFE.

In all of the above, once you are in the REPL, you have access to the `(help)` function which will print out some help text for running a bunch of top-level commands. Also, you may start typing a function call (e.g., `(lists:`) and then double-tap `<TAB>` to provide a list of available functions from the given module. Very often this obviates any need to look up the function you want to call!

## Reference

When `(help)` and tab-completion aren't enough, it's time to break out the books. The core LFE reference set (man page source files in Markdown format) is your friend:

* [The LFE Guide](https://github.com/lfe/lfe/blob/develop/doc/src/lfe_guide.7.md)
* [The data types in LFE](https://github.com/lfe/lfe/blob/develop/doc/src/lfe_types.7.md)
* [REPL functions, variables, and environment](https://github.com/lfe/lfe/blob/develop/doc/src/lfe.1.md)
* [LFE formatting](https://github.com/lfe/lfe/blob/develop/doc/src/lfe_io.3.md)
* [Common Lisp compatibility](https://github.com/lfe/lfe/blob/develop/doc/src/lfe_cl.3.md)
* [Clojure compatibility](https://github.com/lfe/lfe/blob/develop/doc/src/lfe_clj.3.md)

Additionally, we have a large manual that is a work in progress and aims to provide a great deal of examples, context, and general instruction for LFE developers. That is available here:

* [The LFE Machine Manual](https://lfe.io/books/chinenual)

The complete collection of published LFE books is avalable at [/books](https://lfe.io/books).

## docs.lfe.io

The docs site for LFE is QUITE OLD and almost entirely OUT OF DATE, but it's linked here since it still has some jewels that haven't been fully migrated into the LFE Machine Manual:

* [docs.lfe.io](https://docs.lfe.io)

There is a slightly newer version of the docs site whose development has been stalled. Though not currently of much use, it's available here:

* [docs.lfe.io/dev/](https://docs.lfe.io/dev/index.html)


Once all the usful content has been merged into the LFE Machine Manual, docs.lfe.io will redirect to this page and we'll delete this section ;-)

## Erlang

[Erlang Run-Time System Application (ERTS) Reference Manual](http://erlang.org/doc/apps/erts/index.html) - As an LFE programmer, this is one of the most powerful reference resources you can have at your fingertips. Once you get to the point to where you are ready to build production-ready applications, this will be a constant companion. In particular:

* The [erlang](http://erlang.org/doc/man/erlang.html) module
* The [application](http://erlang.org/doc/man/application.html) module
* The [supervisor](http://erlang.org/doc/man/supervisor.html) module
* The [gen_server](http://erlang.org/doc/man/gen_server.html), [gen_event](http://erlang.org/doc/man/gen_event.html), and [gen_statem](http://erlang.org/doc/man/gen_statem.html) modules
* The [lists](http://erlang.org/doc/man/lists.html), [maps](http://erlang.org/doc/man/maps.html), and [proplists](http://erlang.org/doc/man/proplists.html) modules
* And many, many others ...

If you will be interfacing with other languages, then these resources will be of particular use:

* The [Ports and Port Drivers](https://erlang.org/doc/reference_manual/ports.html) reference and the [Erlang Interop/Ports User Guide](http://erlang.org/doc/tutorial/c_port.html)
* The [Jinterface package](http://erlang.org/doc/apps/jinterface/jinterface_users_guide.html) and the [Jinterface Reference Manual](http://erlang.org/doc/apps/jinterface/index.html)

## rebar3

One of the things you might find yourself needing is specialised `rebar3` plugins for your various LFE projects. You may write these in either Erlang or LFE (or, in fact, any BEAM language that has a `rebar3` compiler). There are a ton of good resources on the rebar3 project site, including:

* [Getting Started](https://www.rebar3.org/docs/getting-started)
* [Configuration](https://www.rebar3.org/docs/configuration)
* [Using Plugins](https://rebar3.org/docs/configuration/plugins/)
* [Writing Plugins](https://rebar3.org/docs/tutorials/building_plugins/)

The source for the LFE `rebar3` plugin is written in Erlang and viewable here:

* [https://github.com/lfe-rebar3/rebar3_lfe](https://github.com/lfe-rebar3/rebar3_lfe)

If you want to see examples of `rebar3` plugins written in LFE itself, be sure to checkout some of the other projects in that Github org:

* [https://github.com/lfe-rebar3](https://github.com/lfe-rebar3)
