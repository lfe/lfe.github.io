---
layout: home
---

## Nothing Quite Compares...

...To the taste of Erlang, aged in the oaken barrels of Lisp, served at a
temperature of perfect hotness. New to LFE? Check out our
<a href="/quick-start/1.html">Quick Start</a> guide!

LFE --
<a href="https://github.com/rvirding/lfe/">Lisp Flavoured Erlang</a>
-- is a Lisp syntax front-end to the Erlang compiler. LFE is a
<a href="http://en.wikipedia.org/wiki/Lisp-1_vs._Lisp-2#The_function_namespace">Lisp-2</a>,
like Common Lisp, and comes with a
<a href="http://en.wikipedia.org/wiki/REPL">REPL</a> (shell) for use with
interactive coding.

LFE-based projects coexist seamlessly with Core Erlang and OTP. As such, code
written in LFE can freely be used together with modules written in other BEAM
languages such as Erlang and Elixir.


## LFE Features

We have a <a href="/features.html">Features page</a>, but here are some
of the highlights:

* Homoiconicity
* Lisp macros
* Actor model
* Async
* High concurrency
* Pattern matching
* Functional programming language
* 100% interop with Erlang/OTP
* Interop with Java via JInterface and Erjang


## Overview

### Build Projects with LFE... or Install it

LFE installation isn't recommended. Instead, one should:

* Use <a href="https://github.com/lfe/lfetool">lfetool</a> to create projects
  (which will automatically set up LFE as a dependency when it creates skeleton
  libraries, OTP apps, etc.):

```bash
    $ lfetool new library my-lib
    $ ls -al my-lib/deps/lfe
  ...
```

or

* Use LFE directly in a working dir, e.g.:

```bash

    $ git clone https://github.com/rvirding/lfe.git
    $ cd lfe
    $ git checkout develop
    $ make compile
```

If you really *do* want to install LFE (master branch) system-wide, you can do
so like this:

```bash

    $ lfetool install lfe
```


### REPL

If you have used ``lfetool`` to set up your project, you can simply do this to
start a REPL:

```bash
    $ lfetool repl lfe
```

If you want to compile your source code before you start the repl, you can use
this command:

```bash
    $ make shell
```

Both of those commands will set ``ERL_LIBS`` to the dirs of all your declared
and downloaded dependencies.

If you're running LFE from a git clone working dir, you can start the REPL
like so:

```bash
    $ ./bin/lfe -pa ./ebin
```

Note that if you want access to your dependencies and have started the REPL
manually, you will need to either tack them on with additional ``-pa``
parameters or you'll want to define ``ERL_LIBS``.


### Usage

Here's a quick taste:

* start up an LFE REPL as demonstrated above
* then, do something like this:

```cl
    > (* 2 (+ 1 2 3 4 5 6))
    42
    > (* 2 (lists:foldl (lambda (n acc) (+ n acc)) 0 (lists:seq 1 6)))
    42
```

### Sample Program

Here's an implementation of the famous Ring test for LFE:

```cl
(defmodule ring
  (export
    (main 1)
    (roundtrip 2)))

(defun main (args)
  "Call with the following:
     $ lfec ring.lfe
     $ lfe -smp disable -noshell -run ring main 503 50000000
  "
  (apply
    #'start-ring/2
    (lists:map #'list_to_integer/1 args)))

(defun start-ring (process-count traversal-count)
  (let ((batch (make-processes process-count traversal-count)))
    (! batch traversal-count)
    (roundtrip 1 batch)))

(defun make-processes (process-count traversal-count)
  (lists:foldl
    #'make-process/2
    (self)
    (lists:seq process-count 2 -1)))

(defun make-process (id pid)
  (spawn 'ring 'roundtrip (list id pid)))

(defun roundtrip (id pid)
  (receive
    (1
      (io:fwrite '"Result: ~b~n" (list id))
      (erlang:halt))
    (data
      (! pid (- data 1))
      (roundtrip id pid))))
```

## Cool! How do I start?

LFE documentation is maintained in the source code in the
<a href="https://github.com/rvirding/lfe/tree/master/doc">doc directory</a>.
There is also a <a href="https://github.com/rvirding/lfe/wiki">wiki</a> with
some excellent content presented there.  However, the site you are currently
reading is attempting to provide a gentler and more verbose introduction to
Lisp Flavored Erlang. We aim to accomplish this in two important ways:

* First, with a <a href="/quick-start/1.html">Quick Start</a>. This page gives
  a nice general overview of LFE, providing some content as well as a sense of
  what you can do with it.
* Secondly, over time, we will be providing a more detailed version of the
  source code documentation in the <a href="/docs">Docs</a> section of this
  site. Note, however, that for now, those pages are currently under active
  development.

A final resource for the curious and motivated is available in the
<a href="https://github.com/rvirding/lfe/tree/master/examples">examples</a>
directory of the project repo. Each sample provides functioning code that
shows how to use LFE in larger contexts.

Enjoy!


