---
layout: book
title: LFE Quick Start
author: Duncan McGreggor
---
<a name="quick_start"></a>
# Quick Start

<a name="1_going_plaid"></a>
## 1 Going Plaid

<img src="https://raw.github.com/lfe/lfe.github.io/master/images/plaid.jpg"
     style="float: right;">
This guide will give you what you need in order to jump into LFE at ludicrous
speed.

For a more gradual introduction (with a bit more Erlang) and for
something that's going to give you time to see the stars, checkout the
<a href="http://lfe.github.io/user-guide/intro/1.html">User Guide</a>.


<a name="11_lfetool"></a>
### 1.1 lfetool

For a quick introduction to LFE, ``lfetool`` has everything you need. The
Quick Start guide will use it almost exclusively -- all in the interst of
sending you a week and a half into the future.

Where appropriate, we will point you to other resources that provide more
details.

Let's get started: install ``lfetool``!

```bash
$ curl -o ./lfetool https://raw.github.com/lfe/lfetool/master/lfetool
$ bash lfetool install /usr/local/bin
```

Note that this does require the most recent release of Erlang right now (r16).
(We are working towards full support for r15 and will update this Quick Start
when that has landed.)

If you need ``sudo`` to put the script there, be sure to set the ownership:

```bash
$ chown $USER /usr/local/bin/lfetool
```

That way you'll be able to use the ``update`` command to get the latest version
of ``lfetool`` in the future.


<a name="12_other_dependencies"></a>
### 1.2 Other Dependencies

First and foremost, you will need Erlang installed. On Mac OS X, this is as
easy as executing ```brew install erlang``` or on Ubuntu ```apt-get install
erlang```. You can also install Erlang from the various pre-built packages
provided on the <a href="http://www.erlang.org/download.html">official Erlang
download page</a> or from the
<a href="https://www.erlang-solutions.com/downloads/download-erlang-otp">Erlang
Solutions page</a> (supports many more package types).

Personally, we prefer to manage our Erlang builds and installations with
<a href="https://github.com/spawngrid/kerl">kerl</a>.
This allows for multiple versions of Erlang to be installed
on your system simultaneously. For more information about using ``kerl`` in LFE
projects, be sure to visit the "kerl" section of the
<a href="http://lfe.github.io/user-guide/intro/4.html">User Guide</a>.

You will be using ```rebar``` to build LFE under the covers as well as managing
dependencies for your projects, so go ahead and get that set up:
<a href="https://github.com/basho/rebar">get rebar</a>.

You will also need to <a href="http://git-scm.com/downloads">download git</a> or
install it using your favorite package manager.


<a name="next_stop"></a>
### Next Stop

Ready for some LFE? Next you'll learn how to create a new LFE project with
just one command ...

<a name="2_your_first_lfe_project"></a>
## 2 Your First LFE Project

<img src="https://raw.github.com/lfe/lfe.github.io/master/images/barf.jpg"
     style="float: left; padding-right: 1em;">
A project? Already?! It sounds daunting, but it's just one command :-)

Head over to your favorite workspace in an open terminal and do this:

```bash
$ lfetool new library my-test-lib
```

As you watch the output crank away, read on to find out what it's doing:

* ``lfetool`` created a skeleton project (of the "library" type)
* ``rebar`` downloaded all the dependencies listed in the new
  ``rebar.config`` file that was created for your project
* All the downloaded dependendencies were then compiled
* During the first step, a failing test case was created for you; now, that
  test case has been run in the test suite and the error has been revealed
  (TDD!)


<a name="21_tests_first!"></a>
### 2.1 Tests First!

First things first: let's get that failing unit test passing.

Here's the output we saw:

```erlang
**error:{assertEqual_failed,[{module,'my-test-lib_tests'},
                     {line,11},
                     {expression,"(: my-test-lib my-adder 2 2)"},
                     {expected,4},
                     {value,5}]}
```

We can see that our test expected 4 but it got 5.

Before we go further, be sure you are in your project directory:

```bash
$ cd my-test-lib
```

Let's look at the test by opening up ``test/my-test-lib_tests.lfe``.

Well, things there look good -- we should get 4 when 2 and 2 are added.

Let's take a look at the source module: ``src/my-test-lib.lfe``. Ah-ha! a
bug :-) There's an extra operation that's being done which we don't want.

To fix it, change this line:

```cl
  (+ x (+ y 1)))
```

to this:

```cl
  (+ x y))
```

Now re-run the unit tests:

```bash
$ make check-no-deps
```

And you should get a passing test:

```bash
==> my-test-lib (eunit)
======================== EUnit ========================
my-test-lib_tests: my-adder_test (module 'my-test-lib_tests')...[0.031 s] ok
=======================================================
  Test passed.
```

We'll talk about the ``make`` targets in a bit, but our target above run the
unit tests without recompiling all the deps; just our project files and
our unit tests.


<a name="22_project_inventory"></a>
### 2.2 Project Inventory

Okay, we've got our tests passing and our project looks healthy. But what does
it have in it?

* a README file in ReStructured Text
* a ``Makefile``
* the ``rebar.config`` file we mentioned earlier (this has all your project
  dependencies in it)
* a ``package.exs`` file for uploading your project to
  <a href="http://expm.co">expm.co</a>
* the ``src`` directory which holds your project code, and
* the ``test`` directory which holds the tests

There are some other things there (e.g., the ``deps`` directory created by ``rebar``
and the ``ebin`` directory that's created when everything is compiled).


<a name="23_repo_ready_to_go"></a>
### 2.3 Repo Ready to Go

``lfetool`` also did one more thing for you: ``init``ed the project in git
and added all the files:

```bash
$ git status
<a name="on_branch_master"></a>
# On branch master
<a name=""></a>
#
<a name="initial_commit"></a>
# Initial commit
<a name="changes_to_be_committed:"></a>
# Changes to be committed:
<a name="(use_"git_rm_--cached_<file>"_to_unstage)"></a>
#   (use "git rm --cached <file>..." to unstage)
<a name="new_file:___gitignore"></a>
#   new file:   .gitignore
<a name="new_file:___makefile"></a>
#   new file:   Makefile
<a name="new_file:___readmerst"></a>
#   new file:   README.rst
<a name="new_file:___packageexs"></a>
#   new file:   package.exs
<a name="new_file:___rebarconfig"></a>
#   new file:   rebar.config
<a name="new_file:___src/my-test-libappsrc"></a>
#   new file:   src/my-test-lib.app.src
<a name="new_file:___src/my-test-liblfe"></a>
#   new file:   src/my-test-lib.lfe
<a name="new_file:___test/my-test-lib_testslfe"></a>
#   new file:   test/my-test-lib_tests.lfe
```
All you need to do it ``git commit -a && git push --all`` to wherever you want
to put your project :-)



You can taste it, can't you? That LFE flavor coming your way? Yup, you're right.
You're going to be looking at LFE code next ...

<a name="3_hitting_the_code"></a>
## 3 Hitting the Code

<img src="https://raw.github.com/lfe/lfe.github.io/master/images/smash.jpg"
     style="float: right; padding-left: 1em;">
It may not seem like it, but we're off to a pretty fast start. If you had to
do everything we've done, manually, you'd have given up by now. Seriously.

Time to put the brakes on, though, 'cause you're gonna want to see this next
part in slow motion.


<a name="31_repl_me_up!"></a>
### 3.1 REPL Me Up!

Sure thing :-) Just do this from your new LFE project directory:

```bash
make shell-no-deps
```

(There's that ``-no-deps`` thing again... don't worry, we're going to tell
you about it.)

This should give you something that looks like the following:

```cl
Erlang R16B (erts-5.10.1) [source] [smp:8:8] [async-threads:10] [hipe] ...

LFE Shell V5.10.1 (abort with ^G)
>
```

We're not going to spend a lot of time in the REPL right now, but feel free to
take it for a spin :-)

```cl
> (+ 1 2 3 4 5 6)
21
> (* 2 (+ 1 2 3 4 5 6))
42
> (: lists foldl (lambda (n acc) (+ n acc)) 0 (: lists seq 1 6))
21
```

Enough fancy stuff! What about code? What does a project look like?

<a name="32_sample_code"></a>
### 3.2 Sample Code

Well, you've already seen one! Let's print it here for your viewing pleasure,
though. Here's the starter project you have, with just the one module:

```cl
(defmodule my-test-lib
  (export all))

(defun my-adder (x y)
  (+ x y))
```

It has a corresponding test module:

```cl
(defmodule my-test-lib_tests
  (export all)
  (import
    (from lfeunit-util
      (check-failed-assert 2)
      (check-wrong-assert-exception 2))))

(include-lib "deps/lfeunit/include/lfeunit-macros.lfe")

(deftest my-adder
  (is-equal 4 (: my-test-lib my-adder 2 2)))
```

Almost couldn't be simpler.

Here's something a little more involved you may enjoy, from the examples in the
LFE source code:

```cl
(defmodule messenger-back
 (export (print-result 0) (send-message 2)))

(defun print-result ()
  (receive
    ((tuple pid msg)
      (: io format '"Received message: '~s'~n" (list msg))
      (: io format '"Sending message to process ~p ...~n" (list pid))
      (! pid (tuple msg))
      (print-result))))

(defun send-message (calling-pid msg)
  (let ((spawned-pid (spawn 'messenger-back 'print-result ())))
    (! spawned-pid (tuple calling-pid msg))))
```

That bit of code demonstrates one of Erlang's core features in lovely Lisp
syntax :-) (For those that don't read Erlang yet, when loaded into the REPL,
that example shows some bidirectional message passing between the LFE shell
and a spawned process.)

If you'd like to learn more about using the LFE REPL, be sure to read the
<a href="http://lfe.github.io/user-guide/intro/2.html">REPL Section</a> of the
User Guide.

If you want to step through a tutorial on Erlang's light-weight threds (per the
example code above), you'll want to read
<a href="http://lfe.github.io/tutorials/processes/1.html">this tutorial</a>.



We did promise a bit more information, so we're going to do that next and then
wrap up the quick start and point you in the direction of your next LFE
adventure ...

<a name="4_behind_the_scenes"></a>
## 4 Behind the Scenes

<img src="https://raw.github.com/lfe/lfe.github.io/master/images/doubles.jpg"
     style="float: right; padding-left: 1em;">

<a name="41_using_make"></a>
### 4.1 Using make

We promised to come back to ``make``, so here we are.

Eventually, the ``Makefile`` will be replaced with a combination of ``rebar``
scripts and updates to ``lfetool``, but for now it's what we've got.

There are some convenient targets available for you in the ``Makefile``:

* ``upload`` - when your project's ready for release, this will push it up to
  <a href="http://expm.co/">expm.co</a> for you
* ``compile`` - rebuilds all your ``.beam``s, including the dependencies
* ``install`` - should you wish to install your project system-wide (we recommend
  against it), you could use this target to do it
* ``check`` - run your unit tests after recompiling all ``*.lfe`` files ``*.beam``
* ``shell`` - start up the LFE REPL after recompiling all ``*.lfe`` files
* ``get-deps`` - not only does this target get your deps initially, but subsequent
  runs of it will perform ``git pull``s for the repos cloned in your ``deps``
  dir, ensuring you have the latest code for everything

If you do not need to recompile dependencies (and most of the time, you don't),
you can skip the deps with the following targets:

* ``compile-no-deps``
* ``shell-no-deps``
* ``check-no-deps``

There are other interesting ``make`` targets tucked in the ``Makefile``, and
you can learn more about how to manage LFE projects by checking them out.

One thing to keep in mind for future reference: if you add new deps to your
``rebar.confg`` file, you'll need to update the ``ERL_LIBS`` in your
``Makefile`` to include them.

Read more about setting up a development environment
<a href="http://lfe.github.io/user-guide/intro/4.html">here</a>.


<a name="42_next_steps"></a>
### 4.2 Next Steps

This has been a quick overview of what LFE has to offer, so you might enjoy
reading the <a href="/user-guide/intro/1.html">User Guide</a>
next. You can see all our docs at a glance by visiting the
<a href="/docs.html">Docs</a> page.

There is a <a href="http://lfe.github.io/tutorials/processes/1.html">tutorial
on light-weight Erlang processes</a> in LFE that you may be intersted in.

If you want to develop RESTful services in LFE, you should take a look at the
<a href="https://github.com/lfe/yaws-rest-starter">REST on YAWS with LFE</a>
starter project.

The <a href="https://github.com/lfe">LFE organization</a> on Gitub has a large
collection of LFE projects that may be of interest to you.

You should also take some time reading the
<a href="https://github.com/rvirding/lfe">examples</a> provided in the LFE
source code; studying examples is the quickest way to make progress.

If at any time you would like to provide feedback about the documentation on
this site, feel free to
<a href="https://github.com/lfe/lfe.github.io/issues">create an issue</a>. Be
sure to give a full description so that we can best help you!