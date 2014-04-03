---
layout: book
title: LFE Quick Start
author: Duncan McGreggor
---
<a name="quick_start"></a>
# Quick Start

<a name="1_going_plaid"></a>
## 1 Going Plaid

This guide give you what you need in order to jump into LFE at ludicrous speed.
For a more gradual introduction (with a bit more Erlang), for something that's
going to give you time to see the stars, checkout the
<a href="http://lfe.github.io/user-guide/intro/1.html">User Guide</a>.


<a name="11_lfetool"></a>
### 1.1 ``lfetool``

For a quick-start, ``lfetool`` has everything you need. This guide will use it
almost exclusively, in the interst of sending you a week and a half into the
future. Where appropriate, we will point you to other resources that provide
more details.

Let's get started: go ahead and install ``lfetool``:

```bash
$ curl -o ./lfetool https://raw.github.com/lfe/lfetool/master/lfetool
$ bash lfetool install /usr/local/bin
```

If you need ``sudo`` to put the script there, be sure to set the ownership:

```bash
$ chown $USER /usr/local/bin/lfetool
```

That way you'll be able to use the ``update`` command to get the latest version
in the future.


<a name="12_other_dependencies"></a>
### 1.2 Other Dependencies

First and foremost, you will need Erlang installed. On Mac OS X, this is as
easy as executing ```brew install erlang``` or on Ubuntu ```apt-get install
erlang```. You can also install Erlang from the various pre-built packages
provided on the <a href="http://www.erlang.org/download.html">official Erlang
download page</a> or from the
<a href="https://www.erlang-solutions.com/downloads/download-erlang-otp">Erlang
Solutions page</a> (supports many more package types).

Peronally, we prefer to manage our Erlang builds and installations with
<a href="">kerl</a>. This allows for multiple versions of Erlang to be installed
on your system simultaneously. For more information about using ``kerl`` in LFE
projects, be sure to visit the "kerl" section of the
<a href="http://lfe.github.io/user-guide/intro/4.html">User Guide</a>.

You will be using ```rebar``` to build LFE under the covers as well as managing
dependencies for your projects, so go ahead and get that set up:
<a href="https://github.com/basho/rebar">get rebar</a>.

You will need to <a href="http://git-scm.com/downloads">download git</a> or
install it using your favorite package manager.


<a name="13_your_first_lfe_project"></a>
### 1.3 Your First LFE Project

It sounds daunting, but it's just one command :-) Head over to your favorite
workspace in an open terminal and do this:

```bash
$ lfetool new library my-test-lib
```

As you watch it crank away, read on to find out what i's doing:
* ``lfetool`` created a skeleton project (of the "library" type)
* ``rebar`` is downloaded all the dependencies listed in the new
  ``rebar.config`` file that was created for your project
* All the downloaded dependendencies were then compiled
* A failing test case was created for you, and the test suite was run,
  revealing this error (TDD!)

<a name="131_tests_first!"></a>
#### 1.3.1 Tests First!

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


<a name="132_project_inventory"></a>
#### 1.3.2 Project Inventory

Okay, we've got our tests passing and our project looks healthy. But what does
it have in it?

* a README file in ReStructured Text
* a ``Makefile``
* the ``rebar.config`` file we mentioned about (this has all your project
  dependencies in it)
* a ``package.exs`` file for uploading your project to http://expm.co
* the ``src`` directory which holds your project code, and
* the ``test`` directory which holds the tests

There are some other things there (e.g., the deps directory created by ``rebar``
and the ``ebin`` directory that's created when everything is compiled), but
we're just going to focus on our stuff for now.


<a name="133_show_me_lfe!"></a>
#### 1.3.3 Show me LFE!

Sure thing :-) Just do this from your new LFE project directory:

```bash
make shell-no-deps
```

This should give you something that looks like this:

```cl
Erlang R16B (erts-5.10.1) [source] [smp:8:8] [async-threads:10] [hipe] [kernel-poll:false]

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

Enough fancy stuff for now, what about code? What does a project look like?

Well, you've already seen one! Let's print it here for your viewing pleasure,
though. Here's the starter project you have, with just the one module:

```cl
(defmodule my-test-lib
  (export all))


(defun my-adder (x y)
  (+ x (+ y 1)))
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

Almost cound't be simpler.

Here's something a little more involved you may enjoy, fro the examples in the
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
that example demonstrates bidirectional message passing between the LFE shell
and a spawned process.)


<a name="111_using_make"></a>
#### 1.1.1 Using ```make```

Okay, we promised to come back to ``make``, so here we are.

Eventually, the ``Makefile`` will be replaced with a combination of ``rebar``
scripts and updates to ``lfetool``, but for now it's what we've got.

There are some convenient targets available for you in the ``Makefile``:

* upload - when you're project's ready for release, this will push it up to
  <a href="http://expm.com/">expm.co</a> for you
* compile - rebuilds all your ``.beam``s, including the dependencies
* install - should you wish to instlal your project system-wide (we recommend
  against it), you could use this target to do it
* check - run your unit tests after recompiling all ``*.beam``s
* shell - start up the LFE REPL after recompiling all ``*.beam``s
* get-deps - not only does this target get your deps initially, but subsequent
  runs of it will perform ``git pull``s for the repos cloned in your ``deps``
  dir, ensuring you have the latest code for everything

If you do not need to recompile dependencies, you can skip the deps with the
following targets:

* compile-no-deps - only project ``.beam``s get recompiled
* shell-no-deps
* check-no-deps

There are others, and you can learn more about how to manage LFE projects by
reading the ``Makefile``.

One thing to keep in mind for future reference: if you add new deps to your
``rebar.confg`` file, you'll need to update the ``ERL_LIBS`` in your
``Makefile`` to include them.

Read more about setting up a development environment
<a href="http://lfe.github.io/user-guide/intro/4.html">here</a>.

<a name="2_executing_code"></a>
## 2 Executing Code

Once you've got LFE built, you want to play with it, right? Let's take a look
at the REPL (interactive shell) first.

<a name="21_the_repl"></a>
### 2.1 The REPL

To start the REPL, simply run the ```lfe``` script and tell ```erl``` (which is
being run in the script) which additional code paths it should look for (in this
case, the compiled LFE code):

    $ ./bin/lfe -pa ./ebin

This assumes that you are still in the ```lfe``` directory where you build LFE.
Running that command will dump you into the LFE REPL, looking something like
this:

    Erlang R15B03 (erts-5.9.3.1) [source] [64-bit] [smp:8:8] [async-threads:0] ...

    LFE Shell V5.9.3.1 (abort with ^G)
    >

Note that you can also start the the LFE shell manually from an existing Erlang
shell (as we did earlier in this guide) or you can pass parameters to ```erl```
to start it up:

    $ erl -pa ebin -s lfe_boot -noshell -noinput

Now try doing some basic operations:

{% highlight cl %}
> (+ 1 2 3)
6
> (cons 1 2)
(1 . 2)
> (cons (list 1 2) (list 3 4))
((1 2) 3 4)
>
{% endhighlight %}

Next, let's operate on some variables:

{% highlight cl %}
> (let ((x 123456789)) x)
123456789
> (let ((x 123456789)) (* x x))
15241578750190521
> (let ((x 123456789)) (* x x x x))
232305722798259244150093798251441
>
{% endhighlight %}

Looking good!

<a name="22_running_scripts"></a>
### 2.2 Running Scripts

Okay, so now that you can run things in the REPL, you want to run them as a
script, yes? Here's how.

Let's create a temporary subdirectory to play in without fear of messing up our
LFE directory:

    $ mkdir tmp
    $ cd tmp

Then, in that directory, let's create the following file and save it as
```hello.lfe```:

{% highlight cl %}
(defmodule hello
  (export (start 0)))

(defun start ()
  (: io format '"Lfe says 'Hello, World!'~n"))
{% endhighlight %}

To compile that script and then run it, we can do this:

    $ ../bin/lfec hello.lfe
    $ ../bin/lfe -s hello start -s erlang halt

Or, if we want to use Erlang directly, we could do this:

    $ erl -pa ../ebin -s lfe_comp file hello -s erlang halt
    $ erl -pa ../ebin -s hello start -s erlang halt

Or, we could compile it and run it in the same command:

    $ erl -pa ../ebin -s lfe_comp file hello -s hello start -s erlang halt

Note that this is the command line equivalent of the following:

    $ erl -pa ../ebin
    1> lfe_comp:file(hello).
    {ok,hello}
    2> hello:start().
    Lfe says 'Hello, World!'
    ok
    3>

<a name="23_running_scripts_from_the_repl"></a>
### 2.3 Running Scripts from the REPL

You can also use your new ```hello.lfe``` script in the REPL itself. There are
two ways you can do this, using ```slurp``` or compiling the file. If you use
```slurp```, all the functions are pulled into the shell namespace, and you
won't have to reference the module name.  Again, assuming that you are in
```lfe/tmp```:
{% highlight cl %}
$ ../bin/lfe -pa ../ebin
> (slurp '"hello.lfe")
#(ok hello)
> (start)
Lfe says 'Hello, World!'
ok
>
{% endhighlight %}

If you choose to compile your module instead, you will use it like so:
{% highlight cl %}
$ ../bin/lfe -pa ../ebin
> (c '"hello")
#(module hello)
> (: hello start)
Lfe says 'Hello, World!'
ok
>
{% endhighlight %}

Note that in the second example, you need to reference the module.

For more information on the LFE shell, be sure to see the "REPL" section of the
User Guide Introduction.

<a name="3_using_libraries"></a>
## 3 Using Libraries

<a name="31_otp_modules"></a>
### 3.1 OTP Modules

Taking advantage of the
<a href="http://erldocs.com/R15B/index.html?i=734#stdlib">Erlang stdlib</a> is
straightforward. All you need to do is prepend the call with a ```:``` and
adjust to use the LFE syntax vs. the Erlang syntax.

In fact, we've already seen an example of this in Section 2
when we called ```(: io format ...)``` as part of a "Hello World."

Here's an example ```base64``` usage from the Erlang ```stdlib```:

{% highlight cl %}
> (: base64 encode_to_string '"Space is big. Really big.")
"U3BhY2UgaXMgYmlnLiBSZWFsbHkgYmlnLg=="
> (: base64 decode_to_string '"QW5kIHNvIHRoZSBVbml2ZXJzZSBlbmRlZC4=")
"And so the Universe ended."
{% endhighlight %}

<a name="32_processes_in_lfe"></a>
### 3.2 Processes in LFE

One of the first things that people want to do with LFE is examine the message
passing syntax so that they can compare it with vanilla Erlang. Here's a small
example of what this looks like in LFE:

{% highlight cl %}
(defun print-result ()
  (receive
    (msg
      (: io format '"Received message: '~s'~n" (list msg))
      (print-result))))
{% endhighlight %}

If that was saved in a module called ```messenger```, then one could utilize it
thusly:
{% highlight cl %}
> (set pid (spawn 'messenger 'print-result ()))
<0.34.0>
> (! pid '"Ford is missing.")
"Ford is missing."
Received message: 'Ford is missing.'
{% endhighlight %}

For  more information on working with processes in LFE, be sure to view the
<a href="/tutorials/processes/1.html">tutorial</a>.

Related to this, you can find details and discussion around OTP and creating
your own servers in the
<a href="/tutorials/otp-servers/1.html">OTP Servers tutorial</a>.

<a name="33_third-party_libraries"></a>
### 3.3 Third-Party Libraries

Finally, accessing code that is written in third-party libraries is exactly the
same. Simply use the modules they have provided. If you started the LFE REPL
pointing to your third-party libraries with a ```-pa``` (path) option, then all
you have to do is this:

{% highlight cl %}
> (: your-module some-function '"some parameter")
{% endhighlight %}

That's it!

<a name="4_next_steps"></a>
## 4 Next Steps

This has been a quick overview of what LFE has to offer, so you might enjoy
reading the <a href="/user-guide/intro/1.html">User Guide</a>
next. You can see all our docs at a glance by visiting the
<a href="/docs.html">Docs</a> page.

If at any time you would like to provide feedback about the documentation on
this site, feel free to
<a href="https://github.com/lfe/lfe.github.io/issues">create an issue</a>. Be
sure to give a full description so that we can best help you!