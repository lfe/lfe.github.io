---
layout: book
title: LFE Quick Start
author: Duncan McGreggor
---
<a name="quick_start"></a>
# Quick Start

<a name="1_getting_set_up"></a>
## 1 Getting Set Up

<a name="11_dependencies"></a>
### 1.1 Dependencies

First and foremost, you will need Erlang installed. On Mac OS X, this is as
easy as executing ```brew install erlang``` or on Ubuntu ```apt-get install
erlang```. You can also install Erlang from the various pre-built packages
provided on the <a href="http://www.erlang.org/download.html">official Erlang
download page</a> or from the
<a href="https://www.erlang-solutions.com/downloads/download-erlang-otp">Erlang
Solutions page</a> (supports many more package types).

If you will be using ```rebar``` to build LFE, you'll need to
<a href="https://github.com/basho/rebar">get that</a> too.

You will need to <a href="http://git-scm.com/downloads">download git</a> or
install it using your favorite package manager.

Next, you will need to download LFE itself:

    $ git clone git://github.com/rvirding/lfe.git

<a name="12_building"></a>
### 1.2 Building

With the dependencies installed, we're now ready to build LFE.

<a name="121_using_```make```"></a>
#### 1.2.1 Using ```make```

    $ cd ./lfe
    $ erlc -o src src/lfe_scan.xrl
    $ make

Ordinarily, only the ```make``` command would be necessary. However, there's
currently <a href="https://github.com/rvirding/lfe/issues/14">an issue</a> with
the ```Makefile``` that temporarily requires that first step.

<a name="122_using_```rebar```"></a>
#### 1.2.2 Using ```rebar```

Alternatively, one may use ```rebar``` to build LFE:

    $ cd ./lfe
    $ rebar compile

<a name="13_installing"></a>
### 1.3 Installing

On non-development systems, or any system where you don't want to run LFE from
a git checkout, installing system-wide is the preferred way to use LFE. If you
set your ```$ERL_LIBS``` environment variable, LFE will install there. Here's
how one might do this on a Mac OS X system with Erlang installed by
<a href="http://mxcl.github.com/homebrew/">Homebrew</a>:

    $ export ERL_LIBS=/usr/local/lib/erlang/lib
    $ make install

You can then check that everything is where you expect:

    $ ls -1 $ERL_LIBS|grep lfe
    lfe-0.7

Now you can use LFE from anywhere:
{% highlight erlang %}
$ erl
Erlang R15B03 (erts-5.9.3.1) [source] [64-bit] [smp:8:8] [async-threads:0]
[hipe] [kernel-poll:false] [dtrace]

Eshell V5.9.3.1  (abort with ^G)
1> lfe_shell:start().
LFE Shell V5.9.3.1 (abort with ^G)
<0.33.0>
>
{% endhighlight %}

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