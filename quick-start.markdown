---
layout: docs-home
---
# Quick Start

## 1 Dependencies

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

## 2 Building

With the dependencies installed, we're now ready to build LFE.

### 2.1 Using ```make```

    $ cd ./lfe
    $ erlc -o src src/lfe_scan.xrl
    $ make

Ordinarily, only the ```make``` command would be necessary. However, there's
currently <a href="https://github.com/rvirding/lfe/issues/14">an issue</a> with
the ```Makefile``` that temporarily requires that first step.

### 2.2 Using ```rebar```

Alternatively, one may use ```rebar``` to build LFE:

    $ cd ./lfe
    $ rebar compile

## 3 Executing Code

Once you've got LFE built, you want to play with it, right? Let's take a look
at the REPL (interactive shell) first.

### 3.1 The REPL

To start the REPL, simply run the ```lfe``` script and tell ```erl``` (which is
being run in the scrip) which additional code paths it should look for (in this
case, the compiled LFE code):

    $ ./lfe -pa ./ebin

This assumes that you are still in the ```lfe``` directory where you build LFE.
Running that command will dump you into the LFE REPL, looking something like
this:

    Erlang R15B03 (erts-5.9.3.1) [source] [64-bit] [smp:8:8] [async-threads:0] ...

    LFE Shell V5.9.3.1 (abort with ^G)
    >

Now try doing some basic operations:

    > (+ 1 2 3)
    6
    > (cons 1 2)
    (1 . 2)
    > (cons (list 1 2) (list 3 4))
    ((1 2) 3 4)
    >

Looking good!

### 3.2 Running Scripts

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

### 3.3 Running Scripts from the REPL

You can also use your new ```hello.lfe``` script in the REPL itself. Again,
assuming that you are in ```lfe/tmp```:


{% highlight cl %}
$ ../lfe -pa ../ebin
> (slurp '"hello.lfe")
#(ok hello)
> (: hello start)
Lfe says 'Hello, World!'
ok
>
{% endhighlight %}

## 4 Using Libraries

### 4.1 OTP Modules

Taking advantage of the
<a href="http://erldocs.com/R15B/index.html?i=734#stdlib">Erlang stdlib</a> is
straightforward. All you need to do is prepend the call with a ```:``` and
adjust to use the LFE syntax vs. the Erlang styntax.

In fact, we've already seen an example of this above with the ```(: io format ...)```
call.

Here's an example ```base64``` usage from the Erlang ```stdlib```:

{% highlight cl %}
> (: base64 encode_to_string '"Space is big. Really big.")
"U3BhY2UgaXMgYmlnLiBSZWFsbHkgYmlnLg=="
> (: base64 decode_to_string '"QW5kIHNvIHRoZSBVbml2ZXJzZSBlbmRlZC4=")
"And so the Universe ended."
{% endhighlight %}

### 4.2 More OTP

The rest of OTP utilizes the same ```: <module> <function> <parameters>```
format. Let's take a more in-depth look, exercising some of the niftier
features of OTP, starting with a server:

{% highlight cl %}
{% endhighlight %}


### 4.3 Third-Party Libraries

Finally, accessing code that is written in third-party libraries is exactly the
same. Simply use the modules they have provided:


{% highlight cl %}
{% endhighlight %}

## 5 Next Steps

This has been a quick overview of what LFE has to offer, so you might enjoy
reading the <a href="http://lfe.github.com/user-guide/1.html">User Guide</a>
next. You can see all our docs at a glance by visiting the
<a href="/docs.html">Docs</a> page.
