---
layout: home
---

## Nothing Quite Compares

To the taste of Erlang, aged in the oaken barrels of Lisp, served at a
temperature of perfect hotness. New to LFE? Check out our
<a href="/quick-start/1.html">Quick Start</a>!

### What is this?

You are at the home page of LFE --
<a href="https://github.com/rvirding/lfe/">Lisp Flavoured Erlang</a>
-- a Lisp syntax front-end to the Erlang compiler. LFE is a
<a href="http://en.wikipedia.org/wiki/Lisp-1_vs._Lisp-2#The_function_namespace">Lisp-2</a>,
like Common Lisp, and comes with a
<a href="http://en.wikipedia.org/wiki/REPL">REPL</a> (shell) for use with
interactive coding.

Also, note that code produced with LFE is compatible with normal Erlang code.

### Features

Below are a selection of code samples representing features available in LFE.
There's much more to see, though -- so don't forget to visit the
<a href="/docs.html">Docs</a> page!

#### Records

Records are very simple in LFE. They are created with the ```defrecord``` form
like so:

{% highlight cl %}
(defrecord person
  name
  address
  age)
{% endhighlight %}

For more information, see the
<a href="/tutorials/records/1.html">tutorial on LFE records</a>.

#### Pattern Matching

The power of Erlang's pattern matching is available in Lisp form:

{% highlight cl %}
(let (((tuple name place age) #("Ford Prefect" "Betelgeuse Seven" 234)))
  (list name place age))
{% endhighlight %}

For more information, see the
<a href="/user-guide/1.html">User Guide</a> as well as the
<a href="/tutorials/patterns/1.html">tutorial on pattern matching</a>.

#### Macros

Here are a couple example LFE macros:

{% highlight cl %}
(defmacro caar (x) `(car (car ,x)))
{% endhighlight %}

{% highlight cl %}
(defmacro list*
  ((list e) e)
  ((cons e es) `(cons ,e (list* . ,es)))
  (() ()))
{% endhighlight %}

Note that the functionality represented by the LFE code above was implemented
internally in Erlang, not in LFE itself.

### More Shiny!

For those that can't wait, here's an example client/server in action:

{% highlight cl %}
(defmodule ping_pong
  (export (start_link 0) (ping 0))
  (export (init 1) (handle_call 3) (handle_cast 2)
          (handle_info 2) (terminate 2) (code_change 3))
  (behaviour gen_server)) ;Just indicates intent

(defun start_link ()
  (: gen_server start_link
    (tuple 'local 'ping_pong) 'ping_pong (list) (list)))

;; Client API
(defun ping ()
  (: gen_server call 'ping_pong 'ping))

;; Gen_server callbacks
(defrecord state (pings 0))

(defun init (args)
  (tuple 'ok (make-state pings 0)))

(defun handle_call (req from state)
  (let* ((new-count (+ (state-pings state) 1))
         (new-state (set-state-pings state new-count)))
    (tuple 'reply
           (tuple 'pong new-count)
           new-state)))

(defun handle_cast (msg state)
  (tuple 'noreply state))

(defun handle_info (info state)
  (tuple 'noreply state))

(defun terminate (reason state)
  'ok)

(defun code_change (old-vers state extra)
  (tuple 'ok state))
{% endhighlight %}

### Cool! How do I start?
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
  source code documentation in the <a href="http://lfe.github.com/docs">Docs</a>
  section of this site. Note, however, that for now, those pages are currently
  under active development.

A final resource for the curious and motivated is available in the
<a href="https://github.com/rvirding/lfe/tree/master/examples">examples</a>
directory of the project repo. This is functioning code those shows how to use
LFE in blocks of code larger than the snippets provided in the documentation.
