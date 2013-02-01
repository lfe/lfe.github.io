---
layout: home
---

## Nothing Quite Compares

To the taste of Erlang, aged in the oaken barrels of Lisp, served at a
temperature of perfect hotness.

### What is this?

You are at the home page of LFE -- Lisp Flavoured Erlang -- a Lisp syntax
front-end to the Erlang compiler. LFE is a Lisp-2, like Common Lisp. LFE comes
with a REPL (shell) for use with interactive coding.

Also, note that code produced with LFE is compatible with normal Erlang code.

### Features

Below are some of the key features of LFE, with code samples to help illustrate
them.

#### Macros

TBD

#### Pattern Matching

TBD

### Shiny!

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
    (tuple 'reply (tuple 'pong new-count) new-state)))

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
However, this site is attempting to provide a gentler and more verbose
introduction to Lisp Flavored Erlang. We aim to accomplish this in two
important ways:

* First, with a <a href="/quick-start/1.html">Quick Start</a>. This set of
  pages gives a nice general overview of LFE.
* Secondly, over time, we will be providing a more detailed version of the
  source code documentation in the <a href="http://lfe.github.com/docs">Docs</a>
  section of this site. Note, however, that for now, those pages are empty.

A final resource for the curious and motivated is available in the
<a href="https://github.com/rvirding/lfe/tree/master/examples">examples</a>
directory of the project repo. This is functioning code those shows how to use
LFE in blocks of code larger than the snippets provided in the documentation.
