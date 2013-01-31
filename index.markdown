---
layout: home
---

### Nothing Quite Compares

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
The docs for the project are maintained in the source code:
https://github.com/rvirding/lfe/tree/master/doc

However, this site is attempting to provide a gentler introduction. As such,
you can check out the <a href="/quick-start/1.html">Quick Start</a> pages, if
you're looking for a nice general overview of LFE.

Over time, we will be providing a more verbose version of the source code docs
here: http://lfe.github.com/docs. For now, though, those pages are empty.

More complete code that you can run are kept in the project repo's
"examples" directory: https://github.com/rvirding/lfe/tree/master/examples

We've crafted some handsome templates for you to use. Go ahead and continue to
layouts to browse through them. You can easily go back to edit your page before
publishing. After publishing your page, you can revisit the page generator and
switch to another theme. Your Page content will be preserved if it remained
markdown format.
