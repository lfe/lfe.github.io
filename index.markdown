---
layout: home
---

# Nothing Quite Compares

To the taste of Erlang, aged in the oaken barrels of Lisp, served at a
temperature of perfect hotness:

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

### Designer Templates
We've crafted some handsome templates for you to use. Go ahead and continue to
layouts to browse through them. You can easily go back to edit your page before
publishing. After publishing your page, you can revisit the page generator and
switch to another theme. Your Page content will be preserved if it remained
markdown format.

### Rather Drive Stick?
If you prefer to not use the automatic generator, push a branch named
`gh-pages` to your repository to create a page manually. In addition to
supporting regular HTML content, GitHub Pages support Jekyll, a simple, blog
aware static site generator written by our own Tom Preston-Werner. Jekyll makes
it easy to create site-wide headers and footers without having to copy them
across every page. It also offers intelligent blog support and other advanced
templating features.

### Authors and Contributors
You can @mention a GitHub username to generate a link to their profile. The
resulting `<a>` element will link to the contributor's GitHub Profile. For
example: In 2007, Chris Wanstrath (@defunkt), PJ Hyett (@pjhyett), and Tom
Preston-Werner (@mojombo) founded GitHub.

### Support or Contact
Having trouble with Pages? Check out the documentation at
http://help.github.com/pages or contact support@github.com and we’ll help you
sort it out.
