---
layout: post.liquid
title: "New LFE T-Shirt"
description: "If you missed the last LFE T-shirt, you're gonna want to be in line for this one ..."
permalink: "/blog/formalwear/2015/03/20/1256-new-lfe-t-shirt"
categories: ["formalwear"]
tags: ["hackersuits", "t-shirts", "clothing", "community"]
published_date: 2015-03-20 12:56:00 +0000
is_draft: false
data:
  author: duncan-mcgreggor
  written_for: null
  last_validated: null
  cover_image: null
  cover_alt: null
  math: false
---
<a href="/blog/assets/images/posts/lfescars2-front-back.png"><img class="left thumb" src="/blog/assets/images/posts/lfescars2-front-back.png" /></a>The new LFE T-shirt is designed and awaiting more sign-ups! We're about 1/4 of the way (**Update**: now 1/2!) to the minimum pledges required. If you'd like to order one or ten, you may do so <a href="http://www.customink.com/signup/96rbuknu">here</a>. For more information on the process, read on ...

Even though the
<a href="https://twitter.com/oubiwann/status/493803591748968451">tweet
announcing the new T-shirt</a> was back in July, you haven't lost your chance
to get the new shirt :-) A lot was going on during the summer and fall, and
let's face it, who's going to do anything during the winter? But, it's the
first day of spring, so we have no excuses any more: it's time to get this
puppy re-started!

### How This Works

<a href="/blog/assets/images/posts/lfescars2-front.png"><img class="right medium" src="/blog/assets/images/posts/lfescars2-front.png" /></a>
Step 1: In order to get the price offered by
<a href="http://www.customink.com/">Custom Ink</a> ($28),
we need 40 t-shirt "pledges" in the <a href="http://www.customink.com/signup/96rbuknu">sign up form</a>.
And by pledges, we
just mean listing your size(s) and how many you want; payment comes later.

Step 2: Once we've confirmed that enough T-shirts are desired, we'll
start emailing everyone who has pledged, letting you know it's time to Pay the
Pal :-)

Step 3: As soon as all shirts are paid for, we'll place the order with CustomInk.

Step 4: When the shipment arrives, we'll mail each of you your T-shirt orders.

At each step of the way, we'll keep you posted on status. We used this approach
last time, and it worked out well -- we got lots of pictures of happy hackers
with their LFE T-shirts :-)

### More About The T-Shirt

The front of the shirt has the LFE logo (high-octane λ-amrita) over the left
breast (keeping LFE close to your heart). The back of the shirt has a watermark
of an LFE translation of the Erlang ring benchmark used on the
<a href="http://benchmarksgame.alioth.debian.org/u32/program.php?test=threadring&lang=hipe&id=1">Computer
Language Benchmarks Game</a> site:

```lfe
(defmodule ring
  (export
    (main 1)
    (roundtrip 2)))

(defun main (args)
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

Then, emblazoned across the shoulders is the
classic "Erlang" script logo. Underneath that is a reference to the closing comments of
"Mike Williams" in Garrett's
<a href="https://www.youtube.com/watch?v=rRbY3TMUcgQ">Erlang The Movie II: The Sequel</a>.
(The images in this post are linked to larger images of the T-shirt, should you
want a closer look.)

<a href="/blog/assets/images/posts/lfescars2-back.png"><img class="left medium" src="/blog/assets/images/posts/lfescars2-back.png" /></a>
So, sign up now, and let us know you want a limited-edition LFE T-shirt (there
are only about 20 of the first LFE T-shirts made ... they're so rare, they
don't even show up on eBay!).

### The Future for LFE T-shirts?

We have a ton of great ideas for LFE t-shirts, and we'd like to be able to
iterate on these more quickly. CustomInk is amazing (brand-name shirts,
long-lasting prints) but they are pretty expensive and require larger orders
for higher color counts (and to get the price down so that we can cover the
shipping costs).

As such, we're going to look into various local venues which might be able to
offer a better deal so that we can get these out to you in a more timely
manner, without community members having to queue up around the world every six
months ;-)

Stay tuned!


