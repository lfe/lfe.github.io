---
layout: post.liquid
title: "LFE Friday - erl_tar:table/1"
description: ""
permalink: "/blog/tutorials/2015/10/18/1451-lfe-friday---erl_tartable1"
categories: ["tutorials"]
tags: ["lfe friday", "lfe", "erlang"]
published_date: 2015-10-18 14:51:00 +0000
is_draft: false
data:
  author: robert-virding
  written_for: null
  last_validated: null
  cover_image: "/images/fridays/LFE_Friday_00273_.png"
  cover_alt: "Vigdís — LFE Friday, LiffyBot studying Erlang modules in a Mead-style command room"
  math: false
---
<a href="/blog/assets/images/posts/LispFlavoredErlang-medium-square.png"><img class="left tiny" src="/blog/assets/images/posts/LispFlavoredErlang-medium-square.png" /></a>This week's LFE Friday was translated with permission from the
[Erlang Thursday](http://www.proctor-it.com/category/erlang/erlang-thursday/)
series by [Steven Proctor](https://twitter.com/stevenproctor).
*This week's translator*: Robert Virding.

Today's LFE Friday is on [erl_tar:table/1](http://erlang.org/doc/man/erl_tar.html#table-1).

``erl_tar:table/1`` returns a list of filenames included in the tar file.

```lfe
> (erl_tar:table "animal_sounds.tar")  
#(ok ("dog.txt" "cat.txt" "pony.txt" "bear.txt"))
```

There is also a version [erl_tar:table/2](http://erlang.org/doc/man/erl_tar.html#table-2) that takes a options list as well.

```lfe
> (erl_tar:table "animal_sounds.tar.gz" '(compressed)) 
#(ok ("dog.txt" "cat.txt" "pony.txt" "bear.txt"))
> (erl_tar:table "animal_sounds.tar.gz" '(compressed verbose))
#(ok
  (#("dog.txt" regular 5 #(#(2015 10 18) #(16 47 9)) 420 503 20)
   #("cat.txt" regular 5 #(#(2015 10 18) #(16 47 18)) 420 503 20)
   #("pony.txt" regular 8 #(#(2015 10 18) #(16 47 28)) 420 503 20)
   #("bear.txt" regular 19 #(#(2015 10 18) #(16 47 46)) 420 503 20)))
```

With the `verbose` option, instead of just getting a list of filenames, we get a list of tuples when we pass `verbose`.

The tuple is: file-name, file-type (regular file/directory or a symbolic link), bytes of the file, timestamp tuple, permissions (expressed in decimal instead of octal so 420 is #o644), user-id, and group-id.

The documentation does not specify any of the information about the return type, and the credit for clarification of what the `420 501 20` items represent is all from Robert Virding, from emailing him this post to be translated as part of <a href="http://blog.lfe.io/tags.html#lfe friday-ref" target="_blank">LFE Fridays</a>.

-Proctor, Robert
