---
layout: post.liquid
title: "LFE Friday - erl_tar:create/2"
description: ""
permalink: "/blog/tutorials/2015/09/18/0131-lfe-friday---erl_tarcreate2"
categories: ["tutorials"]
tags: ["lfe-friday", "lfe", "erlang"]
published_date: 2015-09-18 01:31:00 +0000
is_draft: false
data:
  author: robert-virding
  written_for: null
  last_validated: null
  cover_image: "/images/fridays/LFE_Friday_00272_.png"
  cover_alt: "Vigdís — LFE Friday, LiffyBot studying Erlang modules in a Mead-style command room"
  math: false
---
<a href="/blog/assets/images/posts/LispFlavoredErlang-medium-square.png"><img class="left tiny" src="/blog/assets/images/posts/LispFlavoredErlang-medium-square.png" /></a>This week's LFE Friday was translated with permission from the
[Erlang Thursday](http://www.proctor-it.com/category/erlang/erlang-thursday/)
series by [Steven Proctor](https://twitter.com/stevenproctor).
*This week's translator*: Robert Virding.

Today's LFE Friday is on [erl_tar:create/2](http://erlang.org/doc/man/erl_tar.html#create-2).

``erl_tar:create/2`` creates a tar file with a given filename and adds the given list of filenames to the archive.

``erl_tar:create/2`` takes two arguments, the first is a filename to write to, and the second argument is a list of filenames to add to the tar file.

First, we will open up a new OS shell session and create some files to add to a new tar file.

```sh
$ echo "foo" > foo.txt
$ echo "bar" > bar.txt
$ echo "baz" > baz.txt
$ ls 
bar.txt  baz.txt  foo.txt
```

Now that we have some files to archive, we can open up a new ``lfe`` session, and create a new tar file named ``test.tar``.

```lfe
> (erl_tar:create "test.tar" '("foo.txt" "bar.txt" "baz.txt"))
ok
```

That looks like it worked; so let's go to an OS shell, and inspect the resulting file for the filename we gave to ``erl_tar:create/3``.

```sh
$ tar -tf test.tar
foo.txt
bar.txt
baz.txt
```

And yes, ``tar`` can read that file and tells us that the three files we added are indeed part of the tar file.

Erlang also provides [erl_tar:create/3](http://erlang.org/doc/man/erl_tar.html#create-3) that takes a options list as it's last argument.

We will create a new file, with the same contents, and pass in that we want this tar file to be compressed, and to be verbose with what it is doing as well.
	
```lfe
> (erl_tar:create "options.tar.gz" '("foo.txt" "bar.txt" "baz.txt") '(compressed verbose))
a foo.txt
a bar.txt
a baz.txt
ok
```

Again, let's switch back to our OS shell, and inspect the resulting file.

```sh
$ tar -tf options.tar.gz
foo.txt
bar.txt
baz.txt
```

And let's test it to see if it was considered compressed by `gzip`.

```bash
$ gzip --test options.tar.gz
$ 
```

And there we go, `gzip` considers this a compressed file with integrity.  So let's take a look at the size difference between the two tar files we created.

```sh
$ ls -l test.tar options.tar.gz
-rw-r--r--  1 rv  staff    158 Sep 18 01:43 options.tar.gz
-rw-r--r--  1 rv  staff  10240 Sep 18 01:43 test.tar
```

And looking at the filesize we can see that it is definitely compressed, as ``options.tar.gz`` is two orders of magnitude smaller than ``test.tar``.

As we just created ``test.tar`` and saw it had the contents, let's see what happens when we call create on a file that already exists, by passing the same filename with a empty list of files.

```lfe
> (erl_tar:create "test.tar" ())
ok      
```

And we take a look at the contents, we can see the original tar has been replaced.

```sh
$ tar -tf test.tar
$ 
```

This tells us that ``erl_tar:create/2`` will create a tar file and overwrite the existing file, and doesn't error out if the file already exists (assuming the user the shell is running has access to write to that file/directory).

If we give a bad path for a file, we can see that ``erl_tar:create/2`` will return a error tuple, with the filename and reason for the failure.

```lfe
> (erl_tar:create "/path/does/not/exist" ())
#(error #("/path/does/not/exist" enoent))
```

There a few potential gotchas to be aware of. First, the documentation states that it takes ``filename()``s as arguments, but the documentation page for ``erl_tar`` does not specify on that page what a filename data type is.

If you use ``atom()``s for the filename, you are going to get an error like the one below that I was getting at first, before using ``string()``s for the filenames.

```lfe
> (erl_tar:create 'test.tar '(foo.txt bar.txt baz.txt))
exception error: function_clause
  in (: filename join ())
  in erl_tar:split_filename/4 (erl_tar.erl, line 471)
  in erl_tar:create_header/3 (erl_tar.erl, line 400)
  in erl_tar:add1/4 (erl_tar.erl, line 323)
  in erl_tar:foreach_while_ok/2 (erl_tar.erl, line 1000)
  in erl_tar:create/3 (erl_tar.erl, line 132)
```

Second, according to the [Limitations](http://erlang.org/doc/man/erl_tar.html#id180770) section of the erl_tar documentation page, filenames should be less than 100 characters for maximum compatibility across different systems and version of the tar program.

Lastly, it is on us the user to include the file extension when specifying the filename, as ``erl_tar:create/2`` does not manage the extension for us.

-Proctor, Robert
