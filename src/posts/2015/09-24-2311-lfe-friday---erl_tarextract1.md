---
layout: post.liquid
title: "LFE Friday - erl_tar:extract/1"
description: ""
permalink: "/blog/tutorials/2015/09/24/2311-lfe-friday---erl_tarextract1"
categories: ["tutorials"]
tags: ["lfe-friday", "lfe", "erlang"]
published_date: 2015-09-24 23:11:00 +0000
is_draft: false
data:
  author: robert-virding
  written_for:
    lfe: "0.10"
    erlang: "18"
  last_validated: null
  cover_image: "/images/fridays/LFE_Friday_00282_.png"
  cover_alt: "Vigdís — LFE Friday, LiffyBot studying Erlang modules in a Mead-style command room"
  math: false
---
<a href="/blog/assets/images/posts/LispFlavoredErlang-medium-square.png"><img class="left tiny" src="/blog/assets/images/posts/LispFlavoredErlang-medium-square.png" /></a>This week's LFE Friday was translated with permission from the
[Erlang Thursday](http://www.proctor-it.com/category/erlang/erlang-thursday/)
series by [Steven Proctor](https://twitter.com/stevenproctor).
*This week's translator*: Robert Virding.

Today's LFE Friday cover's [erl_tar:extract/1](http://www.erlang.org/doc/man/erl_tar.html#extract-1).

``erl_tar:extract/1`` takes a file, either as a binary tuple, file descriptor tuple, or filename, and extracts the contents of the tar out to the current directory.

Since we will need to have a tar file to extract, let's create some files and add them to a new tar file.

```sh
$ echo "woof" > dog.txt
$ echo "meow" > cat.txt
$ echo "sparkle" > pony.txt
$ echo 'Wocka Wocka Wocka!' > bear.txt
$ tar -cvf animal_sounds.tar dog.txt cat.txt pony.txt bear.txt
a dog.txt
a cat.txt
a pony.txt
a bear.txt
```

And while we are at it, lets create a compressed version as well.

```sh
$ tar -cvzf animal_sounds.tar.gz dog.txt cat.txt pony.txt bear.txt
a dog.txt
a cat.txt
a pony.txt
a bear.txt
```

Since we are going to test out extracting the tar, we will go ahead and clean up the files that we put in the tar.

```sh
$ rm dog.txt cat.txt pony.txt bear.txt
```

With all the ceremony of making sure we have a tar file to experiment with out of the way, it is time to fire up our LFE REPL, and call ``erl_tar:extract/1``.

```lfe
> (erl_tar:extract "animal-sounds.tar")
ok
```

That seemed straight forward enough, so let's see if we have our files extracted back out at the command prompt.

```sh
$ ls dog.txt cat.txt pony.txt bear.txt
bear.txt cat.txt  dog.txt  pony.txt
$ rm dog.txt cat.txt pony.txt bear.txt
```

And since we saw them, we will go ahead and remove them to get back to a clean state.

Erlang also has a [erl_tar:extract/2](http://www.erlang.org/doc/man/erl_tar.html#extract-2), which allows us to give options to the extraction process, by passing a list as its second argument.

We can have ``erl_tar:extract/2`` extract the files and tell it to be ``verbose``, and then follow that up with another extraction, where we specify that we not only want it to be verbose, but don't overwrite any files that are already there.

```lfe
> (erl_tar:extract "animal-sounds.tar" '(verbose))
x /Users/rv/lfe/blog/tmp/dog.txt

x /Users/rv/lfe/blog/tmp/cat.txt

x /Users/rv/lfe/blog/tmp/pony.txt

x /Users/rv/lfe/blog/tmp/bear.txt

ok
> (erl_tar:extract "animal-sounds.tar" '(verbose keep_old_files))
x /Users/rv/lfe/blog/tmp/dog.txt - exists, not created

x /Users/rv/lfe/blog/tmp/cat.txt - exists, not created

x /Users/rv/lfe/blog/tmp/pony.txt - exists, not created

x /Users/rv/lfe/blog/tmp/bear.txt - exists, not created

ok
```

And yet again, we swing back to the command prompt to remove the extracted files.

```sh
$ rm dog.txt cat.txt pony.txt bear.txt
```

Next we extract `animal_sounds.tar.gz` by passing the atom `compressed` in the list of options.

```lfe
(erl_tar:extract "animal-sounds.tar.gz" '(verbose compressed keep_old_files))
x /Users/rv/lfe/blog/tmp/dog.txt

x /Users/rv/lfe/blog/tmp/cat.txt

x /Users/rv/lfe/blog/tmp/pony.txt

x /Users/rv/lfe/blog/tmp/bear.txt

ok
```

And sometimes when working with a tar file in your program, you don't want to have to do all the management of the files on the filesystem just to read the contents of a tar file, so there is even an option to keep it all in memory.

```lfe
> (erl_tar:extract "animal-sounds.tar.gz" '(verbose compressed keep_old_files memory))
#(ok
  (#("dog.txt" #"woof\n")
   #("cat.txt" #"meow\n")
   #("pony.txt" #"sparkle\n")
   #("bear.txt" #"Wocka Wocka Wocka!\n")))
```

When passing the ``memory`` option, the return value of ``erl_tar:extract/2`` becomes an tuple of the status, and a list of tuples composed of the filename, and the contents of the file as a binary for each file in the tar that was extracted. As the contents of the file were legal Unicode characters the results were printed as binary strings.

If an error occurs on extraction to memory, for example we forget to pass the compressed option to a compressed tar file, it returns an ``error`` tuple.

```lfe
> (erl_tar:extract "animal-sounds.tar.gz" '(verbose memory))
#(error eof)
```

There are quite a bit more options that ``erl_tar:extract/2`` can take as well, so we highly recommend checking out the documentation for the full list of options.

-Proctor, Robert
