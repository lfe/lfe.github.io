---
layout: post.liquid
title: "ledis: Using Redis from LFE"
description: ""
permalink: "/blog/tutorials/2015/04/05/1332-ledis-using-redis-from-lfe"
categories: ["tutorials"]
tags: ["howtos", "databases", "libraries", "redis", "nosql"]
published_date: 2015-04-05 13:32:00 +0000
is_draft: false
data:
  author: duncan-mcgreggor
  written_for: null
  last_validated: null
  cover_image: null
  cover_alt: null
  math: false
---
<a href="/blog/assets/images/posts/ButterCrunchLettuce-2-medium.png"><img class="right small" src="/blog/assets/images/posts/ButterCrunchLettuce-2-medium.png" /></a>
This tutorial is a conversion of the [Redis tutorial on data
types](http://redis.io/topics/data-types-intro), where instead of the Redis
CLI, the LFE REPL is used in conjunction with the
[ledis](https://github.com/lfex/ledis) library.

The ledis library has defined functions for the Redis commands most often used;
if we haven't added one that you need, just
[open a ticket](https://github.com/lfex/ledis/issues/new) with your request, or
even submit a pull request with your changes.

The original tutorial on Redis data types is a bit long for a blog post, so
we'll be splitting it up across multiple installments; this post covers the
following topics:

 * Installing ledis and running the LFE REPL
 * Redis Keys
 * Redis Strings
 * Altering and Querying the Key Space

Be sure to checkout the original tutorial for the full introduction; we're
going to skip that in this tutorial and jump right into the data types.

## Preparation

You will need:

* Redis installed on your machine
* Erlang
* The latest dev versions of ``lfetool``
* The [ledis project](https://github.com/lfex/ledis)

LFE and ledis' dependencies will all be downloaded for you automatically when
you start up the project REPL for the first time (see below). This tutorial
will not cover the installation of Redis or Erlang. To install the development
version of ``lfetool``, simply do the following:

```bash
$ curl -L -o ./lfetool https://raw.github.com/lfe/lfetool/dev-v1/lfetool
$ bash ./lfetool install
```

This will create an executable ``lfetool`` in your ``/usr/local/bin``
directory.

Next you'll need to get ledis itself:

```bash
$ git clone https://github.com/lfex/ledis.git
$ cd ledis
```

We're going to edit the ``lfe.config`` file
so that all results are automatically converted from binary to strings (just
for the purpose of this tutorial). Open up the file, and change this line:

```lfe
#(return-type binary)))
```

to this:

```lfe
#(return-type string)))
```

Now go ahead and start the LFE REPL. The first time you run this, all the
dependencies will be downloaded and compiled. Note that the project's ``make``
targets automatically start the ledis application, so there's no need to type
``(ledis:start)``:

```bash
$ make repl
```
```lfe
Erlang/OTP 17 [erts-6.3] [source] [64-bit] [smp:4:4] ...

LFE Shell V6.3 (abort with ^G)
>
```

At this point, we're ready to start the tutorial!


## Redis Keys

Redis keys are binary safe, this means that you can use any binary sequence as
a key, from a string like "foo" to the content of a JPEG file. The empty string
is also a valid key.

A few other rules about keys:

* Very long keys are not a good idea, for instance a key of 1024 bytes is a bad
  idea not only memory-wise, but also because the lookup of the key in the
  dataset may require several costly key-comparisons. Even when the task at
  hand is to match the existence of a large value, to resort to hashing it (for
  example with SHA1) is a better idea, especially from the point of view of
  memory and bandwidth.
* Very short keys are often not a good idea. There is little point in writing
  "u1000flw" as a key if you can instead write "user:1000:followers". The
  latter is more readable and the added space is minor compared to the space
  used by the key object itself and the value object. While short keys will
  obviously consume a bit less memory, your job is to find the right balance.
* Try to stick with a schema. For instance "object-type:id" is a good idea, as
  in "user:1000". Dots or dashes are often used for multi-word fields, as in
  "comment:1234:reply.to" or "comment:1234:reply-to".
* The maximum allowed key size is 512 MB.

## Redis Strings

The Redis String type is the simplest type of value you can associate with a
Redis key. It is the only data type in Memcached, so it is also very natural
for newcomers to use it in Redis.

Since Redis keys are strings, when we use the string type as a value too, we
are mapping a string to another string. The string data type is useful for a
number of use cases, like caching HTML fragments or pages.

Let's play a bit with the string type, using the LFE REPL and the ledis
library.

```lfe
> (ledis:set 'mykey "somevalue")
#(ok "OK")
> (ledis:get 'mykey)
#(ok "somevalue")
```

As you can see using the ``set`` and the ``get`` functions are the way we set
and retrieve a string value. Note that ``set`` will replace any existing value
already stored into the key, in the case that the key already exists, even if
the key is associated with a non-string value. So ``set`` performs an
assignment.  Values can be strings (including binary data) of every kind, for
instance you can store a jpeg image inside a key. A value can't be bigger than
512 MB.

The ``set`` function has interesting options, that are provided as additional
arguments. For example, you may ask ``set`` to fail if the key already exists:

```lfe
> (ledis:set 'mykey "anothervalue" '(#(nx)))
#(ok undefined)
> (ledis:get 'mykey)
#(ok "somevalue")
```

Note that when ``undefined`` is returned, this is a translation of Redis'
``nil`` response (for a command that had no results or made no changes). Also,
it still has the old value (as expected).

You can also do the opposite: ask that it only succeed if the key already
exists:

```lfe
> (ledis:set 'mykey "anothervalue" '(#(xx)))
#(ok "OK")
> (ledis:get 'mykey)
#(ok "anothervalue")
```

Set has additional options, allowing one to expire a value from Redis:

```lfe
> (ledis:set 'mykey "athirdvalue" '(#(xx) #(px 10000)))
#(ok "OK")
> (ledis:get 'mykey)
#(ok "athirdvalue")
> (timer:sleep 10000)
ok
> (ledis:get 'mykey)
#(ok undefined)
```
In that example, we did the following:

1. Set a value for ``mykey`` -- but only if it already existed -- and set its
   expiration for 10,000 milliseconds.
1. Checked the value, to show that our function all worked.
1. Set the timer for a time when the value would be expired.
1. Checked that the value did in fact expire.

Even if strings are the basic values of Redis, there are interesting operations
you can perform with them. For instance, one is atomic increment:

```lfe
> (ledis:set 'counter 100)
#(ok "OK")
> (ledis:incr 'counter)
#(ok "101")
> (ledis:incr 'counter)
#(ok "102")
> (ledis:incrby 'counter 50)
#(ok "152")
```

The ``incr`` function parses the string value as an integer, increments it by
one, and finally sets the obtained value as the new value. There are other
similar functions like ``incrby``, ``decr`` and ``decrby``. Internally it's
always the same Redis command, acting in a slightly different way.

What does it mean that ``incr`` is atomic? That even multiple clients issuing
``incr`` against the same key will never enter into a race condition. For
instance, it will never happen that client 1 reads "10", client 2 reads "10" at
the same time, both increment to 11, and set the new value to 11. The final
value will always be 12 and the read-increment-set operation is performed while
all the other clients are not executing a command against the Redis server at
the same time.

There are a number of functions for operating on strings. For example the
``getset`` function sets a key to a new value, returning the old value as the
result. You can use this function, for example, if you have a system that
increments a Redis key using ``incr`` every time your web site receives a new
visitor. You may want to collect this information once every hour, without
losing a single increment.  You can ``getset`` the key, assigning it the new
value of "0" and reading the old value back.

The ability to set or retrieve the value of multiple keys in a single function
is also useful for reduced latency. For this reason there are the
``multi-set`` and ``multi-get`` ledis functions (which map to the ``MSET`` and
``MGET`` Redis commands):

```lfe
> (ledis:multi-set '(a 10 b 20 c 30))
#(ok "OK")
> (ledis:multi-get '(a b c))
#(ok ("10" "20" "30"))
```

Note that, since LFE already has built-in functions named ``mset`` and ``mget``,
the Redis command names could not be used.


## Altering and Querying the Key Space

There are functions that are not defined on particular types, but are useful in
order to interact with the space of keys, and thus, can be used with keys of
any type.

For example the ``exists`` function returns 1 or 0 to signal if a given key
exists or not in the database, while the ``del`` function deletes a key and
associated value, whatever the value is.

```lfe
> (ledis:set 'mykey "Hello")
#(ok "OK")
> (ledis:exists 'mykey)
#(ok "1")
> (ledis:del 'mykey)
#(ok "1")
> (ledis:exists 'mykey)
#(ok "0")
```

From the examples you can also see how ``del`` itself returns 1 or 0 depending
on whether the key was removed (it existed) or not (there was no such key with
that name). You may also delete multiple keys in one go:

```lfe
> (ledis:multi-set '(key1 "val1" key2 "val2" key3 "val3"))
#(ok "OK")
> (ledis:multi-get '(key1 key2 key3))
#(ok ("val1" "val2" "val3"))
> (ledis:del '(key1 key2 key3 key4))
#(ok "3")
```

In this case, the return value for ``del`` is the number of successful deletes:
we asked it to delete four keys, but the fourth doesn't exist, so it only
deleted three.

There are many key space related commands, but the above two are the essential
ones together with the ``type`` function, which returns the kind of value
stored at the specified key:

```lfe
> (ledis:set 'mykey 1)
#(ok "OK")
> (ledis:type 'mykey)
#(ok "string")
> (ledis:del 'mykey)
#(ok "1")
> (ledis:type 'mykey)
#(ok "none")
```

## More Redis

As progress is made on the LFE library, we'll be posting more installments of
the [Redis tutorial](http://redis.io/topics/data-types-intro) here. Expect to
see the following in the coming weeks and months:

* Redis Lists
* Redis Hahes
* Redis Sorted Sets (including ranges and lexicographical scores)
* Bitmaps
* Probabilistic Data Structure


