---
layout: post.liquid
title: "LFE Friday - httpc:request/1 and httpc:request/4"
description: ""
permalink: "/blog/tutorials/2015/01/22/2243-lfe-friday---httpcrequest1-and-httpcrequest4"
categories: ["tutorials"]
tags: ["lfe-friday", "lfe", "erlang"]
published_date: 2015-01-22 22:43:00 +0000
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

Today's LFE Friday is on [httpc:request/1](http://www.erlang.org/doc/man/httpc.html#request-1) and [httpc:request/4](http://www.erlang.org/doc/man/httpc.html#request-4). The httpc module is Erlang’s HTTP 1.1 client, and the ``request`` function is a powerful way to make web requests using Erlang.

To start using the httpc module, you first have to make sure ``inets`` has been started.

```lfe
> (inets:start)
ok
```

``httpc:request/1`` takes one argument, and that is the URL, as a Erlang string, you want to make the request against.

```lfe
> (httpc:request "http://www.example.com")
#(ok
  #(#("HTTP/1.1" 200 "OK")
    (#("cache-control" "max-age=604800")
     #("date" "Thu, 22 Jan 2015 21:52:42 GMT")
     #("accept-ranges" "bytes")
     #("etag" "\"359670651\"")
     #("server" "ECS (ewr/1584)")
     #("content-length" "1270")
     #("content-type" "text/html")
     #("expires" "Thu, 29 Jan 2015 21:52:42 GMT")
     #("last-modified" "Fri, 09 Aug 2013 23:54:35 GMT")
     #("x-cache" "HIT")
     #("x-ec-custom-error" "1"))
    "<!doctype html>\n<html>\n<head>\n    <title>Example Domain</title>\n\n    <meta charset=\"utf-8\" />\n    <meta http-equiv=\"Content-type\" content=\"text/html; charset=utf-8\" />\n    <meta name ..."))
```

``httpc:request/1`` is the equivalent of the ``httpc:request/4`` function called as ``(httpc:request 'get (tuple url ()) () ())``.

```lfe
> (httpc:request 'get #("http://www.example.com" ()) () ())
#(ok
  #(#("HTTP/1.1" 200 "OK")
    (#("cache-control" "max-age=604800")
     #("date" "Thu, 22 Jan 2015 21:55:34 GMT")
     #("accept-ranges" "bytes")
     #("etag" "\"359670651\"")
     #("server" "ECS (ewr/1584)")
     #("content-length" "1270")
     #("content-type" "text/html")
     #("expires" "Thu, 29 Jan 2015 21:55:34 GMT")
     #("last-modified" "Fri, 09 Aug 2013 23:54:35 GMT")
     #("x-cache" "HIT")
     #("x-ec-custom-error" "1"))
    "<!doctype html>\n<html>\n<head>\n    <title>Example Domain</title>\n\n    <meta charset=\"utf-8\" />\n    <meta http-equiv=\"Content-type\" content=\"text/html; charset=utf-8\" />\n    <meta name ..."))
```

You can specify headers as part of your request. For example, say we want to get DuckDuckGo's page in Swedish in honor of Erlang being created by Ericsson. To do that, we add a tuple of ``#("Accept-Language" "sv")`` to the headers list as part of the request.

```lfe
> (httpc:request 'get #("http://duckduckgo.com" (#("Accept-language" "sv"))) () ())
#(ok
  #(#("HTTP/1.1" 200 "OK")
    (#("cache-control" "max-age=1")
     #("connection" "keep-alive")
     #("date" "Thu, 22 Jan 2015 21:58:14 GMT")
     #("accept-ranges" "bytes")
     #("etag" "\"54c126fc-1488\"")
     #("server" "nginx")
     #("content-length" "5256")
     #("content-type" "text/html; charset=UTF-8")
     #("expires" "Thu, 22 Jan 2015 21:58:15 GMT"))
    "<!DOCTYPE html>\n<!--[if IEMobile 7 ]> <html lang=\"sv_SE\" class=\"no-js iem7\"> <![endif]-->\n<!--[if lt IE 7]> <html class=\"ie6 lt-ie10 lt-ie9 lt-ie8 lt-ie7 no-js\" lang=\"sv_SE\"> <![endif]-->\n<!--[if IE 7]>    <html class=\"ie7 lt-ie10 lt-ie9 lt-ie8 no-js\" lang=\"sv_SE\"> <![endif]-->\n<!-- ..."))
```

The third argument of ``httpc:request/4`` is a list of HTTP option tuples. For example, you need to set timeouts on the response in order to avoid waiting on a response from an unresponsive or slow website because if it doesn't respond in time, the requesting code needs to back off and try again later to avoid triggering the equivalent of a Denial of Service attack. In this case, I am specifying a timeout of 0, expressed in milliseconds, to ensure a timeout happens for illustrative purposes.

```lfe
> (httpc:request 'get #("http://erlang.org" ()) '(#(timeout 0)) ())
#(error
  #(failed_connect (#(to_address #("erlang.org" 80)) #(inet (inet) timeout))))
```

As it's final argument, ``httpc:request/4`` takes a list of other options, these options are for how the Erlang side of things should work. Maybe you want to make a request asynchronously, and want to receive a message when it is complete. To do that you can specify an option tuple of ``#(sync false)``.

```lfe
> (set `#(ok ,requestid) (httpc:request 'get '#("http://example.com" ()) () '(#(sync false))))
#(ok #Ref<0.0.0.87>)
> (receive (`#(http #(,requestid ,result)) result) (after 500 -> error))
#(#("HTTP/1.1" 200 "OK")
  (#("cache-control" "max-age=604800")
   #("date" "Thu, 22 Jan 2015 22:06:01 GMT")
   #("accept-ranges" "bytes")
   #("etag" "\"359670651\"")
   #("server" "ECS (phl/9D2C)")
   #("content-length" "1270")
   #("content-type" "text/html")
   #("expires" "Thu, 29 Jan 2015 22:06:01 GMT")
   #("last-modified" "Fri, 09 Aug 2013 23:54:35 GMT")
   #("x-cache" "HIT")
   #("x-ec-custom-error" "1"))
  #B("<!doctype html>\n<html>\n<head>\n    <title>Example Domain</title>\n\n    <meta "...))
```

Or maybe you want to get the response body back as an Erlang binary instead of a string.

```lfe
> (httpc:request 'get '#("http://example.com" ()) () '(#(body_format binary)))
#(ok          
  #(#("HTTP/1.1" 200 "OK")
    (#("cache-control" "max-age=604800")
     #("date" "Thu, 22 Jan 2015 22:08:20 GMT")
     #("accept-ranges" "bytes")
     #("etag" "\"359670651\"")
     #("server" "ECS (ewr/1584)")
     #("content-length" "1270")
     #("content-type" "text/html")
     #("expires" "Thu, 29 Jan 2015 22:08:20 GMT")
     #("last-modified" "Fri, 09 Aug 2013 23:54:35 GMT")
     #("x-cache" "HIT")
     #("x-ec-custom-error" "1"))
    #B("<!doctype html>\n<html>\n<head>\n    <title>Example Domain</title>\n\n    <meta "...)))
```

This post just scratches the surface of what you can do with ``httpc:request/4``, and I highly recommend checking out the Erlang documentation for the [httpc module](http://www.erlang.org/doc/man/httpc.html). For more examples and information, also check out the [Erlang inets User Guide](http://www.erlang.org/doc/apps/inets/users_guide.html), and the chapter ["HTTP Client"](http://www.erlang.org/doc/apps/inets/http_client.html).

–Proctor, Robert
