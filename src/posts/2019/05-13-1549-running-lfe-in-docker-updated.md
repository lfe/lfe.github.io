---
layout: post.liquid
title: "Running LFE in Docker, Updated"
description: "Significant changes have been made to the LFE Docker support of late, least of which is using the official Erlang Docker images as the base."
permalink: "/blog/tutorials/2019/05/13/1549-running-lfe-in-docker-updated"
categories: [tutorials]
tags: [docker, paas, howtos, web, apps, ops, lfest, exemplar]
published_date: 2019-05-13 15:49:00 +0000
is_draft: false
data:
  author: duncan-mcgreggor
  written_for:
    lfe: "1.3"
    erlang: "21"
  last_validated: null
  cover_image: "/images/LFE_Docker_00263_.png"
  cover_alt: "Vigdís — LiffyBot at a Docker console, Mead-style server room"
  math: false
---

<a href="/blog/assets/images/posts/DockerLogo.png"><img class="right thumb" src="/blog/assets/images/posts/DockerLogo.png" /></a>So much
changed since the [original version of this tutorial](/blog/tutorials/2014/12/07/1837-running-lfe-in-docker) was created, back in 2014 -- not the least of which is [Docker](https://www.docker.com/)'s 
continued rise in the industry. There have been many new releases of Erlang and there is an actively
supported Dockerhub repo of Erlang Docker images. Most significantly for LFE users, though, are the 
following changes:

* The [LFE images](https://hub.docker.com/r/lfex/lfe/tags) now use the 
  [official Erlang Docker images](https://hub.docker.com/_/erlang?tab=tags) as their base
* As such, the only two distributions supported are Debign and Alpine (OpenSUSE, Fedora, 
  Tinycore, etc., are no longer supported)
* The LFE images now use `ENTRYPOINT`, allowing them to be easily used interchangably with 
  on-system `lfe` binary

To that last point, this means that when you see an example calling the `lfe` binary, you'll be able to 
use that with the LFE Docker images by interchanging that with a call to `docker run` instead (details on
that below).

This post not only is an update to the 2014 LFE/Docker blog post, it's an update to the 
[2015 LFE/YAWS/Docker post](/blog/tutorials/2015/11/28/2110-lfe-yaws-docker-update/).
Content is taken from those as well as the current 
[lfex/dockerfiles README](https://github.com/lfex/dockerfiles).

## Setup

This tutorial assumes that you have [Docker](https://www.docker.com/) installed on a host machine.

You don't need LFE or Erlang installed on your system to do the rest of this tutorial.

## Images

Since they are based upon the Erlang Docker images, the supported LFE image distros are as 
follows:

- standard (Debian, [buildpack-deps](https://hub.docker.com/_/buildpack-deps/)
  based images)
- slim (Debian based images with only what Erlang requires)
- alpine

These include the following LFE versions:

- 1.3-dev (with some additional features from still-open PRs)

Dependeing upon image type, some or all of the following Erlang versions are
available:

- 17.5 (only with standard image type)
- 18.3 (standard and slim)
- 19.3 (standard and slim)
- 20.3 (all)
- 21.3 (all)

The LFE images are published with tags in the following format:

```
[org]/[project]:[lfe-version]-[erlang-version]-[image-type]
```

For example, LFE v1.3 running on Erlang 20.3 in the Debian-based slim container would be:

```
lfex/lfe:1.3-20.3-slim
```

Note that the Alpine image is considered the canonical one, thus the `latest`
tag is against an Alpine image with the most recent release of LFE and Erlang.
If this is what you want, than simply using either of the following will pull
this down:

```
$ docker run -it lfex/lfe:latest
```

or

```
$ docker run -it lfex/lfe
```

Which, as noted, is currently the same as `lfex/lfe:1.3-21.3-alpine`.

To help make your image selection process better informed, the following table is
given in the README, comparing the LFE and Erlang image types and their sizes:

| REPOSITORY | TAG               | SIZE   |
|------------|-------------------|--------|
| lfex/lfe   | latest            | 80.1MB |
| lfex/lfe   | 1.3-21.3-alpine   | 80.1MB |
| lfex/lfe   | 1.3-20.3-alpine   | 84MB   |
| lfex/lfe   | 1.3-21.3-slim     | 258MB  |
| lfex/lfe   | 1.3-20.3-slim     | 266MB  |
| lfex/lfe   | 1.3-19.3-slim     | 523MB  |
| lfex/lfe   | 1.3-18.3-slim     | 277MB  |
| lfex/lfe   | 1.3-21.3-standard | 1.07GB |
| lfex/lfe   | 1.3-20.3-standard | 1.08GB |
| lfex/lfe   | 1.3-19.3-standard | 1.1GB  |
| lfex/lfe   | 1.3-18.3-standard | 1.1GB  |
| lfex/lfe   | 1.3-17.5-standard | 753MB  |

| REPOSITORY | TAG               | SIZE   |
|------------|-------------------|--------|
| erlang     | 21.3-alpine       | 73.3MB |
| erlang     | 20.3-alpine       | 77.2MB |
| erlang     | 21.3-slim         | 251MB  |
| erlang     | 20.3-slim         | 258MB  |
| erlang     | 19.3-slim         | 515MB  |
| erlang     | 18.3-slim         | 270MB  |
| erlang     | 21.3              | 1.07GB |
| erlang     | 20.3              | 1.07GB |
| erlang     | 19.3              | 1.09GB |
| erlang     | 18.3              | 1.09GB |
| erlang     | 17.5              | 746MB |


## Usage

We're going to look at a handful of different usages of the LFE Docker images in the 
following sections:

* As a means of running the LFE REPL
* Running examples with the image's `lfe` executable as the entry point
* Running script examples as entry points
* A custom app, ready to deploy to a Docker container hosting service


### Instant REPL

Running an LFE REPL in any of the provided images is as simple as the following:

```
$ docker run -it lfex/lfe
```

```
Erlang/OTP 21 [erts-10.3.5] [source] [64-bit] [smp:6:6] [ds:6:6:10] [async-threads:1] [hipe]

   ..-~.~_~---..
  (      \\     )    |   A Lisp-2+ on the Erlang VM
  |`-.._/_\\_.-':    |   Type (help) for usage info.
  |         g |_ \   |
  |        n    | |  |   Docs: http://docs.lfe.io/
  |       a    / /   |   Source: http://github.com/rvirding/lfe
   \     l    |_/    |
    \   r     /      |   LFE v1.3-dev (abort with ^G)
     `-E___.-'

lfe>
```

### Running Examples

#### Modules via CLI

Some of the LFE example modules have been compiled in these Docker images for
your testing convenience. How they are run depends upon each example. For
instance, here's how to run the LFE port of the classic Erlang "ring" example:

```
$ docker run lfex/lfe -pa examples/ebin -noshell -run ring main 503 50000000
```

Note that, because these Docker images use `ENTRYPOINT`, they can be run just
like you run the installed `lfe` binary on a system, complete with command line
flags. The only difference is that instead of typing `lfe` in the terminal,
we type `docker run lfex/lfe:1.3-20.3-alpine`.

This will run for a while, then you'll get the expected output:

```
Result: 292
```

Another example, based on
http://joearms.github.io/2013/11/21/My-favorite-erlang-program.html, will take
_quite_ a long while to finish:

```
$ docker run lfex/lfe:1.3-18.3-slim -pa examples/ebin -noshell -run joes-fav run-it
```

```
30414093201713378043612608166064768844377641568960512000000000000
```

#### Interactive Modules via `main`

For interactive modules where you don't need the LFE prompt:

```
$ docker run -i lfex/lfe \
  -pa examples/ebin -noshell -run guessing-game main
```

```
Guess the number I have chosen, between 1 and 10.
Guess number: 1
Your guess is too low.
Guess number: 10
Your guess is too high.
Guess number: 5
Well-guessed!!
```

#### Precompiled Modules via LFE REPL

Another pre-compiled module in the Docker images is the one demonstrating
Church numerals in LFE. To use it, you just need to include the `examples/ebin`
in the Elrang modules path:

```
$ docker run -it lfex/lfe:latest -pa examples/ebin
```

```lfe
lfe> (church:one)
#Fun<church.1.125931718>
lfe> (church:get-church 10)
#Fun<church.7.125931718>
lfe> (church:church->int1 (church:get-church 20))
20
```

Another pre-compiled example, utilizing Erlang inboxes and message-passing:

```lfe
lfe> (messenger-back:send-message (self) "Well, I was able to extend the original entry a bit, yes.")
#(#Pid<0.80.0> "Well, I was able to extend the original entry a bit, yes.")
Received message: 'Well, I was able to extend the original entry a bit, yes.'
Sending message to process <0.80.0> ...
lfe> (messenger-back:send-message (self) "And what does it say now?")
#(#Pid<0.80.0> "And what does it say now?")
Received message: 'And what does it say now?'
Sending message to process <0.80.0> ...
lfe> (messenger-back:send-message (self) "Mostly harmless.")
#(#Pid<0.80.0> "Mostly harmless.")
Received message: 'Mostly harmless.'
Sending message to process <0.80.0> ...
```

Then, we can flush the REPL process' inbox to see all the messages it has
received:

```lfe
(c:flush)
Shell got {"Well, I was able to extend the original entry a bit, yes."}
Shell got {"And what does it say now?"}
Shell got {"Mostly harmless."}
ok
```

#### Slurping Modules via LFE REPL

If we wanted to run one of the LFE examples that is not pre-compiled (or, as
is the case for the following example, run code that is not meant to be
compiled, but instead simply run in a REPL session), we can just use `slurp`.
Here's the General Problem Solver LFE example using this approach:

```
$ docker run -it lfex/lfe
```

```lfe
lfe> (slurp "examples/gps1.lfe")
```

```lfe
#(ok gps1)
```

```lfe
lfe> (gps '(son-at-home car-needs-battery have-money have-phone-book)
lfe>      '(son-at-school)
lfe>      (school-ops))
```

```
executing 'look-up-number'
executing 'telephone-shop'
executing 'tell-shop-problem'
executing 'give-shop-money'
executing 'shop-installs-battery'
executing 'drive-son-to-school'
solved
```

#### Scripts

You can also run the LFE example scripts by changing the entry point:

```
$ docker run --entrypoint=examples/sample-lfescript lfex/lfe:1.3-20.3-standard
```

This will give us an error, since we didn't pass it the correct argument type:

```
usage: examples/sample-lfescript <integer>
```

Now that we know what to do, thanks to the usage message, let's try again:

```
$ docker run --entrypoint=examples/sample-lfescript lfex/lfe:1.3-20.3-slim 10
```

```
factorial 10 = 3628800
```

Or another script example:

```
$ docker run --entrypoint=examples/sample-lfe-shellscript lfex/lfe 5
```

```
factorial 5 = 120
```


## A Simple LFE/YAWS Web App Image

Let's go nuts!

```bash
$ mkdir /tmp/sample-app
$ cd /tmp/sample-app
$ vi Dockerfile
```

```dockerfile
FROM lfex/lfe

ENV APP_DIR /opt/sample-app
ENV APP_REPO https://github.com/oubiwann/docker-lfe-yaws-sample-app.git
ENV DEPS_DIR $APP_DIR/deps
ENV YAWS_DIR $DEPS_DIR/yaws
ENV YAWS_APP_ID sampleapp
ENV LFE_DEPS $DEPS_DIR/lutil:$DEPS_DIR/exemplar:$DEPS_DIR/lfest
ENV DEPS $YAWS_DIR:$LFE_DEPS:$DEPS_DIR/ibrowse
ENV ERL_LIBS $ERL_LIBS:/opt/erlang/lfe:$DEPS

RUN apt-get update && apt-get install -y \
        libpam0g-dev

RUN git clone $APP_REPO $APP_DIR && \
        cd $APP_DIR && \
        rebar compile

EXPOSE 5099

CMD sh -c "/opt/sample-app/bin/daemon;while true; do sleep 10; done"
```

```vim
:x
```

```bash
$ docker build .
...
Successfully built 1904cb1d856b
```

Now we can tag it:

```bash
$ docker tag 1904cb1d856b sample-app
```

And then run it!

```bash
$ docker run -d -p 5099:5099 sample-app
1b21f79c42ec84cec841b25ff17e921b4d4ce8ee247c5293d5c0d73766a4c713
```

Let's make sure that it's running and that our port is exposed:

```bash
$ docker ps
CONTAINER ID   IMAGE               COMMAND                STATUS         PORTS
1b21f79c42ec   sample-app:latest   "\"/bin/sh -c 'sh -c   Up 6 seconds   0.0.0.0:5099->5099/tcp
```

And then open up <a href="http://172.16.4.64:5099">http://172.16.4.64:5099</a>
(or whatever host you setup for SSH port forwarding) in a web browser. You
should be greeted with something that looks like this:

<img src="/blog/assets/images/posts/docker-lfe-sample-app-main-page.png" />

And that's all there is to it!

## An LFE Web App Aside

If you'd like to look at the source code for this sample app, it has been
made available here:

- [https://github.com/oubiwann/docker-lfe-yaws-sample-app](https://github.com/oubiwann/docker-lfe-yaws-sample-app)

It takes advantage of two interesting LFE libraries:

- [lfest](https://github.com/lfex/lfest) - a
  Clojure/[Compojure](https://github.com/weavejester/compojure)-like
  macro for defining app routes
- [exemplar](https://github.com/lfex/exemplar) - an LFE library for HTML as
  LFE-native s-expressions

Here are some highlights from the sample app:

### Routes

```lfe
(defroutes
  ('GET "/"
    (sample-app-content:get-sidebar-content arg-data))
  ('GET "/content/:id"
    (sample-app-content:get-content id arg-data))
  ('GET "/relation/:userid/:accountid"
    (sample-app-content:get-content userid accountid arg-data))
  ;; When nothing matches, do this
  ('NOTFOUND
   (let* ((joined-path (++ "/" (string:join path "/")))
          (msg (++ "Unmatched route!~n~n"
                   "Path-info: ~p~n"
                   "joined path: ~p~n"
                   "arg-data: ~p~n~n"))
          (msg-args `(,path ,joined-path ,arg-data)))
    (io:format msg msg-args)
    (sample-app-content:four-oh-four
      (++ (strong "Unmatched Route: ") joined-path)))))
```

### Exemplar Content

```lfe
(defun get-side-menu ()
  "An example reusable menu."
  (list
    (li (a '(href "/") "Main Page"))
    (li (a '(href "/content/1") "/content/1"))
    (li (a '(href "/content/2") "/content/2"))
    (li (a '(href "/content/3") "/content/3"))
    (li (a '(href "/relation/1/2") "/relation/1/2"))
    (li (a '(href "/bob") "404"))))
```

## Closing

There are soooo many more examples and use cases to explore. Hopefully we'll
be putting some of those up on this blog for the benefit and interest of our
kind readers.

If you have any questions about the material here, be sure to stop by the
[LFE mail list](https://groups.google.com/forum/#!forum/lisp-flavoured-erlang)
and let us know.

Also, you have any any success stories, cool new LFE Docker images, or other
amazing tales to share, we want to hear those :-)

See you there!
