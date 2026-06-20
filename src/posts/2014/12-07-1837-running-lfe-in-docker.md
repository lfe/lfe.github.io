---
layout: post.liquid
title: "Running LFE in Docker"
description: "LFE Community is working on growing support for Docker"
permalink: "/blog/tutorials/2014/12/07/1837-running-lfe-in-docker"
categories: ["tutorials"]
tags: ["docker", "paas", "howtos", "web", "apps", "ops", "lfest", "exemplar", "boot2docker"]
published_date: 2014-12-07 18:37:00 +0000
is_draft: false
data:
  author: duncan-mcgreggor
  written_for:
    lfe: "0.9"
    erlang: "17"
  last_validated: null
  cover_image: "/images/tutorials/LFE_Tutorial_00340_.png"
  cover_alt: "Vigdís — LFE tutorial, a retro-futurist study aboard a spaceship"
  math: false
---
<a href="/blog/assets/images/posts/DockerLogo.png"><img class="left thumb" src="/blog/assets/images/posts/DockerLogo.png" /></a>
**Update**: This post has been <a href="/blog/tutorials/2019/05/13/1549-running-lfe-in-docker-updated/">updated</a>
to account for the recenrt changes in the newly published `lfex/lfe` Docker images. The examples given below in this post will
no longer work as written ... original post follows:

With the growing prominence of
[Docker](https://www.docker.com/) in PaaS offerings, from
[Linode](https://blog.linode.com/2014/01/03/docker-on-linode/) and
[OpenShift](https://blog.openshift.com/openshift-v3-platform-combines-docker-kubernetes-atomic-and-more/) to
[Google](https://cloud.google.com/appengine/docs/managed-vms/custom-runtimes) and
[AWS](http://docs.aws.amazon.com/elasticbeanstalk/latest/dg/create_deploy_docker.html),
it is increasingly more crucial that open source projects support their users
by offering images which make deploying their apps even easier. We've only
[just](https://registry.hub.docker.com/u/lfex/lfe/)
[started](https://github.com/rvirding/lfe/issues/97)
doing this in the [LFE](http://lfe.io/) community, but are already quite
excited by the possibilities and the early successes we've seen.

We're currently working on an official LFE image for Docker, but in the mean
time, we will demonstrate functionality with an image we've made available on
the [LFE Exchange in Docker Hub](https://registry.hub.docker.com/u/lfex/lfe/).

**UPDATE**: There is an [updated blog post](https://lfe.io/blog/tutorials/2015/11/28/2110-lfe-yaws-docker-update/)
for the latest image. Please read that for the quickest possible use of the
LFE YAWS Dockerized web app.


## Background Info

For a wonderful intro to Docker, see last year's OpenShift blog post
[Day 21: Docker–The Missing Tutorial](https://blog.openshift.com/day-21-docker-the-missing-tutorial/)
by [Shekhar Gulati](https://twitter.com/shekhargulati). Another *fantastic*
resource is the
[Docker cheatsheet](https://github.com/wsargent/docker-cheat-sheet) by
[Will Sargent](https://github.com/wsargent) of [TypeSafe](https://typesafe.com/).

It goes without saying, but we want to say it anyway: all of the steps below
are just as applicable for all languages running on the
[Erlang](http://erlang.org/) VM: [LFE](http://lfe.io/),
[Elixir](http://elixir-lang.org/),
[Joxa](http://joxa.org/),
[Luerl](https://github.com/rvirding/luerl),
[Erlog](https://github.com/rvirding/erlog),
[Haskerl](https://github.com/etnt/Haskerl), and others. (And, of course,
non-Erlang languages ... but those guys get enough attention as it is ...)

Last but not least, when I was setting up the official LFEX org on Docker Hub,
I was pleasantly surprised to see that someone had already pushed up
[an LFE image](https://registry.hub.docker.com/u/alco/ubuntu-lfe), one based on
Ubuntu. Nice work, [Alexei Sholik](https://twitter.com/true_droid)!


## Setup

This tutorial assumes the following:

 * You are using [boot2docker](http://boot2docker.io/)
 * You have [Docker](https://www.docker.com/) installed on a host machine
 * Both are up and running
 * You have exported the Docker environment variables in all the terminal
   windows you'll be using (2 to 3 is probably all you need)

In order to connect to your containers on your LAN when running boot2docker,
you will need to do something like the following:

```bash
$ boot2docker ssh -L <HOSTMACHINE>:5099:127.0.0.1:5099
```

The server on my LAN that's running ``boot2docker`` and VirtualBox is
172.16.4.64. So here's the SSH port forwarding setup I ran on 172.16.4.64
in order to have access to my containers from a laptop on the LAN:

```text
$ boot2docker ssh -L 172.16.4.64:5099:127.0.0.1:5099
                        ##        .
                  ## ## ##       ==
               ## ## ## ##      ===
           /""""""""""""""""\___/ ===
      ~~~ {~~ ~~~~ ~~~ ~~~~ ~~ ~ /  ===- ~~~
           \______ o          __/
             \    \        __/
              \____\______/
 _                 _   ____     _            _
| |__   ___   ___ | |_|___ \ __| | ___   ___| | _____ _ __
| '_ \ / _ \ / _ \| __| __) / _` |/ _ \ / __| |/ / _ \ '__|
| |_) | (_) | (_) | |_ / __/ (_| | (_) | (__|   <  __/ |
|_.__/ \___/ \___/ \__|_____\__,_|\___/ \___|_|\_\___|_|
Boot2Docker version 1.3.2, build master : 495c19a - Mon Nov 24 20:40:58 UTC 2014
Docker version 1.3.2, build 39fa2fa
docker@boot2docker:~$
```

You'll want to leave that window open :-)

And why wouldn't you? Just *look* at that whale!


## Getting the LFE Exchange Docker Image

Assuming you have everything installed and your forwarding is ready to go,
let's continue by getting the LFE Docker image:

```bash
$ docker pull lfex/lfe
```

Once it's finished downloading, you can make sure everything's working by
running the default command:

```bash
$ docker run lfex/lfe
42
```

This image is dead-simple: it's based on ``debian:jessie`` with the
standard Erlang package from Debian installed on it (as well as the others
needed to support ``git`` and ``rebar``). We're working on support for CentOS,
Ubuntu, and OpenSUSE as well.


## The LFE REPL

Those commands are just executing the default ``CMD`` directive provided with
the image. We can use this image to run all sorts of commands in the
container, such as ``bash``:

```bash
$ docker run -t -i lfex/lfe /bin/bash
```

```bash
root@95bc1d88e581:/#
```

Hey, I've got an idea ...

```bash
root@95bc1d88e581:/# lfe
```

```lfe
Erlang/OTP 17 [erts-6.2] [source] [64-bit] [smp:8:8] [async-threads:10] [kernel-poll:false]

LFE Shell V6.2 (abort with ^G)
> (* 2 (lists:foldl (lambda (n acc) (+ n acc)) 0 (lists:seq 1 6)))
42
```

While we're in here, let's check out some basic version info:

```lfe
> (erlang:system_info 'otp_release)
"17"
> (erlang:system_info 'version)
"6.2"
> (erlang:system_info 'driver_version)
"3.1"
```

Let's quit out:

```lfe
> ^g
User switch command
 --> q
```

```bash
root@c384176355bd:/# exit
exit
```

We're no longer running that container, but it still exists:

```bash
$ docker ps -a
CONTAINER ID   IMAGE         COMMAND       CREATED    STATUS
0a9b061634b5   f80fa561b172  "/bin/bash"   47 s ago   Exited (0) 12 seconds ago
```

But you know what? We can do even better than ``bash`` ... by jumping directly
into our favourite REPL :-)

```bash
$ docker run -t -i lfex/lfe /usr/bin/lfe
```

```lfe
Erlang/OTP 17 [erts-6.2] [source] [64-bit] [smp:8:8] [async-threads:10] [kernel-poll:false]

LFE Shell V6.2 (abort with ^G)
> (* 2 (lists:foldl (lambda (n acc) (+ n acc)) 0 (lists:seq 1 42)))
1806
> ^g
User switch command
 --> q
```

```bash
$
```

Quitting brought us all the way out, bask to the host machine.


## A Simple Customisation of the LFE ``Dockerfile``

So far:

 * We have obtained the LFE image from Docker Hub on our local machine
 * We've run it with the default ``CMD``
 * We've run it with some custom commands

Now we should be comfortable enough using this image to create our own
variation, based on it. We'll start simple, thought, with a single change
to ``CMD``.

```bash
$ mkdir /tmp/lfe
$ cd /tmp/lfe
$ vi Dockerfile
```

Copy the following into this new file (which is just a change of the
original, upstream ``lfex/lfe`` ``Dockerfile`` from ``6`` to ``42``):

```dockerfile
FROM lfex/lfe
CMD /usr/bin/lfe -eval "(io:format \"~p~n\"  \
    (list (* 2 (lists:foldl (lambda (n acc) (+ n acc)) 0 (lists:seq 1 42)))))"
```

Save and quit:

```vim
x:
```

And build your new Docker image:

```bash
$ docker build .
...
Removing intermediate container 73cea1afcab1
Successfully built 46957afffb2c
```

Looking in the output of that last command, we can see that our new image
has an ID of ``46957afffb2c``. Let's tag it (using your own image, of course):

```bash
$ docker tag 46957afffb2c crazy42
```

And run it:

```bash
$ docker run crazy42
1806
```

Excellent -- that's the right answer :-)

Let's take a look at our images so far:

```bash
$ docker images
REPOSITORY      TAG          IMAGE ID          CREATED             VIRTUAL SIZE
crazy42         latest       46957afffb2c      45 minutes ago      521.9 MB
lfe             latest       e7554d932622      About an hour ago   521.9 MB
lfex/lfe        latest       e7554d932622      About an hour ago   521.9 MB
debian          jessie       aaabd2b41e22      4 weeks ago         154.7 MB
```

Ready to create another one?


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

 * [https://github.com/oubiwann/docker-lfe-yaws-sample-app](https://github.com/oubiwann/docker-lfe-yaws-sample-app)

It takes advantage of two interesting LFE libraries:

 * [lfest](https://github.com/lfex/lfest) - a
   Clojure/[Compojure](https://github.com/weavejester/compojure)-like
   macro for defining app routes
 * [exemplar](https://github.com/lfex/exemplar) - an LFE library for HTML as
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
amazing tales to share, we want to hear there :-)

See you there!
