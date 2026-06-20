---
layout: post.liquid
title: "Update: Running an LFE YAWS app in Docker"
description: "Docker LFE/YAWS sample app has been updated"
permalink: "/blog/tutorials/2015/11/28/2110-lfe-yaws-docker-update"
categories: ["tutorials"]
tags: ["docker", "paas", "howtos", "web", "apps", "ops", "lfest", "exemplar", "boot2docker"]
published_date: 2015-11-28 21:10:00 +0000
is_draft: false
data:
  author: duncan-mcgreggor
  written_for:
    lfe: "0.10"
    erlang: "18"
  last_validated: null
  cover_image: "/images/tutorials/LFE_Tutorial_00392_.png"
  cover_alt: "Vigdís — LFE tutorial, a retro-futurist study aboard a spaceship"
  math: false
---
<a href="/blog/assets/images/posts/DockerLogo.png"><img class="left thumb" src="/blog/assets/images/posts/DockerLogo.png" /></a>Last December we posted a
[blog entry](/blog/tutorials/2014/12/07/1837-running-lfe-in-docker/)
about Docker
[Docker](https://www.docker.com/) and LFE. Since that time there have been
several version changes in Erlang, LFE, and the LFE libraries used. In addition,
the process to get the demo Dockerized YAWS web app running has been streamlined.
This post covers those changes.


## Setup

If you are running on Mac OS X, you'll need to install and start
[boot2docker](http://boot2docker.io/). If you're running on Linux,
you'll need to start the ``docker`` services (for instance, on Ubuntu,
``sudo /etc/init.d/docker start``)


### Port-Forwarding

Docker on Linux handles port-redirects between the host and guest just fine;
for Docker on Mac OS X, you'll need to setup port-forwarding via SSH:

```bash
$ boot2docker ssh -L <HOSTMACHINE>:5099:127.0.0.1:5099
```

Then just leave that terminal window open and continue with the rest of this
post in another terminal.


## LFE Exchange Docker Images

There are several LFE Docker images to choose from, all available via the
LFEX Docker Hub org:
 * https://hub.docker.com/u/lfex/

They are of two different types:
 * One image generated from the ``Dockerfile`` maintained in the
   [official LFE repository](https://github.com/rvirding/lfe/) using the ``develop``
   branch
 * All the others which are maintained in the
  [lfex dockerfiles repository](https://github.com/lfex/dockerfiles) are kept in
  sync with the official ``develop`` branch but also offer the added
  feature of a [color logo banner](https://github.com/rvirding/lfe/pull/116)
  when you start up the LFE REPL.


## YAWS LFE Docker Image

As part of the update of the old post, I've created a Docker with YAWS and
the demo web app pre-installed and ready to go. It's available here:
 * https://hub.docker.com/r/oubiwann/lfe-yaws-sample-app/
 * https://github.com/oubiwann/docker-lfe-yaws-sample-app/

For the last blog post we used the
[OpenSUSE image](https://hub.docker.com/r/lfex/opensuse/), but for this one
we've based off of the [Debian image](https://hub.docker.com/r/lfex/debian/),
since it's the smallest one.

If you'd rather build the YAWS LFE sample app Docker image yourself than
download it from Docker Hub, simply run ``make docker-build`` from the cloned
source directory for the ``docker-lfe-yaws-sample-app`` project.


## Running the Container

Whether you build the image yourself or chose to let ``docker`` download it for
you from Docker Hub, either way the same command is used to run the container:

```bash
$ docker run -p 5099:5099 -t oubiwann/lfe-yaws-sample-app:latest
```

Almost immediately the YAWS app will be up and running. Visiting
``http://<HOSTMACHINE>:5099`` in a web browser will show the running YAWS LFE
web app:

<img src="/blog/assets/images/posts/docker-lfe-sample-app-main-page.png" />


## The LFE REPL

If, instead of running the app server, you'd like to use the LFE REPL on this
image with all of the dependent libraries, you can run this command instead:

```bash
$ docker run -i -t oubiwann/lfe-yaws-sample-app:latest lfe
```

<img src="/blog/assets/images/posts/lfe-color-banner.png" />


## Why the Update?

Due to the question of a curious community member, I'm now preparing a blog
post on using YAWS websockets support with an LFE app. I wanted to use the
sample app featured in the original LFE Docker blog post (linked at the
beginning of this post), but it needed some updates and further finessing ...
upon the completion of which, it warranted a new post describing how to use it
now.

Keep your eyes peeled for the websockets post ...


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
