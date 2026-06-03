---
layout: post.liquid
title: "lfest 0.0.2 Is Released"
description: "New Version of lfest Available"
permalink: "/blog/announcements/2014/12/08/1929-lfest-002-is-released"
categories: ["announcements"]
tags: ["releases", "libraries"]
published_date: 2014-12-08 19:29:00 +0000
is_draft: false
data:
  author: duncan-mcgreggor
  written_for: null
  last_validated: null
  cover_image: "/images/newsroom/LFE_Newsroom_00310_.png"
  cover_alt: "Vigdís — LFE news, a busy rotating space-station newsroom"
  math: false
---
lfest 0.0.2 includes the addition of text-only responses in addition to the
already-supported HTML and JSON responses. This addition was made in order to
more readily support LFE web app developers writing for services such as Google
App Engine that expect apps to implement particular resource endpoints
(e.g.,
[/_ah/health](https://cloud.google.com/appengine/docs/python/managed-vms/#health_checking))
which return plain-text responses.

More information:

 * [lfest repo](https://github.com/lfex/lfest)
 * [lfest 0.0.2](https://github.com/lfex/lfest/releases/tag/0.0.2)
 * An LFE-Erlang-Docker
   [example web app](https://github.com/oubiwann/docker-lfe-yaws-sample-app)
   which uses lfest for its routing and responses
