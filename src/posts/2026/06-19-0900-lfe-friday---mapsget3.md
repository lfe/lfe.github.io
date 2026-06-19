---
layout: post.liquid
title: "LFE Friday - maps:get/3"
description: "Reopening LFE Friday with the safe map read: maps:get/3 hands back a default instead of crashing on a missing key — plus the one eager-evaluation gotcha to know."
permalink: "/blog/tutorials/2026/06/19/0900-lfe-friday---mapsget3"
categories: ["tutorials"]
tags: [lfe-friday, lfe, erlang, maps, defaults]
published_date: 2026-06-19 09:00:00 +0000
is_draft: false
data:
  author: duncan-mcgreggor
  written_for:
    lfe: "2.2.1"
    erlang: "28"
  last_validated: 2026-06-18
  cover_image: "/images/fridays/LFE_Friday_00269_.png"
  cover_alt: "Vigdís — LFE Friday, LiffyBot reaching into a glowing key-value lattice in a Mead-style command room"
  math: false
---

Today's LFE Friday digs into [`maps:get/3`](https://www.erlang.org/doc/apps/stdlib/maps.html#get/3) — and with it, quietly reopens the series after a rather long pause.

It has been a while. The last time LFE Friday reached for a key-value store, the answer was [`dict:merge/3`](/blog/tutorials/2015/03/01/1823-lfe-friday---dictmerge3), back in March 2015. `dict` still works. But it is no longer where modern LFE reaches first: the `maps` module landed in Erlang/OTP 17, a few months after this series first started, and somehow never got its turn here. So the revival starts where you actually start most days — pulling a value back out of a map without getting burned when the key isn't there. Over the next few Fridays we will walk the rest of the module; today, the safe read.

`maps:get/3` takes a key, a map, and a default value. It returns the value stored under that key — or, if the key is not in the map, the default you handed it. That third argument is the whole point: it is the difference between a lookup that answers and a lookup that crashes.

Let's build a little config map to read from.

```lfe
lfe> (set config (map 'host "localhost" 'port 8080 'scheme 'http))
#M(port 8080 scheme http host "localhost")
```

Notice the shell hands the keys back in its own order, not the order we typed them in — `maps` are unordered, and the printed layout reflects the map's internal arrangement rather than our insertion order. Reading a key that exists, though, is unremarkable, which is exactly what you want from a read.

```lfe
lfe> (maps:get 'host config)
"localhost"
lfe> (maps:get 'port config)
8080
```

Those are `maps:get/2`, the two-argument cousin. Now let's try to break it — what happens when we ask for a key that isn't there?

```lfe
lfe> (maps:get 'timeout config)
** exception error: #(badkey timeout)
  in (maps : get timeout #M(port 8080 scheme http host "localhost"))
```

`maps:get/2` does not shrug and hand back `undefined` the way some languages' map lookups do. A missing key is an *error* — `#(badkey timeout)` — and it will take down a process that wasn't expecting it. Often that strictness is precisely what you want. But just as often you have a sensible fallback in mind, and crashing is overkill. That is what the third argument buys you.

```lfe
lfe> (maps:get 'timeout config 5000)
5000
lfe> (maps:get 'host config "0.0.0.0")
"localhost"
```

When the key is missing you get the default; when it is present the default is ignored and you get the real value. One function — no `try`, no `case`.

There is one wrinkle worth knowing before you sprinkle `maps:get/3` everywhere. LFE, like Erlang, **evaluates arguments eagerly**: every argument is computed *before* the call happens. So the default is built whether or not it ends up being used.

```lfe
lfe> (maps:get 'host config (progn (io:format "computing default~n") "fallback"))
computing default
"localhost"
```

The key `host` is present, so `"fallback"` is thrown away — but `computing default` still printed, because the argument was evaluated first. For a constant like `5000` that costs nothing. For a default that hits a database or spawns a process, it is a real bill, paid on every lookup, including the hits. When the default is expensive, don't reach for `get/3` — reach for `maps:find/2` and decide what to do only on a miss.

```lfe
lfe> (maps:find 'host config)
#(ok "localhost")
lfe> (maps:find 'timeout config)
error
```

`maps:find/2` is `get/3`'s quieter sibling: instead of a default, it tells you whether the key was there at all, as a tagged result — `#(ok Value)` or the bare atom `error` — that you can match on, rather than a value you can't tell apart from a real stored default.

This is also where the decade shows. `dict` has `fetch/2` (crashes, like `get/2`) and `find/2` (tagged, like `maps:find/2`) — but no ergonomic fetch-with-a-default. `maps:get/3` — which arrived in OTP 17.1, a point release behind the `maps` module itself — folds that whole pattern into a single call, and underneath, a map lookup is effectively constant-time for the sizes you'll meet in practice. That economy is a good part of why `maps` won.

Next week we will start putting things *into* maps. 
