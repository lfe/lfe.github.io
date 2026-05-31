---
layout: post.liquid
title: "FreeBSD & LFE Images: Docker-like functionality with ezjail"
description: ""
permalink: "/blog/tutorials/2015/07/08/1416-freebsd--lfe-images-docker-like-functionality-with-ezjail"
categories: ["tutorials"]
tags: ["freebsd", "docker", "jails", "installs", "containers", "bsd"]
published_date: 2015-07-08 14:16:00 +0000
is_draft: false
data:
  author: duncan-mcgreggor
  written_for: null
  last_validated: null
  cover_image: null
  cover_alt: null
  math: false
---
<a href="/blog/assets/images/posts/freebsd-logo.jpg"><img class="left thumb" src="/blog/assets/images/posts/freebsd-logo.jpg" /></a>Following
on the heels of the
[previous BSD post](/blog/tutorials/2015/07/08/1349-dragonflybsd--lfe/), we now turn
to virtualization in *BSDs, in particular, jails, ``ezjail``, and FreeBSD, a
combination that is not too dissimilar from a Docker experience (minus, of
course, Docker Hub. Oh, if only there was a *BSD jail hub ...).

Below we'll briefly outline the steps necessary to install ``ezjail``, use it
to create a base jail, jail images, and in particular, an LFE image.

Let's get started.

## ``ezjail``

The first step is installing and configuring ``ezail``:

```
% su -
# pkg install -y ezjail
```

Then configuring it:

```
# echo 'cloned_interfaces="${cloned_interfaces} lo1"' >> /% etc/rc.conf
# echo 'ezjail_enable="YES"' >> /etc/rc.conf
```

And then starting up some services and installing a base jail:

```
# service netif cloneup
# service ezjail start
# ezjail-admin install -p
```

## Creating a Base Erlang Jail

When your base jail has finished with its installation, you will be ready to
create the Erlang jail. The first thing you'll need to do is configure
networking:

```
# ezjail-admin create erlang-jail 'lo1|127.0.1.1,em0|10.0.4.70'
```

That command associates the IP address 127.0.1.1 with the cloned loopback
address we created. It also adds a new local network IP address to the existing
em0 network interface.

Now you can start the jail and then connect to it:

```
# ezjail-admin start erlang-jail
# ezjail-admin console erlang-jail
```

From inside the jail you'll need to make sure hostname resolution is enabled
and that your hosts file gets updated:

```
root@erlang-jail:~ # vi /etc/resolv.conf
nameserver 10.0.4.1
:x
root@erlang-jail:~ # vi /etc/hosts
:%s/127.0.0.1/127.0.1.1/g
:x
```

At this point, you'll be able to pull down packages from the FreeBSD mirrors:

```
root@erlang-jail:~ # pkg update
The package management tool is not yet installed on your system.
Do you want to fetch and install it now? [y/N]: y
root@erlang-jail:~ # pkg install -y gmake curl git erlang rebar rebar3
root@erlang-jail:~ # exit
```

Having exited from the jail, you can stop it and then create your Erlang base
image from it:

```
# ezjail-admin stop erlang-jail
Stopping jails: erlang-jail.
# ezjail-admin archive erlang-jail
# ls -l /usr/jails/ezjail_archives
total 311136
-rw-r--r--  1 root  wheel  318460289 12:30 erlang_jail-201507081230.20.tar.gz
```

We can now use this as the basis for other jails and images. Take LFE, for
instance ...

## Creating an LFE Jail

Using the erlang-jail as the base, let's create an LFE jail, start it, and then
connect to it:

```
# ezjail-admin create \
    -a /usr/jails/ezjail_archives/erlang_jail-201507081230.20.tar.gz \
    lfe-jail  'lo1|127.0.2.1,em0|10.0.4.71'
# ezjail-admin start lfe-jail
# ezjail-admin console lfe-jail
```

This jail is using a different IP address for localhost from both the host
machine as well as the Erlang jail upon which it is based. So let's update your
/etc/hosts file:

```
root@lfe-jail:~ # vi /etc/hosts
:%s/127.0.0.1/127.0.2.1/g
:x
```

Now install the required packages for the LFE jail and set up the sources:

```
root@lfe-jail:~ # pkg install -y base64 bash
root@lfe-jail:~ # rehash
root@lfe-jail:~ # cd /usr/local
root@lfe-jail:~ # git clone https://github.com/rvirding/lfe
root@lfe-jail:~ # cd lfe
root@lfe-jail:~ # gmake && gmake install
root@lfe-jail:~ # curl -L -o ./lfetool \
    https://raw.github.com/lfe/lfetool/dev-v1/lfetool
root@lfe-jail:~ # bash ./lfetool install
root@lfe-jail:~ # rm lfetool
root@lfe-jail:~ # rehash
root@lfe-jail:~ # exit
```

We're now ready to create the LFE image:

```
# ezjail-admin stop lfe-jail
Stopping jails: lfe-jail.
# ezjail-admin archive lfe-jail
```

Let's make sure it's where it should be:

```
# ls -lh /usr/jails/ezjail_archives
total 628992
-rw-r--r--  1 root  wheel   304M 12:30 erlang_jail-201507081230.20.tar.gz
-rw-r--r--  1 root  wheel   310M 12:44 lfe_jail-201507081243.56.tar.gz
```

This image can now be shared and installed on any FreeBSD machine running
ezjail, though the networking will need to be updated, depending upon the
ezjail host setup.

You're done!

## Using the LFE Image

At any time in the future when you want to do LFE development on your image,
just start it up like we did above:

```
# ezjail-admin start lfe-jail
# ezjail-admin console lfe-jail
```

```bash
root@lfe-jail:~ # lfe
Erlang/OTP 17 [erts-6.4.1] [source] [64-bit] ,,,

> (* 2 (lists:foldl (lambda (n acc) (+ n acc)) 0 (lists:seq 1 6)))
42
```

## Related Projects

Docker is currently [preparing to support FreeBSD](https://github.com/docker/docker/pull/13542)
(see more context [here](https://github.com/docker/docker/pull/5467)), which is
very exciting. There's also the [jetbpack project](https://github.com/3ofcoins/jetpack),
a Rocket implementation under way for FreeBSD. They are both well worth
tracking and playing with.
