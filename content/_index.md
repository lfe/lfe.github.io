+++
title = "Lisp Flavoured Erlang"
description = "The website for LFE, the Erlang community's own Lisp."
in_search_index = true
insert_anchor_links = "heading"

[extra]

###   Title Section   #########################################

logo_image = "/images/LFE-logo-abbr-6.2.png"
sitetagline = "Taking the syntax out of distributed systems programming"

#### Important!
####
#### The LFE landing page is a complex arrangement of various widgets
#### which each need to pull their data from specific variables defined
#### in this [extra] section of the Zola config.
####
#### Due to this wild variety on the home page, the actual Markdown
#### content for this file isn't used; it's the metadata that fills
#### all the home page widgets.
####
#### To make this easier to read, the variables have been grouped with
#### their sections via comment dividers below.

###   Quotes   ################################################

quotes = '''
LFE is a proper Lisp, 100% compatible with Core Erlang and able to take full
advantage of OTP.

Light-weight, massive concurrency.
Fault tolerance.
Continuous operation with no downtime.
Full distributed systems.
Asynchronous communication.
Process isolation.
Soft real-time.

Immutable data.
Fixed set of data types.
Pattern matching.
Functional programming language.
Support for modules.
No global data.

Runs efficiently on the BEAM.
Seamless Erlang interop, including the ecosystem of Erlang libraries.

"It is because of you, mother*@#$%, that we are not all using Lisp"
"I'm sorry, what was that last part again? Did you just say 'Lisp'?"

Taking the Syntax out of Distributed Systems Programming

We're Syntactically and Semantically Chaotic

Curly braces go home!

LFE: Where even a distributed Prolog dare not go.

MACLISP and supervision trees are all that anyone needs.

LFE: Fifty Shades of Distributed, Syntax-Free Message-Passing.

Come for the Lisp, stay for the Erlang/OTP.

Come for the fault-tolerance and high-availability. Stay for the fault-tolerance and high-availability.

LFE is _way_ cooler than FOCAL.
What's "FOCAL"?
See?!
'''

###   Download Section   ######################################

[extra.download]

repository = "https://github.com/lfe/lfe"
stable_version = "2.0.1"
unstable_version = "2.1.1"

###   Code Excerpts   #########################################

[extra.excerpts]

[extra.excerpts.repl]

name = "REPL"
id = "repl"
code_old = '''
```text
Erlang/OTP 25 [erts-13.1.1] [source] [64-bit] [smp:16:16] [ds:16:16:10] [async...

   ..-~.~_~---..
  (      \\     )    |   A Lisp-2+ on the Erlang VM
  |`-.._/_\\_.-':    |   Type (help) for usage info.
  |         g |_ \   |
  |        n    | |  |   Docs: http://docs.lfe.io/
  |       a    / /   |   Source: http://github.com/lfe/lfe
   \     l    |_/    |
    \   r     /      |   LFE v2.1.1 (abort with ^G)
     `-E___.-'

lfe>

```
'''
code = '''
![repl image mobile](/images/repl.png)
'''
desc = '''
LFE comes with a powerful REPL, supporting interactive development
(including writing macros!) as well as running scripts or even evaluating
arbitrary LFE code via the command line.
'''

[extra.excerpts.simple_types]

name = "Simple Types"
id = "core-types"
code = '''
```lisp
lfe> (== 42 #b101010)
true

lfe> (integer_to_list 42 2)
"101010"

lfe> #\a
97

lfe> "regular string"
"regular string"

lfe> #"binary string"
#"binary string"
```
'''
desc = '''
Note that in LFE and Erlang a string is really just a list of integers;
there is no "string" type. There is, however, an "atom" type in LFE; this would be analogous to the Lisp symbol. For example, `'my-atom`, or if the atom has spaces in it, `'|my atom|`.
'''

[extra.excerpts.compound_types]

name = "Compound Types"
id = "data-struct"
code = '''
```lisp
;; Lists
lfe> '(a b c 1 2 5)
(a b c 1 2 5)

;; Tuples
lfe> #("element 1" 2 elem-3)
#("element 1" 2 elem-3)

;; Maps
lfe> #m(key1 "value 1"
        "key 2" value-2)
#M("key 2" value-2 key1 "value 1")


```
'''
desc = '''
In LFE lists are like they are in a Lisp (except they also include
strings). Additionally, LFE has tuples (Lisp vectors) and maps
(Lisp has tables). LFE has property lists, dicts, and ordered dicts
from Erlang, supported via additional libraries.
'''

[extra.excerpts.records]

name = "Records"
id = "records"
code = '''
```lisp
;; Defining a record automatically generates a set of
;; useful functions for that particular record.
lfe> (defrecord person
       name
       address
       age)
set-person-age

;; Use the generated record constructor:
lfe> (make-person name "Ford Prefect"
                       address "Betelgeuse Seven"
                       age 234))
#(person "Ford Prefect" "Betelgeuse Seven" 234)

```
'''
desc = '''
Like all data in LFE, records can be pattern-matched. Pattern matching
on record field names and data in function arguments is an extremely powerful
capability provided to developers.
'''

[extra.excerpts.funcs]

name = "Functions"
id = "funcs"
code = '''
```lisp
;; A recursive function with pattern matching:
lfe> (defun ackermann
       ((0 n) (+ n 1))
       ((m 0) (ackermann (- m 1) 1))
       ((m n) (ackermann (- m 1)
                         (ackermann m (- n 1)))))

;; Call the function
lfe> (ackermann 3 4)
125

;; Apply the function
lfe> (funcall #'ackermann/2 3 4))
125
```
'''
desc = '''
As well as supporting the standard Lisp syntax for `defun`,
LFE functions support pattern matching in arguments, allowing you to create
concise, expressive, and elegant code.
'''

[extra.excerpts.macros]

name = "Macros"
id = "macros"
code = '''
```lisp
;; LFE and Erlang do not support n-arity functions, but
;; you can write a Lisp macro to get around that :-)
lfe> (defmacro mean args
       `(/ (lists:sum ,args)
           ,(length args)))

;; Use the macro with different numbers of arguments:
lfe> (mean 1)
1.0
lfe> (mean 1 2)
1.5
lfe> (mean 1 2 3 4 5 6 42 108)
21.375

```
'''
desc = '''
LFE macros are unhygenic, but with scoped variables. There is no `gensym` in
LFE due to this being unsafe in long-lived, distributed code (LFE supports
sharing code with remote nodes). With the exception of running in the REPL,
macros are only compile-time.
'''

[extra.excerpts.erlang_interop]

name = "Erlang Interop"
id = "erl"
code = '''
```lisp
lfe> (lists:reverse
       (erlang:integer_to_list
         (lists:foldl #'*/2 1 '(1 2 3 4))))
"42"

lfe> (supervisor:which_children 'kernel_sup)
(#(logger_sup #Pid<0.70.0> supervisor (logger_sup))
 #(kernel_safe_sup #Pid<0.69.0> supervisor (kernel))
 #(kernel_refc #Pid<0.68.0> worker (kernel_refc))
 #(kernel_config #Pid<0.67.0> worker (kernel_config))
 #(user #Pid<0.63.0> supervisor (user_sup))
 #(standard_error #Pid<0.61.0> supervisor (standard_error))
 #(erl_signal_server #Pid<0.60.0> worker dynamic)
 ...)
```
'''
desc = '''
Here we have two examples of directly calling Erlang functions
from LFE. First, we're "folding" (a.k.a "reducing") over a list
of items, multiplying them by the accumulated value, and then further
transforming using other Erlang functions. Then we are calling an
Erlang function to get information about a particular supervision tree.
'''

[extra.excerpts.otp]

name = "OTP"
id = "otp"
code = '''
```lisp
(defmodule server
  (behaviour gen_server)
  (export
   (start_link 0)
   (stop 0)
   ...))

(defun handle_call
  (('amount _caller state-data)
   `#(reply ,state-data ,state-data))
  (('stop _caller state-data)
   `#(stop shutdown ok state-data))
  ((message _caller state-data)
   `#(reply ,(unknown-command) ,state-data)))
```
'''
desc = '''
OTP is what you use when you need to create industrial grade applications
and services; there's nothing quite like it in the programming world. As
such, it has inspired countless imitations in a great many other programming
languages. In LFE, you get to use the real deal.
'''

###   Summary   ###############################################

[extra.summary]

content = '''
LFE is not a casual Lisp. It's a Lisp for those who want to build distributed
applications -- like the Erlang software that powers 40% of the world's
telecommunications.
'''
link_text = "Learn More"
link_url = "/learn"

###   Why LFE? Section   ######################################

[extra.whylfe]

###   Features Section   ######################################

[extra.features]
title = "LFE Features"

[extra.features.erlang]

title = '''
<i class="fas fa-smile fa-2x"></i>

Core Erlang Foundation
'''
content = '''
All the benefits of Erlang with none of the Prolog:
* No global data
* No mutable data
* Pattern matching and guards
* Compiler and interpreter
* Hot upgrading of deployed code
* The [Banarama of languages](https://www.youtube.com/watch?v=rRbY3TMUcgQ)
<br/><br/><br/>
'''
link_text = "Learn More"
link_url = "#"

[extra.features.lisp]

title = '''
![lisp alien logo](/images/lisplogo-alien-tech-grey.png)

Alien Technology
'''
content = '''
It is an established fact that John McCarthy shared alien tech with the world in 1958 when he introduced us to Lisp. We continue that great tradition.
* Functions and variables with separate namespaces (LFE is a Lisp-2)
* Low-hygiene Macros
* Homoiconicity
* In-REPL Function and macro definitions
<br/><br/>
'''
link_text = "Learn More"
link_url = "#"

[extra.features.otp]

title = '''
<i class="fas fa-pastafarianism fa-2x"></i>

[Utterly Terrifying](https://www.youtube.com/watch?v=rRbY3TMUcgQ)
'''
content = '''
The ability to generate distributed applications and full releases in mere minutes:
* Fault-tolerant
* Massively scalable
* Extreme Concurrency
* Soft real-time
* Open. Telecom. Platform.
<br/><br/><br/><br/><br/><br/>
'''
link_text = "Learn More"
link_url = "#"

[extra.features.lab]

title = '''
<i class="fas fa-flask fa-2x"></i>

Language Lab
'''
content = '''
The mad-scientist powers of a Lisp combined with the efficiency of the Actor Model and Erlang's light-weight processes.

* Experiment with creating distributed systems in new ways.
* Create DSLs on-demand.
* Take advantage of 1000s of cores without having to change your code.
* Easily write your own compilers.
'''
link_text = "Learn More"
link_url = "#"

###   News Section   ##########################################

###   What You Can Build Section   ############################

[extra.buildit]

title = '''
<i class="fas fa-cogs fa-1x"></i> Build It with LFE
'''

[extra.buildit.scripts]

title = "Scripts"
content = '''
In addition to all the heavy-duty power that LFE gets for free, by virtue of
the Erlang VM, you can also create small, easy-to-write one-off
`main` scripts, `lfescript`s, and even escripts -- either as a single file
or with additional modules to help organise those larger scripts.
<br/>
<br/>
'''
link_text = "Learn More"
link_url = "https://lfe.io/reference/lfe-rebar3/current/command-ref/projects/main.html"

[extra.buildit.libraries]

title = "Stand-alone Libraries"
content = '''
Building libraries for use by LFE applications (and even Erlang or other BEAM
language applications!) is the [bread and butter](https://github.com/lfex) of
LFE hackers. The Erlang/BEAM ecosystem is fully accessible to LFE applications,
but you also have the freedom to do more in LFE.
'''
link_text = "Learn More"
link_url = "https://lfe.io/reference/lfe-rebar3/current/command-ref/projects/lib.html"

[extra.buildit.apps]

title = "OTP Applications"
content = '''
You can take advantage of OTP with LFE in mere seconds, creating fault-tolerant
applications with built-in process monitoring and supervision trees, so you
`gen_server`s (and all the other `behaviour`s, too) never go down.
<br/>
<br/>
<br/>
<br/>
'''
link_text = "Learn More"
link_url = "https://lfe.io/reference/lfe-rebar3/current/command-ref/projects/app.html"

[extra.buildit.releases]

title = "OTP Releases"
content = '''
When your LFE prototype is ready for the big-time, you can run it with all the
[sophisticated machinery](https://adoptingerlang.org/docs/production/releases/)
 of an OTP release. In fact, you don't have to wait:
start your prototype as a release, with zero pain and all of the benefit.
<br/>
<br/>
<br/>
<br/>
'''
link_text = "Learn More"
link_url = "https://lfe.io/reference/lfe-rebar3/current/command-ref/projects/release.html"

###   News Section   ##########################################

[extra.news]

title = "News Items"

[extra.news.news1]

title = "TBD"
content = '''
TBD
'''

[extra.news.news2]

title = "TBD"
content = '''
TBD
'''

###   Select Books Section   ##################################

[extra.books]

title = "Books"

[extra.books.lfe_tutorial]

title = "The LFE Tutorial"
content = '''
[img-src]: https://lfe.io/books/tutorial/images/cover.jpg
[img-link]: https://lfe.io/books/tutorial/
[![lfe-tutorial][img-src]][img-link]
'''
link_text = "Read Now"
link_url = "https://lfe.io/books/tutorial/"

[extra.books.casting_spels]

title = "Casting SPELs in LFE"
content = '''
[img-src]: https://lfe.io/books/casting-spels/images/cover.jpg
[img-link]: https://lfe.io/books/casting-spels/
[![casting-spels][img-src]][img-link]
'''
link_text = "Read Now"
link_url = "https://lfe.io/books/casting-spels/"

[extra.books.quick_start]

title = "LFE Quick-Start"
content = '''
[img-src]: https://lfe.io/books/rebar3-quick-start/images/cover.jpg
[img-link]: https://lfe.io/books/rebar3-quick-start/
[![casting-spels][img-src]][img-link]
'''
link_text = "Read Now"
link_url = "https://lfe.io/books/rebar3-quick-start/"

[extra.books.rebar3]

title = "`rebar3_lfe` Command Reference"
content = '''
[img-src]: https://lfe.io/reference/lfe-rebar3/current/images/cover.jpg
[img-link]: https://lfe.io/reference/lfe-rebar3/
[![rebar3_lfe command reference][img-src]][img-link]
'''
link_text = "Read Now"
link_url = "https://lfe.io/reference/lfe-rebar3/"

[extra.books.styleguide]

title = "The LFE<br/>Style Guide"
content = '''
[img-src]: https://lfe.io/books/style-guide/images/cover.jpg
[img-link]: https://lfe.io/books/style-guide/
[![The LFE Style Guide][img-src]][img-link]
'''
link_text = "Read Now"
link_url = "https://lfe.io/books/style-guide/"

[extra.books.sicp]

title = "SICP<br/>The LFE Edition"
content = '''
[img-src]: https://lfe.io/books/sicp/images/cover.jpg
[img-link]: https://lfe.io/books/sicp/
[![sicp][img-src]][img-link]
'''
link_text = "Read Now"
link_url = "https://lfe.io/books/sicp/"

###   Select Videos Section   #################################

[extra.videos]

title = "Videos"

[extra.videos.ecu2016]

title = "EUC 2016 Stockholm<br/>LFE: A Real Lisp in the Erlang Ecosystem<br/>Robert Virding"
content = '''
[img-src]: /images/EUC-2016-LFE.png
[img-link]: https://www.youtube.com/watch?v=x2ysisqgd2g
[![LFE EUC 2016][img-src]][img-link]
'''
link_text = "Watch Now"
link_url = "https://www.youtube.com/watch?v=x2ysisqgd2g"

[extra.videos.efsf2014]

title = "Erlang Factory 2014<br/>LFE from 0 to 120kph...<br/>in 45 Minutes<br/>Duncan McGreggor"
content = '''
[img-src]: /images/EFSF-2014-LFE.png
[img-link]: https://www.youtube.com/watch?v=Dgbm3BRmzuI
[![LFE EFSF 2014][img-src]][img-link]
'''
link_text = "Watch Now"
link_url = "https://www.youtube.com/watch?v=Dgbm3BRmzuI"

[extra.videos.efsf2017]

title = "Erlang Factory 2017<br/>Lisp Machine Flavors for <br/>LFE on OTP<br/>Robert Virding"
content = '''
[img-src]: /images/EFSF-2017-LFE.png
[img-link]: https://www.youtube.com/watch?v=AcehOqbwhPk
[![LFE EFSF 2017][img-src]][img-link]
'''
link_text = "Watch Now"
link_url = "https://www.youtube.com/watch?v=AcehOqbwhPk"


###   Final Callout-1 Section   ###############################

[extra.callout1]
title = "![discord logo](/images/discord-logo.png) Join us on Discord!"
content = '''

To join the LFE conversations on Discord:

* [https://discord.gg/Uf3PszVHtF](https://discord.gg/Uf3PszVHtF)
'''

###   Final Callout-2 Section   ###############################

[extra.callout2]
title = "Give yourself to the Lisp-side of the Force!"
content = '''
[img-src]: /images/xkcd-lisp-cycles.png
[img-link]: http://xkcd.com/297/
[![call-out content mobile][img-src]][img-link]
'''

###   Sponsors Section   ###############################

[extra.sponsors]
title = "A special thanks to our sponsor:"

[extra.sponsors.billo]
content = '''
[img-src]: /images/billo-logo.png
[img-link]: http://billo.systems
[![sponsor][img-src]][img-link]
'''
+++

The home page for LFE.
