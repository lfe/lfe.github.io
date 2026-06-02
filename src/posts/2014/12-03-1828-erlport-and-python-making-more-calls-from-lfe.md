---
layout: post.liquid
title: "ErlPort and Python Part II: Making More Calls from LFE"
description: "Part II of an Introduction to ErlPort with LFE"
permalink: "/blog/tutorials/2014/12/03/1828-erlport-and-python-making-more-calls-from-lfe"
categories: ["tutorials"]
tags: [howtos, docs, erlport, python, interop, lfe, code, libraries, lisp, python 3, tools, tutorials]
published_date: 2014-12-03 18:28:00 +0000
is_draft: false
data:
  author: duncan-mcgreggor
  written_for: null
  last_validated: null
  cover_image: "/images/default/LFE_00245_.png"
  cover_alt: "Vigdís — LFE, retro-futurist digital painting"
  math: false
---
<a href="/blog/assets/images/posts/Erlang-Python-Greats.png"><img class="right thumb" src="/blog/assets/images/posts/Erlang-Python-Greats.png" /></a>
A short while ago, I did a
[teaser post](https://lfe.io/blog/tutorials/2014/11/21/1508-erlport-using-python-from-erlang-lfe/)
about calling Python from LFE. There was only a tiny bit of code... but! It
came with one of the best Erlang/Python pictures EVAR. You know which one I'm
talking about.

In this post, we're going to venture further into this strange and wonderful
landscape. We'll do this by essentially adapting
[Dmitry Vasiliev](https://twitter.com/hdima)'s
[ErlPort Python docs](http://erlport.org/docs/python.html) from Erlang to LFE.
We won't get through all the docs in this post, but we'll definitely get
further than last time :-)


## Warming Up

We're going to re-use the demo repo from the last post. If you didn't get
chance to set it up, you can do that now:

```bash
$ git clone git@github.com:oubiwann/erlport-demo.git
$ cd erlport-demo/python
$ python3.4 -m venv .venv
$ . .venv/bin/activate
$ cd ../lfe
$ make repl
```

If you've already downloaded and run the REPL before, you can skip the build
step with this:

```bash
$ make repl-no-deps
```

Let's start the walkthrough by creating two separate running Python servers:

```lfe
> (set `#(ok ,pid-1) (python:start))
#(ok <0.32.0>)
> (set `#(ok ,pid-2) (python:start))
#(ok <0.33.0>)
```


## Using Operators

Next, let's do some basic math:

```lfe
> (python:call pid-1 'operator 'add '(21 21))
42
> (python:call pid-1 'operator 'sub '(294 252))
42
> (python:call pid-1 'operator 'mul '(21 2))
42
> (python:call pid-1 'operator 'floordiv '(294 7))
42
> (python:call pid-1 'operator 'truediv '(294 7))
42.0
```
The use of operators is extremely convenient, since with ErlPort we are bound
to same the module-function-args approach used in Erlang and LFE. There are
many other operators we can call, but this should give you a taste.

The full list of operators provided as builtin functions is available
[here](https://docs.python.org/3.4/library/operator.html).


## Using Built-ins

Just for fun, we'll switch to our other Python server for these examples:

```lfe
> (set data '(42 3 19 11 7 5 11 2))
(42 3 19 11 7 5 11 2)
> (python:call pid-2 'builtins 'sorted `(,data))
(2 3 5 7 11 11 19 42)
> (python:call pid-2 'builtins 'dir '(str))
("_Atom__atoms"
 "__add__"
 "__class__"
 "__contains__"
 "__delattr__"
 "__dir__"
 "__doc__"
 "__eq__"
 "__format__"
 "__ge__"
 "__getattribute__"
 "__getitem__"
 "__getnewargs__"
 "__gt__"
 "__hash__"
 "__init__"
 "__iter__"
 "__le__"
 "__len__"
 "__lt__"
 "__module__"
 "__mul__"
 "__ne__"
 "__new__"
 "__reduce__"
 "__reduce_ex__"
 "__repr__"
 "__rmul__"
 "__setattr__"
 "__sizeof__" ...)
```

In Python, if you want to use a module's functions, classes, and other objects,
you need to import them. Some, however, are available as part of the language
and don't require any importing. These are called the "builtins".

Here are a couple more examples:

```lfe
> (python:call pid-2 'builtins 'len `(,data))
8
> (python:call pid-2 'builtins 'pow '(16 2))
265
> (python:call pid-2 'builtins 'pow '(2 16))
65536
```

For a full list, see the following:

* Python [builtin functions](https://docs.python.org/3/library/functions.html#built-in-funcs)
* Python [builtin constants](https://docs.python.org/3/library/constants.html#built-in-consts)

If you're wondering how to deal with constants (since there are no constants in
LFE, and everything is a function), hang tight -- we'll cover that below.


## Python Module Hierarchies

We can also call dotted names. Let's get the cosine of 2$\pi$:

```lfe
> (python:call pid-1 'math 'cos `(,(* 2 3.1459)))
0.9999628937632861
```

Note that we're executing the multiplication in LFE before sending it to
Python. But what if we wanted to get Python's ``math.pi`` value instead of
using our own? How do we access Python module constants?

```lfe
> (python:call pid-1 'math 'pi '())
exception error: #(python builtins.TypeError
                   "'float' object is not callable"
                   ...)
```

No such luck: ``math.pi`` is a ``float`` and we're limited to making calls.
Fortunately, though, we have a way out -- a hack, but a way out: we can call
methods on constants!

```lfe
> (python:call pid-1 'math 'pi.__float__ '())
3.141592653589793
> (python:call pid-1 'math 'pi.__int__ '())
3
> (python:call pid-1 'math 'pi.__str__ '())
"3.141592653589793"
```

Yeah, perhaps a bit ugly ... but you *knew* this wasn't going to be pretty :-)

Now we can re-do our example, though:

```lfe
> (set pi (python:call pid-1 'math 'pi.__float__ '()))
3.141592653589793
> (python:call pid-1 'math 'cos `(,(* 2 pi)))
1.0
```

That's more like it :-)


## Errors

Python exceptions are returned in the ``error`` values. Here's a function
which prints the error returned from Python as well as the stacktrace in LFE:

```lfe
> (defun print-error-data ()
    (try
      (python:call pid-1 'unknown 'unknown '())
      (catch (`#(error ,value ,tracebock)
                (lfe_io:format "Error value: ~p~n" `(,value))
                (lfe_io:format "Error stacktrace: ~p~n" `(,tracebock))))))
print-error-data
> (print-error-data)
Error value: #(python builtins.ImportError
               "No module named 'unknown'"
               (#("/Users/oubiwann/lab/erlang/erlport-demo/lfe..."
                  236
                  "_incoming_call"
                  "f = __import__(mod, {}, {}, [objects[0]])")
                #("/Users/oubiwann/lab/erlang/erlport-demo/lfe..."
                  244
                  "_call_with_error_handler"
                  "function(*args)")))
Error stacktrace: (#(erlport call 3 (#(file "src/erlport.erl") #(line 234)))
                  #(lfe_eval eval_try 5
                    (#(file "src/lfe_eval.erl") #(line 663)))
                  #(lfe_shell eval_form_1 2
                    (#(file "src/lfe_shell.erl") #(line 253)))
                  #(lists foldl 3 (#(file "lists.erl") #(line 1261)))
                  #(lfe_shell server_loop 1
                    (#(file "src/lfe_shell.erl") #(line 99))))
ok
```

The Python error is comprised of the following:

 * The ``'python`` atom,
 * The Python exception class, and
 * The Python traceback from the ErlPort library

Here's a function that just displays those:

```lfe
> (defun print-python-error ()
    (try
      (python:call pid-1 'operator 'truediv '(1 0))
      (catch (`#(error #(python ,class ,msg ,traceback) ,_)
                (lfe_io:format "Python exception class: ~p~n" `(,class))
                (lfe_io:format "Python exception text: ~p~n" `(,msg))
                (lfe_io:format "ErlPort traceback: ~p~n" `(,traceback))))))
print-python-error
> (print-python-error)
Python exception class: builtins.ZeroDivisionError
Python exception text: "division by zero"
ErlPort traceback: (#("/Users/oubiwann/Dropbox/lab/erlang/erlport-demo/lfe..."
                     239
                     "_incoming_call"
                     "result = Atom(b\"r\"), mid,
                     self.encoder(f(*map(self.decoder, args)))")
                   #("/Users/oubiwann/Dropbox/lab/erlang/erlport-demo/lfe..."
                     244
                     "_call_with_error_handler"
                     "function(*args)"))
ok
```

In the next post we'll take a look at ErlPorts opaque Python objects.

