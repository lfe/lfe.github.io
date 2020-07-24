+++
title = "Site Design : Layout : Home"
template = "design/layout-home.html"

[extra]
sitetagline = "MACLISP and supervision trees are all that anyone needs."

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
'''

excerpt1_name = "REPL"
excerpt1_id = "repl"
excerpt1_code = '''
```text
Erlang/OTP 23 [erts-11.0] [source] [64-bit] [smp:16:16] [ds:16:16:10] [async...

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
'''
excerpt1_desc = '''
LFE comesm with a powerful REPL, supporting interactive development
(including writing macros!) as well as running scripts or even evaluating 
arbitrary LFE code via the command line.
'''

excerpt2_name = "Simple Types"
excerpt2_id = "core-types"
excerpt2_code = '''
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
excerpt2_desc = '''
Note that in LFE and Erlang a string is really just a list of integers;
there is no "string" type. There is, however, an "atom" type in LFE; this would be analogous to the Lisp symbol. For example, `'my-atom`, or if the atom has spaces in it, `'|my atom|`.
'''


excerpt3_name = "Compound Types"
excerpt3_id = "data-struct"
excerpt3_code = '''
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
excerpt3_desc = '''
In LFE lists are like they are in a Lisp (except they aslo include
strings). Additionally, LFE has tuples (Lisp vectors) and maps
(Lisp has tables). LFE has property lists, dicts, and ordered dicts
from Erlang, supported via additional libraries.
'''

excerpt4_name = "Records"
excerpt4_id = "records"
excerpt4_code = '''
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
excerpt4_desc = '''
Like all data in LFE, records can be pattern-matched. Pattern matching
on record field names and data in function arguments is an extremely powerful
capability provided to developers.
'''

excerpt5_name = "Functions"
excerpt5_id = "funcs"
excerpt5_code = '''
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
excerpt5_desc = '''
As well as supporting the standard Lisp syntax for `defun`, 
LFE functions support pattern matching in arguments, allowing you to create
concise, expressive, and elegent code.
'''

excerpt6_name = "Macros"
excerpt6_id = "macros"
excerpt6_code = '''
```lisp
;; Erlang and LFE do not support n-arity functions, but
;; you can write a Lisp macro to get around that :-)
(defmacro mean args
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
excerpt6_desc = '''
LFE macros are unhygenic, but with scoped variables. There is no `gensym` in
LFE due to this being unsafe in long-lived, distributed code (LFE supports
sharing code with remote nodes). With the excetion of running in the REPL, 
macros are only compile-time.
'''

excerpt7_name = "Erlang Interop"
excerpt7_id = "erl"
excerpt7_code = '''
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
excerpt7_desc = '''
Here we have two examples of directly calling Erlang functions
from LFE. First, we're "folding" (a.k.a "reducing") over a list
of items, mutliplying them by the accumulated value, and then further
transforming using other Erlang functions. Then we are calling an
Erlang fucntion to get information about a particular superversion tree.
'''

excerpt8_name = "OTP"
excerpt8_id = "otp"
excerpt8_code = '''
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
excerpt8_desc = '''
OTP is what you use when you need to create industrial grade applications
and services; there's nothing quite like it in the programming world. As
such it has inspired countless imitations in a great many other programming
languages.
'''

+++

Layout for the home page.
