---
layout: post.liquid
title: "Scientific Computing on the Erlang VM"
description: "Polynomial Curve Fitting with LFE"
permalink: "/blog/tutorials/2015/01/01/1215-scientific-computing-on-the-erlang-vm"
categories: ["tutorials"]
tags: [erlang, python, howtos, libraries, numpy, scipy, erlport, py, ascii, plotting, maths, statistics, science, scientific computing, statistical analysis, linear regression, curve fitting, polynomials]
published_date: 2015-01-01 12:15:00 +0000
is_draft: false
data:
  author: duncan-mcgreggor
  written_for: null
  last_validated: null
  cover_image: "/images/tutorials/LFE_Tutorial_00361_.png"
  cover_alt: "Vigdís — LFE tutorial, a retro-futurist study aboard a spaceship"
  math: true
---
<a href="/blog/assets/images/posts/lsci-erlang-ecosystem.png"><img class="right thumb" src="/blog/assets/images/posts/lsci-erlang-ecosystem.png" /></a>This tutorial brings in the New Year by introducing the Erlang/LFE
scientific computing library [lsci](https://github.com/lfex/lsci)
-- a ports wrapper of NumPy and SciPy (among others) for the Erlang ecosystem.
The topic of the tutorial is polynomial curve-fitting for a given data set.
Additionally, this post further demonstrates [py](https://github.com/lfex/py/)
usage, the
[previously discussed](http://localhost:4000/blog/announcements/2014/12/27/1641-easy-python-from-lfeerlang)
Erlang/LFE library for running Python code from the Erlang VM.


## Background

The content of this post was taken from a
[similar tutorial](http://technicae.cogitat.io/2014/11/scientific-computing-with-hy.html)
done by the same author for the Python Lisp [Hy](http://hylang.org/) in an
[IPython notebook](http://nbviewer.ipython.org/github/oubiwann/linear-regression-tutorial/blob/master/notebooks/tutorial.ipynb).
It, in turn, was completely inspired by the
[Clojure Incanter tutorial](http://data-sorcery.org/2009/06/04/linear-regression-with-higher-order-terms/)
on the same subject, by David Edgar Liebke.

This content is also available in the
[lsci examples directory](https://github.com/lfex/lsci/tree/cbba7e4705bdc8baaa8b8abaf40ae1649ed44b42/examples/polyfit).


## Introduction

The lsci library (pronounced "Elsie") provides access to the fast numerical
processing libraries that have become so popular in the scientific computing
community. lsci is written in LFE but can be used just as easily from Erlang.

lsci provides the following set of features:

 * Wrapper functions (many generated dynamically via macros) for:
   * The Python 3 standard library module [math](https://docs.python.org/3/library/math.html)
   * The Python 3 standard library module [cmath](https://docs.python.org/3/library/cmath.html)
   * The Python 3 standard library module [statistics](https://docs.python.org/3/library/statistics.html)
   * [NumPy](http://www.numpy.org/)
   * [SciPy](http://www.scipy.org/scipylib/index.html)
   * Planned support for
     [fractions](https://docs.python.org/3/library/fractions.html),
     [decimal](https://docs.python.org/3/library/decimal.html),
     [Pandas](http://pandas.pydata.org/),
     [matplotlib](http://matplotlib.org/), and
     [SymPy](http://www.sympy.org/en/index.html)
 * The [py](https://github.com/lfex/py/) wrappers for
   [ErlPort](http://erlport.org/) which make calling Python
   module-level functions, object attributes and methods, constructors,
   function objects, etc., very easy
 * Custom encoders/decoders for some of the wrapped data types

lsci is brand-new, and thus has far to go before it completely wraps all the
functionality in NumPy, SciPy, etc. However, enough of it is done that one can
perform tasks like polynomial curve-fitting and statistical regression.


## Setup

To run this tutorial, you will need the following on your system:

 * Erlang (tested with 17.3)
 * rebar
 * lfetool
 * Python 3
 * git

With those in place, let's get you ready:

```lfe
$ git clone git@github.com:lfex/lsci.git
$ cd lsci
$ make
$ . ./python/.venv/bin/activate
```

That will download all the Erlang and Python dependencies, compile the Erlang
libs, and set up a Python virtualenv in your working directory. If you are
not familiar with Python, the last command is what allows one to actually
use the Python virtualenv, with all the libraries you just downloaded.

You are now ready for the LFE REPL:

```lfe
$ make repl-no-deps
```

## Loading Data

The "observed data" we're going to use is the same data set as
that used in the
[Incanter linear regression tutorial](http://data-sorcery.org/2009/06/04/linear-regression-with-higher-order-terms/),
the
[NIST Filip.dat](http://www.itl.nist.gov/div898/strd/lls/data/LINKS/DATA/Filip.dat)
file. The data file we will use is a conversion of the original NIST file to
CSV.

Let's load our experimental data:

```lfe
> (set data (lsci-np:genfromtxt
              "examples/polyfit/filip.csv"
              `(#(delimiter ,(list_to_binary ","))
                #(names true))))
#($erlport.opaque python
  #B(128 2 99 110 117 109 112 121 46 99 111 114 101 46 109 117 108 ...))
```

The data returned by this function is an ErlPort binary wrapping a Python
pickle of a NumPy array. We can take a look at the data by converting it to
a list:

```lfe
> (lsci-np:->list data)
(#(0.8116 -6.860120914)
 #(0.9072 -4.324130045)
 #(0.9052 -4.358625055)
 #(0.9039 -4.358426747)
 #(0.8053 -6.955852379)
 #(0.8377 -6.661145254)
 #(0.8667 -6.355462942)
 #(0.8809 -6.118102026)
 #(0.7975 -7.115148017)
 #(0.8162 -6.815308569)
 #(0.8515 -6.519993057)
 #(0.8766 -6.204119983)
 #(0.8885 -5.853871964)
 #(0.8859 -6.109523091)
 #(0.8959 -5.79832982)
 #(0.8913 -5.482672118)
 #(0.8959 -5.171791386)
 #(0.8971 -4.851705903)
 #(0.9021 -4.517126416)
 #(0.909 -4.143573228)
 #(0.9139 -3.709075441)
 #(0.9199 -3.499489089)
 #(0.8692 -6.300769497)
 #(0.8872 -5.953504836)
 #(0.89 -5.642065153)
 #(0.891 -5.031376979)
 #(0.8977 -4.680685696)
 #(0.9035 -4.329846955)
 #(0.9078 ...)
 #(...) ...)
```


## A Note about Values

If you haven't noticed yet, you certainly will as you run through these
examples in the LFE REPL: we keep getting opaque binary data back from NumPy.
Why can't lsci just convert that?

While it would be nice to have that data presented to us in the REPL in such
a way that we could *actually tell* what is was, this would run counter to the
whole purpose of using libraries like NumPy and SciPy from the Erlang VM in the
first place. It would do several things:

* Add latency for each calculation
* Reduce the precision available in the NumPy data types (by being converted
  first to Python native types and then to Erlang data types)
* Bring only Erlang-native data types into the REPL, thus requiring us to
  create new NumPy data types to pass *back* to Python if we wanted to benefit
  from NumPy's speed, or another way of saying this,
* Make it difficult to pass results back into NumPy for further processing

As such, lsci compromises by providing convenience functions for converting
returned results to something we can look at. We will use such functions as
``->list`` and ``->float`` in the remainder of this tutorial for just that
purpose.


## Plotting Our Data

Because our CSV file had headers and we told ``genfromtxt`` to use them with the
``#(names true)`` option, we can easily extract the $$x$$ and $$y$$ axis data.
Let's look at their values separately:

```lfe
> (set xs (lsci-np:get data 'x))
#($erlport.opaque python ...)
> (set ys (lsci-np:get data 'y))
#($erlport.opaque python ...)
> (lsci-np:->list xs)
(-6.860120914 -4.324130045 -4.358625055 -4.358426747 -6.955852379
 -6.661145254 -6.355462942 -6.118102026 -7.115148017 -6.815308569
 -6.519993057 -6.204119983 -5.853871964 -6.109523091 -5.79832982
 -5.482672118 -5.171791386 -4.851705903 -4.517126416 -4.143573228
 -3.709075441 -3.499489089 -6.300769497 -5.953504836 -5.642065153
 -5.031376979 -4.680685696 -4.329846955 -3.928486195 -8.56735134 ...)
> (lsci-np:->list ys)
(0.8116 0.9072 0.9052 0.9039 0.8053 0.8377 0.8667 0.8809 0.7975
 0.8162 0.8515 0.8766 0.8885 0.8859 0.8959 0.8913 0.8959 0.8971
 0.9021 0.909 0.9139 0.9199 0.8692 0.8872 0.89 0.891 0.8977 0.9035
 0.9078 0.7675 ...)
```

Now let's plot our data in the terminal:

```lfe
> (lsci-asciiplot:scatter xs ys)
```

Which will give you something like this:

```
                                                                           oo  o o
                                                                      o o
                                                                  o  o   o
                                                               oo
                                                        o o o
                                          o      o o  o o  o
                                        ooo ooo      o
                                     oo o
                                    oooo
                                    oo
                                 ooo
                                  o
                                 o
                               o
                               oo

                             o
                              o
                             o

                           o
                          o o
                         o o

                       oo o
                       o
                     o
                   oo
o           o  ooo
     oooo oo
o oo
o

ok
```


## Curve Fitting

The [NIST data set](http://www.itl.nist.gov/div898/strd/lls/data/LINKS/DATA/Filip.dat)
provided a polynomial describing this data:

```
y = B0 + B1*x + B2*(x**2) + ... + B9*(x**9) + B10*(x**10) + e
```

Or, if you prefer LaTeX:

\begin{equation}
y = B_0 + B_1{x} + B_2{x^2} + ... + B_9{x^9} + B_{10}{x^{10}} + e
\end{equation}

Using NumPy, we can easily fit a 10th-degree polynomial curve to this data.
We will use ``numpy.polyfit`` for finding a least squares polynomial fit,
passing it the $$x$$ and $$y$$ values for the data to fit as well as the degree
of our polynomial:


```lfe
> (set coeffs (lsci-np:polyfit xs ys 10))
#($erlport.opaque python ...)
```

Let's peek at the data:

```lfe
> (lsci-np:->list coeffs)
(-4.029625205186532e-5 -0.0024678107546401437 -0.06701911469215643
 -1.0622149736864719 -10.875317910262362 -75.12420087227511
 -354.47822960532113 -1127.973927925715 -2316.371054759451
 -2772.1795597594755 -1467.4895971641686)
```

``numpy.polyfit`` can return more data, if you are so inclined, by passing the
``#(full true)`` option:

```lfe
> (set `#(,coeffs
          ,residuals
          ,rank
          ,singular-values
          ,rcond) (lsci-np:polyfit xs ys 10 '(#(full true))))
#($erlport.opaque python ...)
> (lsci-np:->list coeffs)
(-4.029625205186532e-5 -0.0024678107546401437 -0.06701911469215643
 -1.0622149736864719 -10.875317910262362 -75.12420087227511
 -354.47822960532113 -1127.973927925715 -2316.371054759451
 -2772.1795597594755 -1467.4895971641686)
> (lsci-np:->list residuals)
(7.958513839371895e-4)
> rank
11
> (lsci-np:->list singular-values)
(3.128894711145785 1.064548669029962 0.27180324022363517
 0.05296821542551952 0.008387108325776571 0.0010157565988992792
 9.583030547029836e-5 7.605115790256685e-6 4.6491044714423815e-7
 1.9871421381342612e-8 6.009222284310632e-10)
> (lsci-np:->list rcond)
1.8207657603852567e-14
```

There is a convenience class in NumPy ``numpy.poly1d`` that, once instantiated
with our fit data, we can use to evaluate at any given point. Let's try it
out:

```lfe
> (set model (lsci-np:poly1d coeffs))
#($erlport.opaque python ...)
```

Let's call this function against several values as a sanity check:

```lfe
> (lsci-np:->list (py:func model '(-9)))
0.7766886098502255
> (lsci-np:->list (py:func model '(-7)))
0.7990591787051926
> (lsci-np:->list (py:func model '(-6)))
0.8860483219018533
> (lsci-np:->list (py:func model '(-5)))
0.8926343904781788
> (lsci-np:->list (py:func model '(-4)))
0.9094348679923314
> (lsci-np:->list (py:func model '(-3)))
0.8893022773313533
```

By examining our original data set, we can see that these check out just fine.


## Polynomial Linear Regression

Next let's see if our linear model matches up with what NIST provided. We're
going to need to calculate the
[coefficient of determination](http://en.wikipedia.org/wiki/Coefficient_of_determination),
or $$R^2$$ -- a value that indicates how well a statistical model fits with
measured data. We'll start by feeding our $$x$$ values into our model:

```lfe
> (set y-predicted (py:func model `(,xs)))
#($erlport.opaque python ...)
> (lsci-np:->list y-predicted)
(0.8115567059126079 0.9058214190752096 0.9051532611576931
 0.9051572417567968 0.8027951904375641 0.8316306323870322
 0.8619032382121077 0.8798465345066688 0.7904039687107343
 0.8159121506946576 0.8461646108687546 0.8741409704916805
 0.8911095569417284 0.880361461470784 0.8923272580695993
 0.8938414797164569 0.8922115816051246 0.8943421465901338
 0.9015888194142008 0.908406582014095 0.9117908062162314
 0.9177642755723809 0.8666215053945052 0.8879651108427424
 0.8940117353472488 0.8924367760930636 0.897684962889798
 0.9057141589178173 0.9097792825882607 0.7667388250863496 ...)
```

We will also need several other values in order to calculate $$R^2$$, per the
equation given on the Wikipedia page linked above:

* The mean value of our observed (original) data:
  $$\bar{y}=\frac{1}{n}\sum_{i=1}^n y_i$$
* The total sum of squares : $$SS_\text{tot}=\sum_i (y_i-\bar{y})^2$$
* The regression sum of squares : $$SS_\text{reg}=\sum_i (f_i -\bar{y})^2$$
* The sum of squares of residuals : $$SS_\text{res}=\sum_i (y_i - f_i)^2$$

We already have the following:

* $$y_i$$: the $$y$$ values from the observed (NIST) data
* $$f_i$$: the values generated by our model

With these, we will be able to calculate $$R^2$$:

\begin{equation}
R^2 \equiv 1 - {SS_{\rm res}\over SS_{\rm tot}}
\end{equation}

Here are the calculations:

```lfe
> (set y-mean (lsci-np:/ (lsci-np:sum ys) (lsci-np:size ys)))
#($erlport.opaque python ...)
> (set ss-tot (lsci-np:sum (lsci-np:^ (lsci-np:- ys y-mean) 2)))
#($erlport.opaque python ...)
> (set ss-reg (lsci-np:sum (lsci-np:^ (lsci-np:- y-predicted y-mean) 2)))
#($erlport.opaque python ...)
> (set ss-res (lsci-np:sum (lsci-np:^ (lsci-np:- ys y-predicted) 2)))
#($erlport.opaque python ...)
```

Instead of ``(lsci-np:^ ...)`` you may use ``(lsci-np:** ...)``, if you
prefer, e.g.:

```lfe
> (set ss-res (lsci-np:sum (lsci-np:** (lsci-np:- ys y-predicted) 2)))
#($erlport.opaque python ...)
```

Those use shortened aliases; you may prefer the long form:

```lfe
> (set y-mean (lsci-np:divide (lsci-np:sum ys) (lsci-np:size ys)))
#($erlport.opaque python ...)
> (set ss-tot (lsci-np:sum (lsci-np:power (lsci-np:subtract ys y-mean) 2)))
#($erlport.opaque python ...)
> (set ss-reg (lsci-np:sum (lsci-np:power (lsci-np:subtract y-predicted y-mean) 2)))
#($erlport.opaque python ...)
> (set ss-res (lsci-np:sum (lsci-np:power (lsci-np:subtract ys y-predicted) 2)))
#($erlport.opaque python ...)
```

Let's sanity-check the results:

```lfe
> (lsci-np:->float y-mean)
0.8495756097560976
> (lsci-np:->float ss-tot)
0.2431874712195122
> (lsci-np:->float ss-reg)
0.24239162093851477
> (lsci-np:->float ss-res)
7.958513853880548e-4
```

Now we're ready to get the $$R^2$$ value for our model:

```lfe
> (set r-squared (lsci-np:- 1 (lsci-np:/ ss-res ss-tot)))
#($erlport.opaque python ...)
> (lsci-np:->float r-squared)
0.9967274161723995
```

If we compare this to the value from the original NIST data file,
``0.996727416185620 ``, we see that our model did pretty well:

```lfe
> (lsci-np:->float (lsci-np:- r-squared 0.996727416185620))
-1.3220535777236364e-11
```

That's a pretty tiny difference!


## A Linear Model Class

The linear model code above is a bit cumbersome; it would be much more
convenient for multiple-use if there was a Python class that did all the
arithmetic for us, and we could just get attribute values ... So that's
what we created:

```python
class PolynomialLinearModel:
    "A convenience class for creating polynomial linear models."
    def __init__(self, xs, ys, degree):
        (self.xs, self.ys) = (xs, ys)
        self.degree = degree
        self.y_mean = self.get_y_mean()
        (self.results, self.model, self.ys_predicted,
         self.ss_tot, self.ss_reg, self.ss_res,
         self.r_squared) = (None, None, None, None, None, None, None)
        (self.coeffs, self.residuals, self.rank,
         self.singular_values, self.rcond) = (None, None, None, None, None)
        self.polyfit()

    def polyfit(self):
        (self.coeffs, self.residuals, self.rank,
        self.singular_values, self.rcond) = np.polyfit(
            self.xs, self.ys, self.degree, full=True)
        self.model = np.poly1d(self.coeffs)
        self.ys_predicted = self.model(self.xs)
        self.ss_tot = self.get_ss_tot()
        self.ss_reg = self.get_ss_reg()
        self.ss_res = self.get_ss_res()
        self.r_squared = self.get_r_squared()
        self.results = {
            "coeffs": self.coeffs.tolist(),
            "residuals": self.residuals.tolist(),
            "rank": self.rank,
            "singular-values": self.singular_values.tolist(),
            "rcond": float(self.rcond),
            "y-mean": float(self.y_mean),
            "ss-tot": float(self.ss_tot),
            "ss-reg": float(self.ss_reg),
            "ss-res": float(self.ss_res),
            "r-squared": float(self.r_squared)}

    def predict(self, xs):
        return self.model(xs)

    def get_y_mean(self):
        return self.ys.sum() / self.ys.size

    def get_ss_tot(self):
        return ((self.ys - self.get_y_mean()) ** 2).sum()

    def get_ss_reg(self):
        return ((self.ys_predicted - self.get_y_mean()) ** 2).sum()

    def get_ss_res(self):
        return ((self.ys - self.ys_predicted) ** 2).sum()

    def get_r_squared(self):
        return 1 - self.get_ss_res() / self.get_ss_tot()

    def __str__(self):
        return str(self.results)

    def __repr__(self):
        return self.__str__()
```

This has been saved to the Python module ``lsci.numpysupl`` with the
``lsci-np`` wrapper function ``lsci-np:poly-linear-model`` added. Now we can
easily create a linear model which provides everything needed in one go:

```lfe
> (set model (lsci-np:poly-linear-model xs ys 10))
#($erlport.opaque python ...)
> (py:ptype model)
lsci.numpysupl.PolynomialLinearModel
> (py:attr model 'results)
#("dict"
  (#("rcond" 1.8207657603852567e-14)
   #("rank" 11)
   #("coeffs"
     (-4.029625205186532e-5 -0.0024678107546401437
      -0.06701911469215643 -1.0622149736864719 -10.875317910262362
      -75.12420087227511 -354.47822960532113 -1127.973927925715
      -2316.371054759451 -2772.1795597594755 -1467.4895971641686))
   #("residuals" (7.958513839371895e-4))
   #("ss-reg" 0.24239162093851477)
   #("singular-values"
     (3.128894711145785 1.064548669029962 0.27180324022363517
      0.05296821542551952 0.008387108325776571 0.0010157565988992792
      9.583030547029836e-5 7.605115790256685e-6
      4.6491044714423815e-7 1.9871421381342612e-8
      6.009222284310632e-10))
   #("r-squared" 0.9967274161723995)
   #("ss-res" 7.958513853880548e-4)
   #("y-mean" 0.8495756097560976)
   #("ss-tot" 0.2431874712195122)))
```

We can also extract only what we need:

```lfe
> (set r-squared (py:attr model 'r-squared))
#($erlport.opaque python ...)
> (lsci-np:->float r-squared)
0.9967274161723995
```

When we created our first model, we ran it against several values to see if
the outputs fit our measured data. One of those was the value ``-9`` which
returned ``0.77668860985022548``. Let's try that again with our new object:

```lfe
> (lsci-np:->float (py:method model 'predict '(-9)))
0.7766886098502255
```

Nice.


## Plotting the Model with the Observed Data

We're going to need some data to feed to our fitted-poly function so that it
can create the smooth polynomial curve that we will overlay on our scatter
plot. Let's create a linear space between our minimum and maximum x values
(200 points should give us a nice, smooth curve). Then let's use fitted-poly
to generate the y values:



```lfe
> (set xs-fitted
    (lsci-np:linspace
      (py:method xs 'min)
      (py:method xs 'max)
      `(#(num 200))))
#($erlport.opaque python ...)
> (set ys-fitted
    (py:method model 'predict `(,xs-fitted)))
```

Now we're ready to put them together:

```lfe
> (defun plot-both ()
    (lsci-asciiplot:scatter xs ys)
    (lsci-asciiplot:line
      xs-fitted ys-fitted
      '(#(hold true))))
plot-both
```

When we call our plot function:

```lfe
> (plot-both)
```

We get the following, which shows the scatter plot of the original data
(``o`` markers) with the polynomial curve fit overlaid (``-`` markers):

```
                                                                           o-----o
                                                                      o o  --    -
                                                                  o  o ----
                                                               ---------
                                                        o o ----
                                          o      o o  o o---
                                        o----------------
                                     oo--
                                    o--o
                                    --
                                 oo--
                                  --
                                 o-
                               o -
                               o-
                               -
                             o -
                              -
                             -
                            --
                           o-
                          o-o
                         o--
                         --
                       o--o
                       --
                     o--
                   oo--
o           o  ooo --
     ooo------------
---------
o-

ok
```


## Conclusion

And there you have it! Fast polynomial curve-fitting from LFE using Python,
NumPy, and SciPy. The work has only just begun on lsci, so if this sort of
thing floats your boat, be sure to take a look at
[some of the development tasks](https://github.com/lfex/lsci/issues) and
come give us a hand!

Even though there's a lot to do, there's a lot of reward :-) Every little
function that gets converted
brings enormous satisfaction: the ability to perform these sorts of computing
tasks without having to leave the Erlang ecosystem is a wonderful change. We
hope it's one that you not only get used to, but can't live without :-)
