# [Conduit packages](https://github.com/tfeb/conduit-packages "Conduit packages")
This system defines a way of treating Common Lisp packages as 'conduits' which can sit between one or more implementation packages and users of those packages.  Conduits try to be dynamic: assuming you modify the packages for which they are  conduits using the functionality defined in this system, then conduit packages will notice the changes and recompute themselves.

This system also defines a way of making clones of packages which try to be as identical to them as possible.  Clones are not dynamic, but rather point-in-time copies.

The most important interface to this is through a variant version of the `defpackage` macro, which supports some extra options.  This version of `defpackage` works by checking for options it cares about and then deferring to the underlying implementation for anything it does not care about.  This means that it can sit on top of an underlying implementation which has options other than the standard CL ones.  Some other standard package functions are also overridden to support the dynamic behaviour of conduits.

## Packages in this system
All packages have domain-structured names.

- `org.tfeb.conduit-packages` is the implementation package.  It exports `defpackage`and some modified versions of other package functionality, as well as a couple of other names.
- `org.tfeb.cl/conduits` (nickname `org.tfeb.clc`) is a variant version of the standard-defined `common-lisp` package – in fact, a conduit package – which exports all of the symbols in `cl` except for some package-related ones, which are replaced with the appropriate versions from `org.tfeb.conduit-packages`.  You can use this package where you would normally use the `cl` package.
- `org.tfeb.cl-user/conduits` (nickname `org.tfeb.clc-user`) is a `cl-user` style package which uses `org.tfeb.clc` instead of `cl`.  This package is useful as a scratch package in the same way `cl-user` is.

Note that it will not (and can not) work, in any package which uses `cl`, to simply say `(use-package :org.tfeb.conduit-packages)`, as there are inevitable name clashes.  Instead you should either define a package which uses `org.tfeb.clc`, or define your own conduit package (see below).

## Conduits by example
All the examples below assume that the current package is `org.tfeb.clc-user`, so that `defpackage` means `org.tfeb.conduit-packages:defpackage` for instance.

Let's say I'm writing a program which consists of several implementation packages, say `com.cley.my-great-prog.clever-hacks`, `com.cley.my-great-prog.not-so-clever-hacks` and `com.cley.my-great-prog.outright-misfeatures`.  The definitions of these three packages might be:

```lisp
(defpackage :com.cley.my-great-prog.clever-hacks
  (:use :cl)
  (:export #:cause-fire))

(defpackage :com.cley.my-great-prog.not-so-clever-hacks
  (:use :cl)
  (:export #:cause-serious-fire))

(defpackage :com.cley.my-great-prog.outright-misfeatures
  (:use :cl)
  (:export #:fail-to-put-out-fire))
```

Now I want to provide a single package, `com.cley.my-great-program` which combines the functionality of the three packages:

```lisp
(defpackage :com.cley.my-great-prog
  (:use)
  (:extends :com.cley.my-great-prog.clever-hacks)
  (:extends :com.cley.my-great-prog.not-so-clever-hacks)
  (:extends :com.cley.my-great-prog.outright-misfeatures))
```

And now

```lisp
> (do-external-symbols
      (s (find-package :com.cley.my-great-prog))
    (format t "~&~A from ~A~%"
            (symbol-name s)
            (package-name (symbol-package s))))
FAIL-TO-PUT-OUT-FIRE from COM.CLEY.MY-GREAT-PROG.OUTRIGHT-MISFEATURES
CAUSE-FIRE from COM.CLEY.MY-GREAT-PROG.CLEVER-HACKS
CAUSE-SERIOUS-FIRE from COM.CLEY.MY-GREAT-PROG.NOT-SO-CLEVER-HACKS
```

So `com.cley.my-great-prog` serves as a *conduit* for the various implementation packages of the program, which means that users of the program don't have to worry about what the implementation packages are.

Conduits are dynamic.  If I now decide that the `com.cley.my-great-prog.clever-hacks` package should export some other symbols, I can simply redefine it:

```lisp
(defpackage :com.cley.my-great-prog.clever-hacks
  (:use :cl)
  (:export #:cause-fire
   #:light-match #:burn-petrol))
```

and now

```lisp
(do-external-symbols
      (s (find-package :com.cley.my-great-prog))
    (format t "~&~A from ~A~%"
            (symbol-name s)
            (package-name (symbol-package s))))
FAIL-TO-PUT-OUT-FIRE from COM.CLEY.MY-GREAT-PROG.OUTRIGHT-MISFEATURES
BURN-PETROL from COM.CLEY.MY-GREAT-PROG.CLEVER-HACKS
CAUSE-FIRE from COM.CLEY.MY-GREAT-PROG.CLEVER-HACKS
LIGHT-MATCH from COM.CLEY.MY-GREAT-PROG.CLEVER-HACKS
CAUSE-SERIOUS-FIRE from COM.CLEY.MY-GREAT-PROG.NOT-SO-CLEVER-HACKS
```

The conduit package noticed the redefinition of a package for which it was a conduit, and changed its exports appropriately.

The dynamic behaviour of conduit packages is a little fragile: it will work so long as you use the conduits-provided versions of `defpackage`, `export` and so on, but it *won't* work if you use the standard CL ones, either explicitly or implicitly, during error recovery say.  If the system gets  in a mess, you can always call `org.tfeb.conduit-packages:recompute-conduits` to recompute everything.  The dynamic behaviour of conduit packages is meant to be a help for when writing a program, during which process packages often get reorganised fairly frequently: it's not something that should be relied on for production.

Conduits can export only part of the packages for which they are conduits.  For instance, perhaps I don't want the `burn-petrol` feature:

```lisp
(defpackage :com.cley.my-great-prog
  (:use)
  (:extends/excluding :com.cley.my-great-prog.clever-hacks
   #:burn-petrol)
  (:extends :com.cley.my-great-prog.not-so-clever-hacks)
  (:extends :com.cley.my-great-prog.outright-misfeatures))
```

Or perhaps I *only* want to burn petrol:

```lisp
(defpackage :com.cley.my-great-prog
  (:use)
  (:extends/including :com.cley.my-great-prog.clever-hacks
   #:burn-petrol)
  (:extends :com.cley.my-great-prog.not-so-clever-hacks)
  (:extends :com.cley.my-great-prog.outright-misfeatures))
```

In these two latter cases the symbols you are excluding or including need to exist in the package being extended: you can't, for instance, add an exclusion for a symbol which only may be there, or which may be there in future.  That might be a nice feature to add.

Using inclusions and exclusions like this allows you to construct conduit packages which are 'like' underlying packages but have some small differences, such as replacing some functionality.  `org.tfeb.clc` is such a package: it is a conduit which extends `cl` but replaces some of its functionality.  Here is its definition:

```lisp
(defpackage :org.tfeb.cl/conduits
  (:use)
  (:nicknames :org.tfeb.clc)
  (:extends/excluding :cl #:export #:unexport #:defpackage
   #:delete-package #:rename-package)
  (:extends/excluding :org.tfeb.conduit-packages
   #:recompute-conduits #:*underlying-implementation-map*))
```

Note that `org.tfeb.clc-user` is *not* a conduit: it's just a package which uses `org.tfeb.clc` rather than `cl`:

```lisp
(defpackage :org.tfeb.cl-user/conduits
  (:nicknames :org.tfeb.clc-user)
  (:use :org.tfeb.clc))))
```

## Utilities
The `org.tfeb.conduit-packages` package (but *not* `org.tfeb.clc`) exports two other things.

**`recompute-conduits`**.  This is a function that will recompute all the conduit packages that exist: it's useful if things have become confused, or if you just want to make sure that they definitely are not confused.

**`*underlying-implementation-map*`**.  This is an alist which maps between the names of the functions and macros for which the `org.tfeb.conduit-packages` provides implementations and the names of the equivalent underlying implementations that it must call.  So, for instance, in this alist, by default, is an entry `(org.tfeb.conduit-packages:defsystem . cl:defsystem)` which tells the system what the underlying implementation of the `defsystem` macro is.

The purpose of this alist is so that, if need be, this system could be layered on top of some *other* system which *also* provides versions of, say, `defsystem` or `export` &c.  Unless you want to use it in that context you never need to worry about this variable.

## Package cloning
Cloning a package is making a package which is 'like' it: all of its internal, external, and shadowing symbols, as well as its used packages will be the same as the package it clones, but in addition any other things asked for by the `defpackage` form will be done.  Once a clone is made it lives an independent life to the the package it clones: clones are not dynamic, and don't remember what package they were cloned from.  Clones also can't be conduits.

Here is an example of making a clone:

```lisp
(defpackage :org.tfeb.foo
  (:use :org.tfeb.clc)
  (:export #:spot))

(intern "FUG" ':org.tfeb.foo)

(defpackage :org.tfeb.bar
  (:use :org.tfeb.clc)
  (:clones :org.tfeb.foo)
  (:export #:spit))
```

Now

```lisp
>  (eq 'org.tfeb.foo:spot 'org.tfeb.bar:spot)
t

> (eq 'org.tfeb.foo::fug 'org.tfeb.bar::fug)
t

> (eq 'org.tfeb.foo::fog 'org.tfeb.bar::fog)
nil

> 'org.tfeb.bar:spit
org.tfeb.bar:spit

> 'org.tfeb.foo:spit

Error: Symbol "SPIT" not found at all in the ORG.TFEB.FOO package.
[...]
```

The idea behind package clones was to allow you to make a quick-and-dirty point-in-time copy of a package in which you could then experiment without contaminating the namespace of the cloned package.  Their intended use was on LispMs which took a long time to reboot: in practice I think I have almost never used them.

## Two approaches to conduits
When I first wrote conduit packages I was concerned about fasl size, so the expansion of `defpackage`first used `cl:defpackage` to define a simple package, and then walked over the packages it was extending and modified it appropriately.  This made for small fasls, but meant that you often got warnings when compiling & loading files, because `cl:defpackage` would redefine a package which was incompatible with its current state when the file was loaded.

I'm less concerned with fasl sizes now, so I have changed the implementation to compute an enormous `cl:defpackage` form instead.  For file containing only, say `(defpackage :foo (:use) (:extends :cl))` this can cause a factor of ten increase in fasl size (from about 3KB to about 22kB in one implementation), but it means you don't get annoying warnings.  Given that everything else around CL has bloated by hugely more than a factor of ten since the late 1990s, I think this is a price worth paying.

## Notes
Conduit packages should generally be defined with `(:use)` so the used-package list is empty rather than an implementation-default.  A conduit really is just that: it doesn't need to use any packages because it's never a package where anything is defined.

The `defsystem` macro uses `*underlying-implementation-map*` to know what the underlying `defsystem` is, and so this variable matters at macro-expansion time as well as at runtime.

## Building
There is an ASDF system definition, but in fact simply compiling and loading `conduit-packages.lisp` should be enough: there are no dependencies.

All of this system should be portable CL: if it's not that's a bug.

---

Conduit packages is copyright 1998-2002, 2020-2021 by Tim Bradshaw.  See `LICENSE` for the license.