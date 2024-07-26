# [Conduit packages](https://github.com/tfeb/conduit-packages "Conduit packages")
A conduit package is a package which sits between one or more implementation packages and users of those packages.  As an example imagine you have a system which has two implementation packages: `a` and `b`.  But you want users of your system to simply be able to use a single package to get all the functionality of the system.  Then a conduit package, say `c`, can import and then reexport all the external symbols from both `a` and `b`.  Or it could reexport only some subset of those symbols.  This is something which is perfectly possible just using normal package definitions, but it's a pain to manage as you have to keep lists of exports up to date.  It's also not very dynamic: if the underlying packages change you need to update and redefine the conduits manually.

The conduit packages system provides tools which help you define and manage conduit packages like this easily as well as two other things.

1. Conduit packages themselves are supported by a variant version of `defpackage`together with variants of some of the CL package functions which are aware of conduits and deal with keeping them up to date.  Assuming you modify packages using the functionality defined in this system, then conduit packages will notice changes you make and recompute themselves.
2. The variant `defpackage` is itself extensible.  This extensibility is used to support the new options needed for conduit packages, but it can be further extended by other systems.
3. Finally there is a way of making clones of packages which try to be as identical to them as possible.  Clones are not dynamic, but rather point-in-time copies.

Conduits and the extensible `defpackage` variant are independently useful, or I think so.  Package cloning probably is not: it was once useful on Lisp Machines where you *really* didn't want to break things too much because it took half an hour to reboot the machine, so instead you might work by making a clone of a package, doing some work in the clone and then deleting it and starting again.  Today, I can restart a lisp and rebuild everything from cold in a few seconds: cloning is not very useful any more.  The extensible `defpackage`variant, together with the reengineering of conduits to use it, is recent: see further down for some history.

## Systems and packages
There are two systems which provide various mixes of features.

### The `org.tfeb.conduit-packages` system
This is the guts of everything.  It defines three packages[^1].

- `org.tfeb.conduit-packages` is the implementation package.  It exports everything, including symbols which have names which conflict with symbols in `CL`.  Generally, you want to interface to this package by defining packages which are themselves conduits of it.
- `org.tfeb.cl/conduits` (nickname `org.tfeb.clc`).  This is a package which is a conduit for both `CL` and `org.tfeb.conduit-packages`: it reexports all the symbols in `CL`, except that its `defpackage`and some other package-related names come from `org.tfeb.cl/conduits`.  If you define a package which uses this (you can't use both it and `CL`) you will get a `CL` variant where `defpackage` understands the conduit (and package cloning) options as well as the normal ones.
- `org.tfeb.cl-user/conduits` (nickname `org.tfeb.clc-user`) is such a package: it is a package which uses only `org.tfeb.cl/conduits` and can be used for experimentation and small programs the way `CL-USER` is.  One of its main uses in practice is as a package in which to place package declarations for other packages.

Note that it will not (and can not) work, in any package which uses `CL`, to simply say `(use-package :org.tfeb.conduit-packages)`, as there are inevitable name clashes.  Instead, you should either define a package which uses `org.tfeb.clc`, define your own conduit package (see below), or use the `org.tfeb.conduit-packages/define-package` package described below.

### The `org.tfeb.conduit-packages/define-package` system
This is a shim system which provides all the conduits functionality under names which do not clash with names in `CL`.

It provides one package: `org.tfeb.conduit-packages/define-package`: this package exports the variant `defpackage` under its native name, `define-package`, some shims for the conduit packages variants of package functionality, as well as all the names associated with managing conduit packages, extending `define-package` itself and so on.

Almost certainly this is the package to use if you want to use conduit packages and are happy to say `define-package` rather than `defpackage`.

## Conduits by example
### The old way using `defpackage`
These examples assume that the current package is `org.tfeb.clc-user`, so that `defpackage` means `org.tfeb.conduit-packages:defpackage` for instance.

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

The dynamic behaviour of conduit packages is a little fragile: it will work so long as you use the conduits-provided versions of `defpackage`, `export` and so on, but it *won't* work if you use the standard CL ones, either explicitly or implicitly, during error recovery say.  If the system gets itself in a mess, you can always call `org.tfeb.conduit-packages:recompute-conduits` to recompute everything.  The dynamic behaviour of conduit packages is meant to be a help for when writing a program, during which process packages often get reorganised fairly frequently: it's not something that should be relied on for production.

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

Using inclusions and exclusions like this allows you to construct conduit packages which are 'like' underlying packages but have some small differences, such as replacing some functionality.  `org.tfeb.clc` is such a package: it is a conduit which extends `CL` but replaces some of its functionality.  Here is its definition[^2]:

```lisp
(defpackage :org.tfeb.cl/conduits
  (:use)
  (:nicknames :org.tfeb.clc)
  (:extends/excluding :cl
   #:export #:unexport #:defpackage
   #:delete-package #:rename-package)
  (:extends/including :org.tfeb.conduit-packages
   #:export #:unexport #:defpackage
   #:delete-package #:rename-package))
```

You can see that it just replaces a set of exports from `CL` with names from `org.tfeb.conduit-packages`.

Note that `org.tfeb.clc-user` is *not* a conduit: it's just a package which uses `org.tfeb.clc` rather than `CL`:

```lisp
(defpackage :org.tfeb.cl-user/conduits
  (:nicknames :org.tfeb.clc-user)
  (:use :org.tfeb.clc))
```

### The new way using `define-package`
These examples assume the package is `CL-USER`.  Assume there are two packages, `com.cley.program-1` and `com.cley.program-2` both of which export a class called `onion`.  We want to define a conduit which exports only one of these.  Here is how to do that:

```lisp
(in-package :cl-user)

(use-package :org.tfeb.conduit-packages/define-package)

(define-package :org.tfeb.program
  (:use)
  (:extends :org.tfeb.program-1)
  (:extends/excluding :org.tfeb.program-2
   #:onion))
```

Here's a slightly larger example where four packages are involved:

```lisp
(in-package :cl-user)

(use-package :org.tfeb.conduit-packages/define-package)

(define-package :org.tfeb.substrate
  (:use :cl)
  (:export #:vegetable))

(define-package :org.tfeb.program-1
  (:use :cl)
  (:use :org.tfeb.substrate)
  (:export #:onion #:shallot))

(define-package :org.tfeb.program-2
  (:use :cl)
  (:use :org.tfeb.substrate)
  (:export #:onion #:leek))

(define-package :org.tfeb.program
  (:use)
  (:extends/including :org.tfeb.substrate
   #:vegetable)
  (:extends :org.tfeb.program-1)
  (:extends/excluding :org.tfeb.program-2
   #:onion))
```

### A simpler way using `define-conduit-package`
If all you want to do is define a pure conduit, then `define-conduit-package` provides a shim for `define-package` which makes it simpler and checks for common mistakes.   It is exactly like `define-package` except:

- it inserts a `(:use)` clause;
- it checks for `(:use x)` clauses and signals a restartable error if it finds any.

```lisp
(define-conduit-package :org.tfeb.program
  (:extends/including :org.tfeb.substrate
   #:vegetable)
  (:extends :org.tfeb.program-1)
  (:extends/excluding :org.tfeb.program-2
   #:onion))
```

is equivalent to the final `define-package` form above.  The real advantage of this form is:

```lisp
> (define-conduit-package :foo
    (:use :cl)
    (:extends :cl))

Error: Conduit foo uses other packages
  1 (continue) Blunder on with this likely-bogus conduit
  2 Use other clauses (interactively: remove the offending clauses)
  3 (abort) Return to top loop level 0.

1 > :c 2
```

### Package cloning
Cloning a package is making a package which is 'like' it: all of its internal, external, and shadowing symbols, as well as its used packages will be the same as the package it clones, but in addition any other things asked for by the `defpackage` form will be done.  Once a clone is made it lives an independent life to the package it clones: clones are not dynamic, and don't remember what package they were cloned from.  Clones can't also be conduits.

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

The idea behind package clones was to allow you to make a quick-and-dirty point-in-time copy of a package in which you could then experiment without contaminating the namespace of the cloned package.  Their intended use was on LispMs which took a long time to reboot: in practice I think I have almost never used them and I don't think they are useful now.

## In detail
Here is at least some documentation of the exports of `org.tfeb.conduit-packages` itself.

### `define-package` and `defpackage`
These two macros are the same (`defpackage` is now a shim for `define-package`) and provide an extensible variant of `defpackage`.  The extensions take the form of new clauses which the macros understand.  The pre provided nonstandard clauses are:

- `(:extends <package>)` to reexport all the external symbols in `<package>`;
- `(:extends/including <package> [<name> ...])` to reexport only the external symbols designated by the specified names from `<package>`;
- `(:extends/excluding <package> [<name> ...])` to reexport all the external symbols *except* those designated by the specified names from `<package>`
- `(:clones <package>)` to make a point-in-time clone of `<package>` (see above).

All of these clauses can also be spelled in the singular: `(extend <package>)` for historical reasons.

These clauses are all implemented by a predefined mechanism for `define-package`: see below.

In addition the semi-standard `:local-nicknames` clause is passed down to the underlying `defsystem` by default: other clauses like this can be added (see below).

### `define-conduit-package`
`define-conduit-package` provides a slightly simpler way of defining pure conduits.  It is exactly like `define-package` but:

- it interpolates a `(:use)` clause;
- it checks the provided clauses for `:use` clauses with a non-empty list of packages, and signals a restartable error, with a restart which offers to expunge these clauses.

So it means you have to type slightly less, and also catches a common mistake.

### Variant package functions, and a utility
These functions call their CL equivalents but also do conduits maintenance.

- **`export`**
- **`unexport`**
- **`delete-package`**
- **`rename-package`**

See below on how the CL equivalents are called and how you can replace them.

In addition there is a single utility function, **`recompute-conduits`**.  This takes no arguments and will recompute all conduit packages, causing them to take notice of any changes in the packages they are conduits for.  This is useful if, for instance, something has called one of the underlying CL package functions directly.

### The `define-package` / `defpackage` extension mechanism
To define an extension for `define-package` you need to pick a name for it, and add that name to the list of known 'mechanisms' for the macro.  You then write `eql` methods for three generic functions keyed on the name of the extension.

**`*define-package-mechanisms*`** is the list of known mechanisms for `define-package`.  It is a list of symbols, which by default contains two entries: one for the conduits mechanism and one for the basic CL mechanism (which itself is slightly configurable, see below).

**`initial-define-package-state`** is a generic function called with three arguments for each mechanism in the list:

- the mechanism name, a symbol;
- the name of the package being defined (first argument to `define-package`);
- all the clauses to `define-package`.

It is called at the start of clause processing and its return value is a 'state' which is some object handed to and returned from the other generic functions in the protocol.  A state can be any object at all.

Mechanisms in `*define-package-mechanisms*` should define an `eql` method for their name on the first argument of this function.  There is no fallback method, so any missing methods will cause an error.

**`process-define-package-clause`** is called for each mechanism and clause in the body of the `define-package` form.  Its arguments are:

- the mechanism name, a symbol;
- the keyword of the `define-package` clause, a keyword (for instance `:use`);
- the whole `define-package` clause being processed, a list (for instance `(:use :cl)`);
- the current state for the mechanism;
- the name of the package being defined;
- all the `define-package` clauses.

It should return two values:

- a state object which may incorporate information about the clause;
- non-`nil` if it handled the clause.

Note that each mechanism gets each clause: more than one mechanism can handle a given clause, but each clause needs to be handled by at least one mechanism.  Also note that the state object returned by a call to `process-define-package-clause` is what it passed to the next call.  The initial state comes from `initial-define-package-state` for that mechanism and the final state will be passed to `compute-define-package-form`.

Again, mechanisms should define `eql` methods for their name on this generic function.  If you wanted to, you could define methods which have two `eql` specializers: one for the mechanism name and one for the clause key.  In practice, I've done the second part by a `case` clause in the body.

**`compute-define-package-form`** is the final generic function in the protocol.  Its job is to compute and return parts of the expansion of `define-class`.  It is called for each mechanism with four arguments:

- the mechanism name, a symbol;
- the final state for this mechanism;
- the name of the package being defined;
- all the `define-package` clauses.

It should return three values:

- a list of forms which will be evaluated before the final `defpackage`form resulting from the expansion;
- a list of clauses to add to the body of the `defpackage` form;
- a list of forms which will be evaluated after the `defpackage` form.

Some or all of these values may be `()` of course.

The result of all this is that the defined mechanisms all get to contribute to the expansion of the `define-package` form.

**Notes.**

- There is limited control over the relative ordering of the various mechanisms, but the forms before the `defpackage` form will be in the reverse order of the mechanisms in `*define-package-mechanisms*`and the clauses in the body and forms after it in the order of the mechanisms.  This is a little like around methods.
- All the forms before and after the main body of the expansion are wrapped in `(eval-when (:compile-toplevel :load-toplevel :execute)` forms.
- All the generic functions get handed the package name and all the clauses just in case they want to, for instance, check for incompatible clauses, and so they can report errors and warnings in useful ways.

### An example: the common lisp mechanism
This predefined mechanism listens to one variable.

**`*extended-cl-define-package-clause-keys*`** contains clause keys which the common lisp mechanism should consider to be standard as well as those defined by the language.  By default, it contains `:local-nicknames` and perhaps some implementation-specific clause keys.  You can add clause keys to this to let the mechanism know it should pass them down to the underlying macro.

Here is the entire mechanism:

```lisp
(defmethod initial-define-package-state ((mechanism (eql 'common-lisp))
                                     name clauses)
  (declare (ignore name clauses))
  '())

(defmethod process-define-package-clause ((mechanism (eql 'common-lisp))
                                      key clause state name clauses)
  (declare (ignore name clauses))
  (case key
    ((:nicknames :documentation :use :shadow :shadowing-import-from
      :import-from :export :intern :size)
     (values `(,@state ,clause) t))
    (otherwise
     (if (member key *extended-cl-define-package-clause-keys*)
         (values `(,@state ,clause) t)
       (values state nil)))))

(defmethod compute-define-package-forms ((mechanism (eql 'common-lisp))
                                     state name clauses)
  (declare (ignore name clauses))
  (values '() state '()))
```

### An older extension mechanism
Before the current extensible `define-system` I wanted to allow the possibility that things might sit on top of functions besides the CL ones.  So `org.tfeb.clc:export` might not just call `cl:export` but some other function (which then presumably would call `cl:export` somehow).  This mechanism is controlled by the following variable:

**`*underlying-package-implementations*`** is an alist which maps between operation names and their underlying implementations.  The operation names are keywords.  It only maps operations for which conduit packages defines variant versions.  If you wanted to cause `define-package` to expand into `my-special:defpackage` then you would push `(:defpackage . my-special:defpackage)` onto this list.  Its default value simply points everything at the CL functions.

Note that this variable is used during the macroexpansion of `define-package` as well as by the variant versions of the CL package functions.

## The shim
The package `org.tfeb.conduit-packages/define-package`, in the system with the same name, exports several shims with names which include the word `conduit` and so will not clash with names from `CL`.  It also exports all the other conduits functionality, omitting only names that would clash with `CL`.

The shim functions are:

- **`delete-conduit-package`** is a shim around the conduits `delete-package`;
- **`rename-conduit-package`** is a shim around the conduits `rename-package`;
- **`export-from-conduit-package`** is a shim around the conduits `export`;
- **`unexport-from-conduit-package`** is a shim around the conduits `unexport`;

You can use this package in a package which also uses `cl` as no names now clash.

## Notes
Conduit packages should generally be defined with `(:use)` so the used-package list is empty rather than an implementation-default.  A conduit really is just that: it doesn't need to use any packages because it's never a package where anything is defined.

The `define-package` /`defpackage` macros use`*underlying-package-implementations*` to know what the underlying `defpackage` is, and so this variable matters at macro-expansion time as well as at runtime.

I believe that [UIOP's `define-package`](https://common-lisp.net/project/asdf/uiop.html#UIOP_002fPACKAGE "define-package") can do at least some of what conduit packages can do: conduit packages predates it by at least a decade however.  For a long time, `define-package` was called `define-conduit-package` to avoid clashing with the UIOP version, but now it is extensible I have given up on that.

## Building
Until 2023 this was all just one file, with a trivial ASDF system definition, That was a pain because you needed all sorts of annoying `eval-when`s[^3], so now it is in multiple files with a system declaration which is less trivial.  Apart from the shim system depending on conduit packages itself, there are no dependencies on other systems.

## Portability, bugs
All of this system should be portable CL: if it's not, then that's a bug.

---

## History
Conduit packages is an old system and has been through several changes, not all of them backward compatible.

### Long ago (1998-2002)
When I first wrote it, it was entangled with a version of the Franz Allegro local package nicknames which was made publicly available some time in the late 1990s or early 2000s and which I made work in at least LispWorks and perhaps CMUCL and Genera at that time.  That entanglement went away when I revived it in 2020 (astonishingly, it still worked in LW at that point).

### Two approaches to conduits (2021)
When I first wrote conduit packages I was concerned about fasl size, so the expansion of `defpackage` first used `cl:defpackage` to define a simple package, and then walked over the packages it was extending and modified it appropriately.  This made for small fasls, but meant that you often got warnings when compiling & loading files because `cl:defpackage` would redefine a package in a way which was incompatible with its current state when the file was loaded.

I'm less concerned with fasl sizes now, so in 2021 I changed the implementation to compute an enormous `cl:defpackage` form instead.  For a file containing only, say `(defpackage :foo (:use) (:extends :cl))` this can cause a factor of ten increase in fasl size (from about 3kB to about 22kB in one implementation), but it means you don't get annoying warnings.  Given that everything else around CL has bloated by hugely more than a factor of ten since the late 1990s, I think this is a price worth paying.

### Substantial incompatible changes (2023)
In 2023, I realised that I needed a way of extending `defpackage` for another purpose, but that conduit packages didn't make this really easy.  I didn't want to implement yet another `defpackage` version, so I bit the bullet and made several changes, not all of them compatible.

- I renamed `define-conduit-package`, previously provided as a shim, to `define-package` and made it be the underlying macro, with `defpackage` now a shim for it.
- I implemented the extension mechanism for `define-package` and used it to replace the old hardwired macro.
- I renamed some variables.
- I split the system into several files to get rid of the old `eval-when` ugliness.
- I renamed the shim system.

Much of this is not entirely compatible: I suspect it hurts nobody but me, however.  It all smells better after the changes, I think.

### `define-conduit-package` (2024)
I just thought this was useful.

---

Conduit packages is copyright 1998-2002, 2020-2024 by Tim Bradshaw.  See `LICENSE` for the license.

---

[^1]:	Here and below I have given package names in lower case: they are, of course, really upper case strings unless you are using Allegro in its incompatible 'modern' mode.  The exceptions are the `CL` and `CL-USER` packages which I've given in upper case: this is slightly inconsistent, sorry.  Symbol names are always given in lower case, although they too are all upper case of course.

[^2]:	In fact the definition now uses `define-package` rather than `defpackage`.

[^3]:	 And, once upon a time, you discover all sorts of exciting bugs in various implementation's handling of `eval-when`.  I expect these are now all long fixed though.