;;;; Conduit packages, and package cloning
;;;
;;; This defines a version of DEFPACKAGE which allows you to define
;;; packages which serve as 'conduits' between one or more other
;;; packages and the user.  A conduit is a package which contains and
;;; reexports some or all of one or more other packages, and by
;;; controlling exactly which symbols are resported you can construct
;;; a conduit which, for instance, contains everything from the CL
;;; package except a few symbols which it exports from some other
;;; package, to produce a package which provides a language which is
;;; similar to, but not identical to, CL.  All of this is possible
;;; just using the standard CL package facilities, but providing a
;;; variant DEFPACKAGE makes it much more convenient.
;;;
;;; A useful conduit package is provided by this system: ORG.TFEB.CLC:
;;; this is a conduit for the CL package and for the implementation
;;; package, ORG.TFEB.CONDUIT-PACKAGES: the final effect of this is
;;; that ORG.TFEB.CLC is the same as CL except that some
;;; package-related functionality, and in particular DEFPACKAGE and
;;; some other package-related functions comes from
;;; ORG.TFEB.CONDUIT-PACKAGES.  There is also a 'user' package, which
;;; is ORG.TFEB.CLC-USER (this package is not a conduit).
;;;
;;; The CL names for which conduit-friendly versions are provided are
;;; DEFPACKAGE, EXPORT, UNEXPORT, RENAME-PACKAGE and DELETE-PACKAGE.
;;; All of these defer to the underlying implementation to do much of
;;; their work.  What the underlyng implementation is is defined by
;;; *UNDERLYING-IMPLEMENTATION-MAP*, which is simply an alist mapping
;;; the symbols in this package to the names of other symbols.  By
;;; default the underlying implementations are all the standard CL
;;; symbols, but it would be possible to change this alist to layer
;;; this system on top of some other system which provides variant
;;; versions of the CL functionality, so long as the function & macro
;;; signatures are compatible.
;;;
;;; A previous version of this code was mildly entangled with a system
;;; called hierarchical-packages which allowed packages with
;;; domain-structured names (or any names with '.' in to refer to each
;;; other by relative names, and also for such packages to have
;;; per-package aliases for other packages (so you could make things
;;; such that CL:DEFPACKAGE really meant ORG.TFEB.CLC:DEFPACKAGE, if
;;; you were in the right package, which was nice).  That system in
;;; turn was based on a very old bit of Allegro sample code.  It was
;;; useful but not completely portable as it relied on being able to
;;; hook package lookup.  There seemed to be no enthusiasm for making
;;; such a hook effectively portable a few years ago, so I have now
;;; disentangled this system from that old code.  The idea of such a
;;; hook is still useful in my opinion as it allows intervention in
;;; string->package lookups by user code: if anyone else revives such
;;; a scheme or any similar scheme I'd like to know so I can change
;;; this code to use it.
;;;
;;; This package seems to have originated on or before the 24th of
;;; July 1998 and was originally written on a Symbolics.  Until
;;; recently not much of it has changed since then, which I was quite
;;; proud about.
;;;
;;; That 1990s version defined conduits & clones by having DEFPACKAGE
;;; expand to a 'clean' underlying DEFPACKAGE form, followed by a form
;;; which constructed the conduit or clone.  This had the advantage
;;; that the resulting macroexpansion was small, but the disadvantage
;;; that compiling and loading a file often resulted in warnings as
;;; the package was redefined apparently incompatibly with its current
;;; state.  At the time that seemed like a reasonable tradeoff: not so
;;; much now.  What now happens is that DEFPACKAGE for a conduit
;;; package expands into an enormous underlying DEFPACKAGE which
;;; defines the package in its final state (together with some
;;; validation and housekeeping code wrapped around it).  This makes
;;; FASLs much larger (a file containing a package which extends CL
;;; now compiles to a 22kB FASL compared with under 3kB with the old
;;; mechanism), but means there are no spurious warnings.
;;;
;;; In 2023 this was further changed: the underlying macro is now
;;; DEFINE-PACKAGE (DEFPACKAGE is a shim around it), and this macro is
;;; now user extensible.
;;;
;;; Copyright 1998-2002, 2020-2023 Tim Bradshaw.  This code may be
;;; used for any purpose whatsoever by anyone. It has no warranty
;;; whatsoever. I would appreciate acknowledgement if you use it in
;;; anger, and I would also very much appreciate any feedback or bug
;;; fixes.
;;;
;;; !!! TODO: more of the package operators probably need to be shadowed
;;;

(in-package :cl-user)

(defpackage :org.tfeb.conduit-packages
  (:size 50)                                    ;why bother with this...
  (:use :cl)
  ;; redefined CL names
  (:shadow #:export #:unexport #:defpackage #:delete-package #:rename-package)
  (:export #:export #:unexport #:defpackage #:delete-package #:rename-package)
  ;; Conduits
  (:export
   #:recompute-conduits
   #:*underlying-package-implementations*)
  ;; Extensible DEFPACKAGE
  (:export
   #:define-package
   #:define-conduit-package
   #:*define-package-mechanisms*
   #:*extended-cl-define-package-clause-keys*
   #:initial-define-package-state
   #:process-define-package-clause
   #:compute-define-package-forms))
