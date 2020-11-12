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
;;; July 1998 and was originally written on a Symbolics.  Not much of
;;; it has changed since then, which I am quite proud about.
;;;
;;; Copyright 1998-2002, 2020 Tim Bradshaw.  This code may be used for
;;; any purpose whatsoever by anyone. It has no warranty whatsoever. I
;;; would appreciate acknowledgement if you use it in anger, and I
;;; would also very much appreciate any feedback or bug fixes.
;;;
;;; !!! TODO: more of the package operators probably need to be shadowed
;;;

(defpackage :org.tfeb.conduit-packages
  (:size 50)                                    ;why bother with this...
  (:use :cl)
  ;; redefined CL names
  (:shadow #:export #:unexport #:defpackage #:delete-package #:rename-package)
  (:export #:export #:unexport #:defpackage #:delete-package #:rename-package)
  ;; non-CL things
  (:export
   #:recompute-conduits
   #:*underlying-implementation-map*))

(in-package :org.tfeb.conduit-packages)

(provide :org.tfeb.conduit-packages)

;;;; Conduit implementation
;;;

(define-condition conduit-error (simple-error package-error)
  ;; I think all conduit errors should be PACKAGE-ERRORs, although
  ;; sometimes it is hard to find the right package
  ())

(defun conduit-error (package format &rest format-arguments)
  (error 'conduit-error
         :package package
         :format-control format
         :format-arguments format-arguments))

(defvar *conduit-package-descriptions* '())
(defvar *package-conduits* '())
(defvar *conduit-packages* '())

(defun canonicalise-package-name (package/name)
  ;; Return a keyword, being the canonical name of the package.
  ;; Second value is the package named, if it exists.
  ;; maybe this should not use KEYWORD but our own secret package.
  (etypecase package/name
    (package (values (intern (package-name package/name)
                             (find-package :keyword))
                     package/name))
    ((or string symbol)
     (let ((found (find-package package/name)))
       (values (intern (if found
                           (package-name found)
                           (typecase package/name
                             (string package/name)
                             (symbol (symbol-name package/name))))
                       (find-package :keyword))
               found)))))

(defun note-conduit (pack conduit)
  (let ((pack (canonicalise-package-name pack))
        (conduit (canonicalise-package-name conduit)))
    (let ((found (assoc pack *package-conduits*)))
      (if found
          (pushnew conduit (cdr found))
          (push (list pack conduit) *package-conduits*)))
    (let ((found (assoc conduit *conduit-packages*)))
      (if found
          (pushnew pack (cdr found))
          (push (list conduit pack) *conduit-packages*)))))

(defun recompute-conduits-for (pack &optional (chain '()))
  (let ((pack (canonicalise-package-name pack)))
    (when (member pack chain)
      (conduit-error pack
                     "Circular conduits: ~S occurs in ~S"
                     pack chain))

    (dolist (conduit (cdr (assoc pack *package-conduits*)))
      (apply #'make-package-conduit-package
             (assoc conduit *conduit-package-descriptions*))
      (recompute-conduits-for conduit (cons pack chain)))
    (find-package pack)))

(defun clean-package-alist (pa)
  ;; return a cleaned package alist: no nulls, no singletons, no nonexistent
  ;; packages.  Just blindly cons a new list here.
  (mapcan #'(lambda (pl)
              (let ((ppl (mapcan #'(lambda (p)
                                     (if (find-package p)
                                         (list p)
                                         nil))
                                 pl)))
                (if (or (null ppl)
                        (null (cdr ppl)))
                    nil
                    (list ppl))))
          pa))

(defun recompute-conduits ()
  "Clean up the lists of conduits, and recompute all conduit packages
to make them consistent"
  (setf *package-conduits* (clean-package-alist *package-conduits*)
        *conduit-packages* (clean-package-alist *conduit-packages*))
  (dolist (pd *package-conduits* (values))
    (recompute-conduits-for (car pd))))

(defun make-package-conduit-package (package/name &key
                                                  extends
                                                  extends/including
                                                  extends/excluding)
  (flet ((ensure-package (p)
           (let ((package (etypecase p
                            (package p)
                            ((or symbol string) (find-package p)))))
             (unless package
               ;; might want to be able to continue
               (conduit-error p "No package named ~S" p))
             package))
         (ensure-external-symbol (d p)
           (multiple-value-bind (s state)
               (find-symbol (etypecase d
                              (symbol (symbol-name d))
                              (string d))
                            p)
             (ecase state
               ((:external)
                s)
               ((nil)
                (conduit-error p "Symbol name ~S not found in ~S" d p))
               ((:internal)
                (conduit-error p "Symbol ~S internal in ~S" s p))
               ((:inherited)
                (conduit-error p "Symbol ~S not directly present in ~S" s p)))))
         (import-symbol (s pack)
           (cl:import (if (eq s 'nil)
                          '(nil)
                          s)
                      pack))
         (export-symbol (s pack)
           (cl:export (if (eq s 'nil)
                          '(nil)
                          s)
                      pack)))
    (let ((package (ensure-package package/name)))
      (dolist (ex extends)
        (note-conduit ex package)
        (do-external-symbols (s (ensure-package ex))
          (import-symbol s package)
          (export-symbol s package)))
      (dolist (ei extends/including)
        (let ((p (ensure-package (first ei))))
          (note-conduit p package)
          (dolist (s (mapcar #'(lambda (sd)
                                 (ensure-external-symbol sd p))
                             (rest ei)))
            (import-symbol s package)
            (export-symbol s package))))
      (dolist (ee extends/excluding)
        (let* ((p (ensure-package (first ee)))
               (es (mapcar #'(lambda (sd)
                               (ensure-external-symbol sd p))
                           (rest ee))))
          (note-conduit p package)
          (do-external-symbols (s p)
            (unless (member s es)
              (import-symbol s package)
              (export-symbol s package)))))
      package)))

;;; Cloning.  Unlike conduits, cloning is a static operation: making a
;;; clone of a package says to copy its state at a given moment and
;;; then ignore any further changes.  Redefining a cloned package will
;;; only pick up some of the changes - in particular symbols which
;;; have been unexported from the cloned packages will not get
;;; unexported and so on.
;;;
;;; It may or may not make sense to clone multiple packages, this
;;; function supports that because it's kind of implicit in the way
;;; DEFPACKAGE works that you might get multiple packages.
;;;
;;; It's not clear if any of this behaviour is right.
;;;

(defun clone-packages-to-package (froms to)
  (let ((tp (typecase to
              (package to)
              (t (or (find-package to)
                     (make-package to :use '()))))))
    (when (null tp)
      (conduit-error to "No target package..."))
    (loop for f in froms
          for from = (typecase f
                       (package f)
                       (t (find-package f)))
          for used = (package-use-list from)
          for shadows = (package-shadowing-symbols from)
          for exports = (let ((exps '()))
                          (do-external-symbols (s from exps)
                            (push s exps)))
          for interned-symbols = (let ((ints '()))
                                   (do-symbols (s from ints)
                                     (when (eq (symbol-package s) from)
                                       (push s ints))))
          when interned-symbols
          do (import interned-symbols tp)
          when shadows
          do (shadow shadows tp)
          when exports
          do (export exports tp)
          when used
          do (use-package used tp))
    tp))

;;;; Define the basic package operations we need to take over.
;;;
;;; !!! Others may need to be added here.  I think that UNINTERN is OK,
;;; but I'm not sure about others.

(eval-when (:load-toplevel :compile-toplevel :execute)
  ;; The following two are needed by the DEFPACKAGE macro, which is
  ;; expanded during compilation of this file below
  ;;
  (defvar *underlying-implementation-map*
    '((export . cl:export)
      (unexport . cl:unexport)
      (defpackage . cl:defpackage)
      (delete-package . cl:delete-package)
      (rename-package . cl:rename-package))
    "An alist which maps from names which conduits provides new
implementations for and their underlying implementation function
names.  You can use this if you want to layer conduits on top of some
other system which already is providing its own versions of these
names.  By default the underlying names are just the standard CL
functions.")

  (defun underlying (name)
    (let ((it (assoc name *underlying-implementation-map*)))
      (unless it
        (error "No underlying implementation for ~S" name))
      (cdr it))))

(defun funcall/ul (fname &rest args)
  (declare (dynamic-extent args))
  (apply (fdefinition (underlying fname)) args))

(defun export (symbol/s &optional (package *package*))
  (prog1
      (funcall/ul 'export symbol/s package)
    (recompute-conduits-for package)))

(defun unexport (symbol/s &optional (package *package*))
  (prog1
      (funcall/ul 'unexport symbol/s package)
    (recompute-conduits-for package)))

(defmacro defpackage (name &body clauses)       ;+++export
  "Define a package.  See CL:DEFPACKAGE for tha basics.
In addition, this version of DEFPACKAGE can define a `conduit package':
that you can use as a conduit to extend existing packages.
This works by importing symbols from the existing packages and
then reexporting them. The syntax is as DEFPACKAGE, with the addition
of three new clauses:
        (:EXTENDS package) takes package and reexports all its symbols;
        (:EXTENDS/INCLUDING package . syms/names) reexports only syms/names;
        (:EXTENDS/EXCLUDING package . syms/names) reexports all *but* syms/names.
When defining a conduit package you almost certainly will want to say (:USE)
to prevent the CL package being used.

This version of DEFPACKAGE also support `cloning' packages: making another
package which is `just like' an existing package. This means that all the
internal, exported and shadowing symbols in the clone will be the same as
those in the cloned package, but any additional things defined by DEFPACKAGE
will also take effect.  This allows you to essentially make a copy of
a package which you can then use to define new functionality without
interning a lot of things in the original package.  Cloning is a static
operation - packages do not know who their clones are, and no attempt is made
to keep clones up to date.  Cloning is done by the clause
        (:CLONES package)
Cloning is not compatible with extending (this is checked).
As with extending you probably want to specify (:USE) when cloning."
  (let ((dpcs '()) (excs '()) (eics ()) (eecs '()) (cpcs '())
        (defpackage/ul (underlying 'defpackage)))
    (dolist (c clauses)
      (case (first c)
        ((:extend :extends)
         (dolist (e (rest c))
           (push e excs)))
        ((:extend/including :extends/including)
         (push (rest c) eics))
        ((:extend/excluding :extends/excluding)
         (push (rest c) eecs))
        ((:clone :clones)
         (dolist (e (rest c))
           (push e cpcs)))
        (otherwise
         (push c dpcs))))
    (when (and cpcs (or excs eics eecs))
      (conduit-error name "Cloning is not compatible with extending"))
    (cond ((or excs eics eecs)
           `(progn
              (,defpackage/ul ,name
                ,@(nreverse dpcs))
              ;; need always to do this because defpackage is always done.
              (eval-when (:compile-toplevel :load-toplevel :execute)
                (let* ((cn (canonicalise-package-name ',name))
                       (found (assoc cn *conduit-package-descriptions*))
                       (descr '(:extends ,(nreverse excs)
                                :extends/including ,(nreverse eics)
                                :extends/excluding ,(nreverse eecs))))
                  (if found
                      (setf (cdr found) descr)
                    (push (cons cn descr) *conduit-package-descriptions*))
                  (apply #'make-package-conduit-package cn descr))
                (recompute-conduits-for ',name))))
          (cpcs
           `(progn
              (,defpackage/ul ,name
                ,@(nreverse dpcs))
              (eval-when (:compile-toplevel :load-toplevel :execute)
                (clone-packages-to-package ',cpcs ',name))))
          (t
           `(progn
              (,defpackage/ul ,name ,@(nreverse dpcs))
              (recompute-conduits-for ',name))))))

(defun delete-package (pack/name)
  (let ((name (canonicalise-package-name pack/name)))
    (let ((conduits (cdr (assoc name *package-conduits*))))
      (when conduits
        (conduit-error
         pack/name
         "Trying to delete ~S, but it has conduits ~S"
         (find-package pack/name) (mapcar #'find-package conduits))))
    (prog1
        (funcall/ul 'delete-package pack/name)
      ;; NAME can occur in *CONDUIT-PACKAGES* if it was a conduit.
      ;; NAME can occur in *PACKAGE-CONDUITS* if it had conduits
      ;; (there will not now be any)
      (setf *conduit-packages* (delete name *conduit-packages* :key #'car)
            *package-conduits* (delete name *package-conduits* :key #'car)))))

(defun rename-package (pack/name new-name &optional (nicknames '()))
  (prog1
      (funcall/ul 'rename-package pack/name new-name nicknames)
    (let ((name (canonicalise-package-name pack/name))
          (new-name (canonicalise-package-name new-name)))
      (dolist (c *conduit-packages*)
        (nsubstitute new-name name c))
      (dolist (p *package-conduits*)
        (nsubstitute new-name name p)))))

;;;; Define the CL/CONDUITS package and a user package.
;;;
;;; I would like to be able to say simply (EVAL-WHEN (:LOAD-TOPLEVEL
;;; ...)  ...)  here, but that breaks, because that results in
;;; DEFPACKAGE being processed as a top-level form in not-compile-time
;;; mode, and *it* expands to (EVAL-WHEN (:COMPILE-TOPLEVEL ...) ...),
;;; so actually gets evaluated at compile-time, which fails.  (LET ()
;;; ...) is just enough to stop this: LOCALLY or PROGN is not.  This is
;;; broken.
;;;
;;; CLISP 2000-03-06 (March 2000)  can't hack this at all:  you need to
;;; extract the remainder of this file and put it into a different
;;; file, compiled and loaded after the main file is loaded.
;;;
;;; CMUCL 18b can't do this.  CMUCL 18c Sources 2000-09-27 does have
;;; bugs in EVAL-WHEN, but does this right.
;;;
(eval-when (:load-toplevel :execute)
  (let ()
    (defpackage :org.tfeb.cl/conduits
      (:use)
      (:nicknames :org.tfeb.clc)
      (:extends/excluding :cl #:export #:unexport #:defpackage
                          #:delete-package #:rename-package)
      (:extends/excluding :org.tfeb.conduit-packages
       #:recompute-conduits #:*underlying-implementation-map*))
    (defpackage :org.tfeb.cl-user/conduits
      (:nicknames :org.tfeb.clc-user)
      (:use :org.tfeb.clc))))

#||
(defpackage :cl/magic-if
  (:extends/excluding :cl #:if)
  (:export #:if))
||#
