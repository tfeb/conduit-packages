;;;; Conduit packages and package cloning: implementation
;;;

(in-package :org.tfeb.conduit-packages)

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

(defun ensure-package (p)
  ;; Return the package corresponding to P.  I am no longer sure why I
  ;; used ETYPECASE rather than relying on FIND-PACKAGE to do that but
  ;; I don't want to change it now, in case there was a good reason.
  (let ((package (etypecase p
                   (package p)
                   ((or symbol string) (find-package p)))))
    (unless package
      ;; might want to be able to continue
      (conduit-error p "No package named ~S" p))
    package))

(defun ensure-external-symbol (d p)
  ;; ensure that D designates an external symbol in P, and return the
  ;; symbol if so
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

(defun make-package-conduit-package (package/name &key
                                                  extends
                                                  extends/including
                                                  extends/excluding)
  ;; In the old implementation this was used after DEFPACKAGE to turn
  ;; a package into a conduit.  It's still used when recomputing
  ;; conduits
  (flet ((import-symbol (s pack)
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

;;; Parts of the expansion of DEFINE-PACKAGE
;;;

(defun validate-conduits (&key extends
                               extends/including
                               extends/excluding)
  ;; Validate that a conduit is going to work: all the packages it is
  ;; a conduit for shold exist, included symbols should be external in
  ;; them.
  (dolist (ex extends)
    (ensure-package ex))
  (dolist (ei extends/including)
    (let ((p (ensure-package (first ei))))
      (dolist (s (rest ei))
        (ensure-external-symbol s p))))
  (dolist (ee extends/excluding)
    (ensure-package (first ee)))
  t)

(defun compute-conduit-clauses (&key extends
                                     extends/including
                                     extends/excluding)
  ;; Return clauses for underlying DEFPACKAGE for a conduit.  This
  ;; does not sanity-check (see VALIDATE-CONDUITS, which will have
  ;; done this).
  (append
   (loop for p in extends
         for externals = (loop for s being the external-symbols of (find-package p)
                               collect (symbol-name s))
         collect `(:import-from ,p ,@externals)
         collect `(:export ,@externals))
   (loop for (p . included) in extends/including
         collect `(:import-from ,p ,@included)
         collect `(:export ,@included))
   (loop for (p . excluded) in extends/excluding
         for excluded-names = (mapcar #'string excluded)
         for included = (loop for s being the external-symbols of (find-package p)
                              for sn = (symbol-name s)
                              unless (member sn excluded-names
                                             :test #'string=)
                                collect sn)
         collect `(:import-from ,p ,@included)
         collect `(:export ,@included))))

(defun note-conduits (package-name &key extends
                                   extends/including
                                   extends/excluding)
  ;; Note conduit relationships.  Again, all validation will have been
  ;; done by VALIDATE-CONDUITS.  The package exists at this point.
  (let* ((cn (canonicalise-package-name package-name))
         (found (assoc cn *conduit-package-descriptions*))
         (descr `(:extends ,extends
                  :extends/including ,extends/including
                  :extends/excluding ,extends/excluding)))
    (if found
        (setf (cdr found) descr)
      (push (cons cn descr) *conduit-package-descriptions*)))
  (let ((p (find-package package-name)))
    (dolist (e extends)
      (note-conduit (find-package e) p))
    (dolist (ei extends/including)
      (note-conduit (find-package (first ei)) p))
    (dolist (ee extends/excluding)
      (note-conduit (find-package (first ee)) p)))
  package-name)

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

(defun compute-clone-clauses (froms)
  ;; Make clauses for cloning one or more packages
  (loop for from/name in froms
        for from = (find-package from/name)
        collect `(:use ,@(mapcar #'package-name (package-use-list from)))
        collect `(:shadow ,@(mapcar #'symbol-name (package-shadowing-symbols from)))
        collect `(:import-from ,(package-name from)
                  ,@(loop for s being the present-symbols of from
                          collect (symbol-name s)))
        collect `(:export ,@(loop for s being the external-symbols of from
                                 collect (symbol-name s)))))

;;;; Define the basic package operations we need to take over.
;;;
;;; !!! Others may need to be added here.  I think that UNINTERN is OK,
;;; but I'm not sure about others.

(defvar *underlying-package-implementations*
  '((:export . cl:export)
    (:unexport . cl:unexport)
    (:defpackage . cl:defpackage)
    (:delete-package . cl:delete-package)
    (:rename-package . cl:rename-package))
  "An alist which maps from names which conduits provides new
implementations for and their underlying implementation function
names.  You can use this if you want to layer conduits on top of some
other system which already is providing its own versions of these
names.  By default the underlying names are just the standard CL
functions.

The keys of this alist are keywords, corresponding to the name in the
CL package, to avoid any package confusion.")

(defun underlying (name)
  (let ((it (assoc name *underlying-package-implementations*)))
    (unless it
      (error "No underlying implementation for ~S" name))
    (cdr it)))

(defun funcall/ul (fname &rest args)
  (declare (dynamic-extent args))
  (apply (fdefinition (underlying fname)) args))

(defun export (symbol/s &optional (package *package*))
  (prog1
      (funcall/ul ':export symbol/s package)
    (recompute-conduits-for package)))

(defun unexport (symbol/s &optional (package *package*))
  (prog1
      (funcall/ul ':unexport symbol/s package)
    (recompute-conduits-for package)))

(defun delete-package (pack/name)
  (let ((name (canonicalise-package-name pack/name)))
    (let ((conduits (cdr (assoc name *package-conduits*))))
      (when conduits
        (conduit-error
         pack/name
         "Trying to delete ~S, but it has conduits ~S"
         (find-package pack/name) (mapcar #'find-package conduits))))
    (prog1
        (funcall/ul ':delete-package pack/name)
      ;; NAME can occur in *CONDUIT-PACKAGES* if it was a conduit.
      ;; NAME can occur in *PACKAGE-CONDUITS* if it had conduits
      ;; (there will not now be any)
      (setf *conduit-packages* (delete name *conduit-packages* :key #'car)
            *package-conduits* (delete name *package-conduits* :key #'car)))))

(defun rename-package (pack/name new-name &optional (nicknames '()))
  (prog1
      (funcall/ul ':rename-package pack/name new-name nicknames)
    (let ((name (canonicalise-package-name pack/name))
          (new-name (canonicalise-package-name new-name)))
      (dolist (c *conduit-packages*)
        (nsubstitute new-name name c))
      (dolist (p *package-conduits*)
        (nsubstitute new-name name p)))))

;;;; New extensible DEFINE-PACKAGE
;;; (Formerly this was DEFPACKAGE)
;;;

(defvar *define-package-mechanisms* '(common-lisp conduit-packages)
  "A list of mechanism names for conduit packages' DEFINE-PACKAGE

For each of these mechanism names, EQL methods must be defined on
INITIAL-DEFINE-PACKAGE-STATE, PROCESS-DEFINE-PACKAGE-CLAUSE and
COMPUTE-DEFINE-PACKAGE-FORMS, which see.

You can (and should) add and remove your own entries to this variable,
but don't muck around with other people's.")

(defvar *extended-cl-define-package-clause-keys*
  #+Lispworks
  '(:add-use-defaults :local-nicknames)
  #-LispWorks
  '(:local-nicknames)
  "A list of DEFPACKAGE clause keywords which should be treated as part of CL

Clauses with these keywords will be handled by the CL clause handler")

(defgeneric initial-define-package-state (mechanism name clauses)
  (:documentation
   "Return the initial state for a DEFINE-PACKAGE mechanism

This is called for each mechanism in *define-package-mechanisms*.
Generally you should define EQL methods on this generic function for
any mechanism name you want to add.

Arguments are the mechanism name, the name of the package being
defined and all the clauses for the DEFINE-PACKAGE form.

The state this returns can be any object at all, and is handed to both
calls to PROCESS-DEFINE-PACKAGE-CLAUSE and COMPUTE-DEFINE-PACKAGE-FORMS.  It
may be mutated freely by the appropriate methods for these generic
functions."))

(defgeneric process-define-package-clause (mechanism key clause state name clauses)
  (:documentation
   "Process a define-package clause for a mechanism

This is called for each mechanism in *DEFINE-PACKAGE-MECHANISMS* and each
clause in the DEFINE-PACKAGE form. Generally you should define EQL methods
on this generic function for any mechanism you want to add.

Arguments are
- the mechanism, a symbol;
- the keyword of the DEFINE-PACKAGE clause, a keyword;
- the whole DEFINE-PACKAGE clause being processed, a list;
- the current state for the mechanism;
- the name of the package being defined;
- all the DEFINE-PACKAGE clauses.

This should return two values:
- an updated state for this mechanism;
- a boolean indicating whether it handled the key

Note that each mechanism gets given each clause, so more than one
mechanism can handle a clause. But unless at least one mechanism does
an error will be signalled."))

(defgeneric compute-define-package-forms (mechanism state name clauses)
  (:documentation
   "Compute forms for the expansion of DEFINE-PACKAGE for a mechanism

This is called for each mechanism in *DEFINE-PACKAGE-MECHANISMS* and each
clause in the DEFINE-PACKAGE form. Generally you should define EQL methods
on this generic function for any mechanism you want to add.

Arguments are:
- the mechanism name, a symbol;
- the final state for this mechanism;
- the name of the package being defined;
- all the DEFINE-PACKAGE clauses.

This must return three values:
- a list of forms which will be evaluated before the final DEFPACKAGE form;
- a list of clauses to add to the DEFPACKAGE form;
- a list of forms which will be evaluated after the DEFPACKAGE form.

Both the before forms and after forms will be evaluated within an
`(eval-when (:compile-toplevel :load-toplevel :execute) ...) form."))

;;;; Common lisp mechanism
;;;

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

;;;; Conduit packages mechanism
;;;

(defmethod initial-define-package-state ((mechanism (eql 'conduit-packages))
                                     name clauses)
  (declare (ignore name clauses))
  '())

(defmethod process-define-package-clause ((mechanism (eql 'conduit-packages))
                                      key clause state name clauses)
  (declare (ignore name clauses))
  (case key
    ((:extend :extends)
     (setf (getf state ':extends)
           (append (getf state ':extends '())
                   (rest clause)))
     (values state t))
    ((:extend/including :extends/including)
     (setf (getf state ':extends/including)
           (append (getf state ':extends/including '())
                   (list (rest clause))))
     (values state t))
    ((:extend/excluding :extends/excluding)
     (setf (getf state ':extends/excluding)
           (append (getf state ':extends/excluding '())
                   (list (rest clause))))
     (values state t))
    ((:clone :clones)
     (setf (getf state :clones)
           (append (getf state ':clones '())
                   (rest clause)))
     (values state t))
    (otherwise
     (values state nil))))

(defmethod compute-define-package-forms ((mechanism (eql 'conduit-packages))
                                     state name clauses)
  (declare (ignore clauses))
  (let ((extends (getf state ':extends))
        (extends/including (getf state ':extends/including))
        (extends/excluding (getf state ':extends/excluding))
        (clones (getf state ':clones)))
    (when (and clones (or extends extends/including extends/excluding))
      (conduit-error name "Cloning is not compatible with extending"))
    (cond
     ((or extends extends/including extends/excluding)
      (values
       (list `(validate-conduits :extends ',extends
                                 :extends/including ',extends/including
                                 :extends/excluding ',extends/excluding))
       (compute-conduit-clauses :extends extends
                                :extends/including extends/including
                                :extends/excluding extends/excluding)
       (list
        `(note-conduits ',name
                        :extends ',extends
                        :extends/including ',extends/including
                        :extends/excluding ',extends/excluding)
        `(recompute-conduits-for ',name))))
     (clones
      (values
       '()
       (compute-clone-clauses clones)
       (list
        `(recompute-conduits-for ',name))))
     (t
      (values
       '()
       '()
       (list
        `(recompute-conduits-for ',name)))))))

;;;; DEFINE-PACKAGE itself
;;;

(defmacro define-package (name &body clauses)
  "Extensible variant of DEFPACKAGE

This macro is like CL:DEFPACKAGE, and supports the standard
CL clauses, some optional common extended clauses (see
*EXTENDED-CL-DEFINE-PACKAGE-CLAUSE-KEYS*, as well as conduit packages and
package cloning.  See *DEFINE-PACKAGE-MECHANISMS* and the the
documentation of conduit packages for the extension mechanism for this
macro."
  (let ((states (mapcar (lambda (mechanism)
                          (initial-define-package-state mechanism name clauses))
                        *define-package-mechanisms*)))
    (dolist (clause clauses)
      (let ((clause-handled nil))
        (setf states (mapcar (lambda (mechanism state)
                               (multiple-value-bind (new-state handled)
                                   (process-define-package-clause mechanism (car clause)
                                                                  clause state name clauses)
                                 (when handled (setf clause-handled t))
                                 new-state))
                             *define-package-mechanisms* states))
        (unless clause-handled
          (conduit-error name "nothing handled clause ~S" clause))))
    (let ((befores '())
          (effective-clauses '())
          (afters '()))
      (mapc (lambda (mechanism state)
              (multiple-value-bind (bs cs as)
                  (compute-define-package-forms mechanism state name clauses)
                ;; Note: befores happen in reverse order to afters: I
                ;; am not *sure* this is right but it gives the effect
                ;; of around methods.
                (setf befores (append bs befores)
                      effective-clauses (append effective-clauses cs)
                      afters (append afters as))))
            *define-package-mechanisms* states)
      `(progn
         (eval-when (:load-toplevel :compile-toplevel :execute)
           ,@befores)
         (,(underlying ':defpackage) ,name ,@effective-clauses)
         (eval-when (:load-toplevel :compile-toplevel :execute)
           ,@afters
           (find-package ',name))))))

(defmacro defpackage (name &body clauses)
  "DEFPACKAGE for conduit packages

This version of DEFPACKAGE is like CL's DEFPACKAGE, but can define a
`conduit package': that you can use as a conduit to extend existing
packages.  This works by importing symbols from the existing packages
and then reexporting them. The syntax is as DEFPACKAGE, with the
addition of three new clauses:
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
As with extending you probably want to specify (:USE) when cloning.

This version of DEFPACKAGE is simply a shim around DEFINE-PACKAGE, which see"
  `(define-package ,name ,@clauses))

#||
(defpackage :cl/magic-if
  (:extends/excluding :cl #:if)
  (:export #:if))
||#
