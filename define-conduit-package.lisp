;;;; Just the basic facts ma'am
;;;

(defpackage :org.tfeb.conduit-packages/define-conduit-package
  (:use :org.tfeb.clc :org.tfeb.conduit-packages)
  (:export
   #:recompute-conduits
   #:define-conduit-package
   #:delete-conduit-package
   #:rename-conduit-package
   #:export-from-conduit-package
   #:unexport-from-conduit-package))

(in-package :org.tfeb.conduit-packages/define-conduit-package)

(provide :org.tfeb.conduit-packages/define-conduit-package)

(defmacro define-conduit-package (name &body clauses)
  "A shim around the conduit packages DEFPACKAGE"
  `(defpackage ,name ,@clauses))

(defun delete-conduit-package (pack/name)
  "A shim around the conduit packages DELETE-PACKAGE"
  (delete-package pack/name))

(defun rename-conduit-package (pack/name new-name &optional (nicknames '()))
  "A shim around the conduit packages RENAME-PACKAGE"
  (rename-package pack/name new-name nicknames))

(defun export-from-conduit-package (symbol/s &optional (package *package*))
  "A shim around the conduit packages EXPORT"
  (export symbol/s package))

(defun unexport-from-conduit-package (symbol/s &optional (package *package*))
  "A shim around the conduit packages UNEXPORT"
  (unexport symbol/s package))
