;;;; Just the basic facts ma'am
;;;

#+org.tfeb.tools.require-module
(org.tfeb.tools.require-module:needs
  (:org.tfeb.conduit-packages :compile t))

(org.tfeb.clc:defpackage :org.tfeb.conduit-packages/define-package
  ;; This is a package you can use in other packages: it exposes
  ;; conduit packages by shims rather than by the CL names.  It is
  ;; itself a conduit, but not a pure one as it seemed absurd to
  ;; define a separate inplementation package
  (:use :org.tfeb.clc)
  (:extends/excluding :org.tfeb.conduit-packages
   ;; Extend conduit packages without the symbols which would clash
   ;; with CL
   #:export #:unexport #:defpackage #:delete-package #:rename-package)
  (:export
   ;; shims
   #:delete-conduit-package
   #:rename-conduit-package
   #:export-from-conduit-package
   #:unexport-from-conduit-package))

(in-package :org.tfeb.conduit-packages/define-package)

(provide :org.tfeb.conduit-packages/define-package)

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
