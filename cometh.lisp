;;;; Define the CL/CONDUITS package and a user package.
;;;
;;; There used to be elaborate EVAL-WHEN hair here when this was all
;;; in one file.  No more!
;;;

(in-package :org.tfeb.conduit-packages)

(define-package :org.tfeb.cl/conduits
  ;; This is CL but with conduits.  It doesn't include any of the
  ;; fancy stuff
  (:use)
  (:nicknames :org.tfeb.clc)
  (:extends/excluding :cl
   #:export #:unexport #:defpackage
   #:delete-package #:rename-package)
  (:extends/including :org.tfeb.conduit-packages
   #:export #:unexport #:defpackage
   #:delete-package #:rename-package))

(define-package :org.tfeb.cl-user/conduits
  ;; CL-USER but with conduits
  (:nicknames :org.tfeb.clc-user)
  (:use :org.tfeb.clc))

(provide :org.tfeb.conduit-packages)
