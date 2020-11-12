;;;; ASDF sysdcl for conduit packages
;;;

(in-package :asdf-user)

(defsystem "org.tfeb.conduit-packages"
  :description "Conduit packages"
  :author "Tim Bradshaw"
  :license "MIT"
  :components ((:file "conduit-packages")))
