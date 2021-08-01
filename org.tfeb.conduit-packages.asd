;;;; ASDF sysdcl for conduit packages
;;;

(in-package :asdf-user)

(defsystem "org.tfeb.conduit-packages"
  :description "Conduit packages"
  :version "2.0.0"
  :author "Tim Bradshaw"
  :license "MIT"
  :homepage "https://github.com/tfeb/conduit-packages"
  :components ((:file "conduit-packages")))
