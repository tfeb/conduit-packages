;;;; ASDF sysdcl for conduit packages
;;;

(in-package :asdf-user)

(defsystem "org.tfeb.conduit-packages"
  :description "Conduit packages"
  :version (:read-file-line "VERSION")
  :author "Tim Bradshaw"
  :license "MIT"
  :homepage "https://github.com/tfeb/conduit-packages"
  :components ((:file "conduit-packages")
               (:file "define-conduit-package"
                :depends-on ("conduit-packages"))))
