;;;; ASDF sysdcls for conduit packages
;;;

(in-package :asdf-user)

(defsystem "org.tfeb.conduit-packages"
  :description "Conduit packages"
  :version (:read-file-line "VERSION")
  :author "Tim Bradshaw"
  :license "MIT"
  :homepage "https://github.com/tfeb/conduit-packages"
  :serial t
  :components ((:file "pkg")
               (:file "impl")
               (:file "cometh")))

(defsystem "org.tfeb.conduit-packages/define-package"
  :description "Shims for conduit packages"
  :version (:read-file-line "VERSION")
  :author "Tim Bradshaw"
  :license "MIT"
  :homepage "https://github.com/tfeb/conduit-packages"
  :depends-on ("org.tfeb.conduit-packages")
  :serial t
  :components ((:file "define-package")))
