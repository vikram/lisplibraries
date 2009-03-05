(defpackage #:gzip-system (:use :cl :asdf))

(in-package :gzip-system)

(defsystem gzip-stream
  :serial t
  :version "0.1"
  :author "Sean Ross <sross@common-lisp.net>"
  :maintainer "Sean Ross <sross@common-lisp.net>"
  :licence "BSD"
  :components ((:file "package")
               (:file "ifstar")
               (:file "inflate")
               (:file "gzip-stream"))
  :depends-on (:salza :flexi-streams :trivial-gray-streams))


