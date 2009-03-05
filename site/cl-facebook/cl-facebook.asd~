;;; facebook api for lisp

(defpackage :cl-facebook.system
  (:use :cl :asdf))

(in-package :cl-facebook.system)

(defsystem :cl-facebook
  :name "facebook"
  :author "Red Daly"
  :version "0.0.1"
  :licence "BSD"
  :description "A library for using the Facebook API."
  :components ((:static-file "cl-facebook.asd")
               (:module :src
                :components ((:file "facebook-package")
                             (:file "facebook" :depends-on ("facebook-package")))))
  :depends-on ("trivial-http" "md5" "cl-json" "cl-ppcre"))
