(defpackage #:nekthuth.system
  (:use #:cl #:asdf)
  (:export #:expected-version
           #:+nekthuth-version+
   ))

(in-package :nekthuth.system)

(defconstant +nekthuth-version+ 0.3)

; We're going to require that the version installed for vim and this are exact
(defun expected-version (v)
 (eql v +nekthuth-version+))

(defsystem nekthuth
  :name "Nekthuth"
  :version (format nil "~A" +nekthuth-version+)
  :maintainer "Frank Duncan (frank@nekthuth.com)"
  :author "Frank Duncan (frank@nekthuth.com)"
  :licence "Lisp Lesser General Public License"
  :description "Connection with a Lisp"
  :long-description "The lisp component of the library that connects vim to lisp.
  Combined with the vim plugin, this allows users to send and receive messages
  in vim with the participating lisp system."

  :serial t
  :components (
               (:file "package")
               (:file "utilities")
               (:file "nekthuth")
               (:file "commands")
               (:file "debugger")
               (:file "receivers")
               (:file "senders"))
  :depends-on (:cl-ppcre :cl-html-parse))
