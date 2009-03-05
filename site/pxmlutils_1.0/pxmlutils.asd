;;;; -*- lisp -*-

(defpackage :pxmlutils.system
  (:use :common-lisp
        :asdf))

(in-package :pxmlutils.system)

(defclass file.cl (cl-source-file)
  ())

(defmethod source-file-type ((f file.cl) (m module))
  (declare (ignore f m))
  "cl")

(defsystem :pxmlutils
    :components ((:file.cl "phtml")
		 (:file.cl "pxml0")
		 (:file.cl "pxml1" :depends-on ("pxml0"))
		 (:file.cl "pxml2" :depends-on ("pxml1"))
		 (:file.cl "pxml3" :depends-on ("pxml2")))
    :depends-on (:acl-compat))
