;;; -*-  Lisp -*-

(defpackage :net-telent-date-system (:use #:cl #:asdf))
(in-package :net-telent-date-system)

(defsystem net-telent-date
    :version "0.41"
    :components ((:file "defpackage")
		 (:file "date" :depends-on ("defpackage"))
		 (:file "parse-time" :depends-on ("defpackage"))))
