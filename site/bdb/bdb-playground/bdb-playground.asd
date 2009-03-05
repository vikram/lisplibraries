;; -*- lisp -*-

;;;; * ASDF info

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :bdb-p-system)
    (defpackage :bdb-p-system
      (:use :common-lisp :asdf))))

(in-package :bdb-p-system)

(defsystem :bdb-playground
  :description "Berkley DB Playground"
  :author ""
  :version "0.0,0"
  :components
  ((:module :src
	    :components ((:file "packages")
			 (:file "util"
				:depends-on ("packages"))
			 (:file "ext-util"
				:depends-on ("packages")))))
  :depends-on (:cl-store :bdb))

;;;;@include "src/packages.lisp"

;;;;@include "src/util.lisp"