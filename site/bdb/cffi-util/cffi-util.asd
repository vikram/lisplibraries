;; -*- lisp -*-

;;;; * CFFI-Util package

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :cffi-util-system)
    (defpackage :cffi-util-system
      (:use :common-lisp :asdf))))

(in-package :cffi-util-system)

(defsystem :cffi-util
  :description "some utilities"
  :author ""
  :version "0.0,0"
  :components
  ((:module :src
	    :components ((:file "packages")
			 (:file "flags"
				:depends-on ("packages"))
			 (:file "defcfun"
				:depends-on ("packages" "flags"))
			 (:file "cbuffer"
				:depends-on ("packages"))
			 (:file "cbuffer-streams"
				:depends-on ("cbuffer")))))
  :depends-on (:cffi :mycl-util :trivial-gray-streams :flexi-streams))

;;;;@include "src/packages.lisp"

;;;;@include "src/flags.lisp"

;;;;@include "src/defcfun.lisp"

;;;;@include "src/cbuffer.lisp"

;;;;@include "src/cbuffer-streams.lisp"
