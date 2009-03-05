;; -*- lisp -*-

;;;; * BDB ASDF definition

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :bdb-system)
    (defpackage :bdb-system
      (:use :common-lisp :asdf))))

(in-package :bdb-system)

(defsystem :bdb
  :description "Berkley DB CFFI-Bindings"
  :author ""
  :version "0.0,0"
  :components
  ((:module :src
	    :components ((:file "package")
			 (:file "classes"
				:depends-on ("package"))
			 (:file "config"
				:depends-on ("package"
					     "aux-bindings"
					     "classes"))
			 (:file "consts"
				:depends-on ("package"))
			 (:file "aux-bindings"
				:depends-on ("package" "consts"
						       "classes"))
			 (:file "bdb"
				:depends-on ("package"))
			 (:file "util"
				:depends-on ("package"))
			 (:file "db"
				:depends-on ("package" "consts"
					     "aux-bindings" "bdb" "util"
					     "config" "classes" "txn"))
			 (:file "cursor"
				:depends-on ("package" "aux-bindings"
					     "consts" "txn"
					     "db" "util" "classes"))
			 (:file "db-env"
				:depends-on ("package" "consts"
					     "aux-bindings" "classes"))
			 (:file "txn"
				:depends-on ("package" "consts"
					     "aux-bindings" "classes"))
			 (:file "locks"
				:depends-on ("package" "consts"
					     "aux-bindings" "classes"))
			 (:file "log"
				:depends-on ("package" "consts"
					     "aux-bindings" "classes"))
			 (:file "secondary"
				:depends-on ("package" "consts"
					     "txn"
					     "aux-bindings" "classes"))
			 (:file "sequence"
				:depends-on ("package" "consts"
					     "aux-bindings" "classes"
					     "txn")))))
  :depends-on (:cffi-util))

;;;;@include "src/package.lisp"

;;;;@include "src/classes.lisp"

;;;;@include "src/consts.lisp"

;;;;@include "src/aux-bindings.lisp"

;;;;@include "src/bdb.lisp"

;;;;@include "src/db.lisp"

;;;;@include "src/cursor.lisp"

;;;;@include "src/db-env.lisp"

;;;;@include "src/txn.lisp"

;;;;@include "src/locks.lisp"

;;;;@include "src/log.lisp"

;;;;@include "src/secondary.lisp"

;;;;@include "src/sequence.lisp"

;;;;@include "src/util.lisp"

;;;; * Development files/Trash (Or what's not implemented yet)

;;;;@include "dev/bindings-dev.lisp"

;;;;@include "dev/consts_dev.lisp"
