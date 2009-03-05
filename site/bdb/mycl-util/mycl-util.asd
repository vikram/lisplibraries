;; -*- lisp -*-

;;;; * mycl-util ASDF description

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :mycl-util-system)
    (defpackage :mycl-util-system
      (:use :common-lisp :asdf))))

(in-package :mycl-util-system)

(defsystem :mycl-util
  :description "some utilities"
  :author ""
  :version "0.0,0"
  :components
  ((:module :src
	    :components ((:file "packages")
                         (:file "list"
				:depends-on ("packages"))
			 (:file "tree"
				:depends-on ("packages"))
			 (:file "io"
				:depends-on ("packages"))
			 (:file "misc"
				:depends-on ("packages"))
			 (:file "lazy"
				:depends-on ("packages"))
			 (:file "memo"
				:depends-on ("packages"))
			 (:module :macro
				  :components ((:file "fun")
					       (:file "anaphorics")
					       (:file "loop")
					       (:file "meta")
					       (:file "misc"
						      :depends-on ("meta"
								   ))
					       (:file "setf"
						      :depends-on ("meta")))
				  :depends-on ("packages" "list"))
			 (:module :struct
				  :components ((:file "comparator")
					       (:file "pipe"
						      :depends-on ("comparator"))
					       (:file "bin-tree"
						      :depends-on ("type-class"
								   "comparator"))
					       (:file "heap"
						      :depends-on ("type-class"
								   "comparator"))
					       (:file "type-class")
					       (:file "misc"
						      :depends-on ("pipe"
								   "bin-tree"
								   "heap"))
					       
					       (:file "pool"
						      :depends-on ("heap"
								   "comparator"))
					       )
				  :depends-on ("packages"
					       "list"
					       "misc"
					       "tree"
					       "lazy"
					       "memo"
					       :macro))))))
;;;;@include "src/packages.lisp"

;;;; * Generic helpers

;;;;@include "src/list.lisp"

;;;;@include "src/tree.lisp"

;;;;@include "src/io.lisp"

;;;;@include "src/misc.lisp"

;;;;@include "src/lazy.lisp"

;;;;@include "src/memo.lisp"

;;;;@include "src/macro/meta.lisp"

;;;;@include "src/macro/misc.lisp"

;;;;@include "src/macro/loop.lisp"

;;;;@include "src/macro/anaphorics.lisp"

;;;;@include "src/macro/fun.lisp"

;;;;@include "src/macro/setf.lisp"

;;;; * Special Structures

;;;;@include "src/struct/misc.lisp"

;;;;@include "src/struct/comparator.lisp"

;;;;@include "src/struct/pipe.lisp"

;;;;@include "src/struct/bin-tree.lisp"

;;;;@include "src/struct/heap.lisp"

;;;;@include "src/struct/pool.lisp"
