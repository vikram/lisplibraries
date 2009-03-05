#| copyright

See the file COPYING for details

|#

(defpackage #:metatilities-base-system (:use #:asdf #:common-lisp))
(in-package #:metatilities-base-system)

(defsystem metatilities-base
  :author "Gary Warren King <gwking@metabang.com>"
  :version "0.6.6"
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "MIT Style license"
  :description "These are metabang.com's Common Lisp basic utilities."
  :long-description "These are metabang.com's Common Lisp basic utilities and what not."
  :components ((:module
		"setup"
		:pathname "dev/"
		:components
		((:file "package")))
	       (:module 
		"dev"
		:depends-on ("setup")
		:components 
		((:file "api")
		 (:file "l0-utils"
			:depends-on ("api"))
		 (:file "l0-strings"
			:depends-on ("api"))
		 (:file "l0-macros"
			:depends-on ("api" "l0-utils"))
		 (:file "l0-arrays"
			:depends-on ("api"))
		 (:file "l0-clos"
			:depends-on ("api"))
		 (:file "l0-files"
			:depends-on ("api" "l0-macros"))
		 (:file "l0-time"
			:depends-on ("l0-macros"))
		 (:file "set-equal"
			:depends-on ("api"))
		 (:file "generic-lisp"
			:depends-on ("api"))
		 (:file "generic-interface"
			:depends-on ("api" "generic-lisp" "l0-macros"))
		 (:file "defclass-star"
			:depends-on ("api" "l0-macros"))
		 (:file "copy-file")
		 #+(or)
		 (:file "define-class"
			:depends-on ("api" "defclass-star"))))
	       )
  :in-order-to ((test-op (load-op metatilities-base-test)))
  :perform (test-op :after (op c)
		    (funcall (intern (symbol-name '#:run-tests) :lift) 
			     :config :generic)))

(defmethod operation-done-p 
           ((o test-op) (c (eql (find-system 'metatilities-base))))
  (values nil))
