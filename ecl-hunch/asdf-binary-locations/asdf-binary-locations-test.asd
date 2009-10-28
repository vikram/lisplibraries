;;;-*- Mode: Lisp; Package: common-lisp-user -*-

#| simple-header

Author: Gary King

|#

(in-package #:common-lisp-user)
(defpackage #:asdf-binary-locations-system-test (:use #:asdf #:cl))
(in-package #:asdf-binary-locations-system-test)

(defsystem asdf-binary-locations-test
  :author "Gary Warren King <gwking@metabang.com>"
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "MIT Style License"
  :description "tests for asdf-binary-locations."
  :components ((:module 
                "tests"
                :components
		((:module 
		  "setup"
		  :pathname ""
		  :components
		  ((:file "packages")))
		 (:module 
		  "code"
		  :pathname ""
		  :depends-on ("setup")
		  :components
		  ((:file "tests"))))))
  :depends-on (:lift
	       :asdf-binary-locations))
