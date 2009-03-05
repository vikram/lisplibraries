#| copyright

See the file COPYING for details

|#

(in-package #:common-lisp-user)
(defpackage #:defsystem-compatibility-test-system
  (:use #:common-lisp #:asdf))
(in-package #:defsystem-compatibility-test-system)


(defsystem defsystem-compatibility-test
  :version "0.1"
  :author "Gary Warren King <gwking@metabang.com>"
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "MIT Style license"
  :description "Tests for defsystem-compatibility!"
  :components ((:module 
		"unit-tests"
		:components ((:file "package")
			     (:file "tests"
				    :depends-on ("package")))))
  :depends-on (defsystem-compatibility lift))



