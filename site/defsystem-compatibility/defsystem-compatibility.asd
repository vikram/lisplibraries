#| copyright

See the file COPYING for details

|#

(in-package #:common-lisp-user)
(defpackage #:defsystem-compatibility-system
  (:use #:common-lisp #:asdf))
(in-package #:defsystem-compatibility-system)

;; try hard
(unless (find-system 'asdf-system-connections nil)
  (format t ";;; Note: The defsystem-compatibility system is happier when it can find asdf-system-connections. See http://www.cliki.net/asdf-system-connections for details and download instructions."))

(defsystem defsystem-compatibility
  :version "0.1.1"
  :author "Gary Warren King <gwking@metabang.com>"
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "MIT Style license"
  :description "Make many defsystems look the same!"
  :components ((:module 
		"dev"
		:components ((:file "package")
			     (:file "api"
				    :depends-on ("package"))
			     (:file "defsystem-compatibility"
				    :depends-on ("api"))
			     (:file "asdf"
				    :depends-on ("api"))))
               
               (:module 
		"website"
		:components
		((:module "source"
			  :components ((:static-file "index.lml"))))))
  :in-order-to ((test-op (load-op defsystem-compatibility-test)))
  :perform (test-op :after (op c)
                    (describe 
		     (funcall (intern (symbol-name '#:run-tests) :lift) 
			      :suite '#:defsystem-compatibility-test)))
    :depends-on (:metatilities-base))

;; testing is never done...
(defmethod operation-done-p 
           ((o test-op)
            (c (eql (find-system 'defsystem-compatibility))))
  (values nil))

#+asdf-system-connections
(asdf:defsystem-connection defsystem-compatibility-and-metatilities
  :requires (defsystem-compatibility metatilities-base)
  :perform (load-op :after (op c)
                    (use-package (find-package :defsystem-compatibility) 
                                 (find-package :metatilities))))
