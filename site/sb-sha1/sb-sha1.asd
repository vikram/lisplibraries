;;; -*-  Lisp -*-

(defpackage #:sb-sha1-system 
  (:use #:cl #:asdf))

(in-package #:sb-sha1-system)

(defsystem sb-sha1
  :depends-on (sb-rotate-byte)
  :version "1.0"
  :components ((:file "sha1")))

(defmethod perform :after ((o load-op) (c (eql (find-system :sb-sha1))))
  (provide 'sb-sha1))

(defmethod perform ((o test-op) (c (eql (find-system :sb-sha1))))
  (operate 'load-op 'sb-sha1-tests)
  (operate 'test-op 'sb-sha1-tests))

(defsystem sb-sha1-tests
  :depends-on (sb-sha1 sb-rt)
  :version "1.0"
  :components ((:file "sha1-tests")))

(defmethod perform ((o test-op) (c (eql (find-system :sb-sha1-tests))))
  (or (funcall (intern "DO-TESTS" (find-package "SB-RT")))
      (error "test-op failed")))
			    