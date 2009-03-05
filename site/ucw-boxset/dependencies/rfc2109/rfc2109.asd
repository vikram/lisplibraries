;-*- mode: lisp -*-
(in-package :cl-user)

(defpackage #:rfc2109-system
    (:use #:cl #:asdf))

(in-package #:rfc2109-system)

(defclass load-file-with-tests (cl-source-file)
  ())

(defmethod perform ((op load-op) (component load-file-with-tests))
  (let ((*features* *features*))
    (push :test *features*)
    (perform (make-instance 'compile-op) component)
    (call-next-method)))

(defsystem "rfc2109"
  :depends-on (:split-sequence)
  :version "0.4"
  :components
  ((:file "rfc2109")))

(defsystem "rfc2109.test"
  :depends-on (:split-sequence :fiveam)
  :components
  ((:load-file-with-tests "rfc2109")))
