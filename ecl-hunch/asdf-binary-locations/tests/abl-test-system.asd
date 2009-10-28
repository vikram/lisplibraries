;;;-*- Mode: Lisp; Package: common-lisp-user -*-

#| simple-header

Author: Gary King

|#

(in-package #:common-lisp-user)
(defpackage #:abl-test-system-system (:use #:asdf #:cl))
(in-package #:abl-test-system-system)

(defclass abl-foo-source-file (source-file)
  ())

(defmethod output-files ((operation compile-op) (c abl-foo-source-file))
  (list (merge-pathnames (make-pathname :type "xxx") (component-pathname c))))

(defmethod source-file-type ((c abl-foo-source-file) (s t)) "ablfoo")

(defsystem abl-test-system
  :components ((:module 
                "dev"
                :components
		((:file "main")
		 (:abl-foo-source-file "test-source") 
		 (:static-file "notes.text")))))

