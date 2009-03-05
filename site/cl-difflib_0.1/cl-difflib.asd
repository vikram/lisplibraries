;;; ------------------------------------------------- -*- Mode: LISP -*-
;;; CL-DIFFLIB -- A Lisp library for computing differences between
;;; sequences.
;;;
;;; Copyright 2005 
;;; John Wiseman (jjwiseman@yahoo.com)
;;; 2005-02-02
;;;
;;; Licensed under the MIT license--see the accompanying LICENSE.txt
;;; file.
;;;
;;; This is the ASDF system definition.

(in-package :asdf)

(defsystem :cl-difflib
    :name "CL-DIFFLIB"
    :author "John Wiseman <jjwiseman@yahoo.com>"
    :version "0.1"
    :maintainer "John Wiseman <jjwiseman@yahoo.com>"
    :licence "MIT"
    :description "A Lisp library for computing differences between sequences."
    :long-description "A Lisp library for computing differences between sequences.  Based on Python's difflib module."
    
    :components ((:file "package")
		 (:file "difflib" :depends-on ("package"))))

(defmethod perform ((o test-op) (c (eql (find-system 'cl-difflib))))
  (oos 'load-op 'cl-difflib-tests)
  (oos 'test-op 'cl-difflib-tests :force t))