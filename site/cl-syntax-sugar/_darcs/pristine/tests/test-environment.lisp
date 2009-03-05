;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-syntax-sugar-test)

(defsuite* (test :in root-suite))

(defmacro deftest (name args &body body)
  `(stefil:deftest ,name ,args
     ;; it's not strictly necessary because Stefil rebinds and copies *readtable* but let's
     ;; just don't test Stefil itself and make sure we have a cloned readtable.
     (with-local-readtable
       ;; it's pretty much needed for anything we do in the tests
       (enable-readtime-wrapper-syntax)
       ;; Stefil captures the value of *package* at compile time and binds it when running
       ;; the tests, but again, let's just not test Stefil here...
       (bind ((*package* (find-package :cl-syntax-sugar-test)))
         ,@body))))
