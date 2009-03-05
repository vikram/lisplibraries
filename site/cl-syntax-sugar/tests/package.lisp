;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(cl:in-package :common-lisp-user)

(defpackage :cl-syntax-sugar-test
  (:use
   #:common-lisp
   :cl-syntax-sugar
   :cl-syntax-sugar-unicode
   :cl-walker
   :stefil
   :alexandria
   :metabang-bind
   :iterate
   )

  (:export
   #:test
   #:test/lambda
   )

  (:shadow
   #:deftest))

(in-package :cl-syntax-sugar-test)

(defun setup-readtable ()
  ;; these are the readers that we enable in the toplevel test package.
  (enable-readtime-wrapper-syntax)
  (enable-string-quote-syntax #\｢ #\｣))

(register-readtable-for-swank
 '(:cl-syntax-sugar-test) 'setup-readtable)

(defmacro define-test-package-with-syntax (name &body body)
  (bind ((setup-readtable-name `(intern "SETUP-READTABLE" ,name)))
    `(progn
       (defpackage ,name
         (:use #:common-lisp :cl-syntax-sugar :cl-walker :cl-syntax-sugar-test :stefil :alexandria :metabang-bind :iterate))
       (setf (fdefinition ,setup-readtable-name)
             (lambda ()
               ,@body))
       (register-readtable-for-swank
        ',name ,setup-readtable-name))))

(define-test-package-with-syntax :cl-syntax-sugar-test.sharp-l
  (setup-readtable)
  (enable-sharp-l-syntax))
