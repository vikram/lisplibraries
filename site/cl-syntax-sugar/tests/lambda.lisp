;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-syntax-sugar-test)

(eval-when (:compile-toplevel)
  (setup-readtable))

(defsuite* (test/lambda :in test))

(deftest test/lambda/installed-on-square-braces ()
  (is (equal (funcall {(with-lambda-with-bang-args-syntax :start-character #\[ :end-character #\])
                         [+ !1 40]}
                      2)
             42)))

(deftest test/lambda/argument-ignoring ()
  (is (equal (funcall (λ (a _ b)
                        (* a b))
                      21 0 2)
             42)))
