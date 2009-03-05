;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2007 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-def)

(defpackage :cl-def-test
    (:use :common-lisp :cl-def :metabang-bind :alexandria :iterate :stefil)
  (:shadow)
  (:export #:test))

(eval-always
  (import
   '(enable-sharp-boolean-syntax rebind eval-always)
   (find-package :cl-def-test)))

(in-package :cl-def-test)

(defsuite* (test :in root-suite))

(deftest test/function ()
  (is (equal '(progn
               (declaim (inline foo))
               (locally (declare (optimize (speed 0) (debug 3)))
                 (export 'foo)
                 (defun foo (bar baz unused)
                   "documentation"
                   (declare (ignore unused))
                   (+ bar baz))))
             (macroexpand-1 '(def (function ioed) foo (bar baz unused)
                              "documentation"
                              (declare (ignore unused))
                              (+ bar baz))))))

(deftest test/method ()
  (is (equal '(progn
               (locally
                   (declare (optimize (speed 0) (debug 3)))
                 (export 'foo)
                 (defmethod foo ((bar integer) (baz string) unused)
                   "documentation"
                   (declare (ignore unused))
                   (+ bar baz))))
             (macroexpand-1 '(def (method oed) foo ((bar integer) (baz string) unused)
                              "documentation"
                              (declare (ignore unused))
                              (+ bar baz))))))

(deftest test/test ()
  (is (equal '(progn
               (locally
                   (deftest foo (bar baz unused)
                     "documentation"
                     (declare (ignore unused))
                     (+ bar baz))))
             (macroexpand-1 '(def test foo (bar baz unused)
                              "documentation"
                              (declare (ignore unused))
                              (+ bar baz))))))

(deftest test/constant ()
  (is (equal '(progn
               (eval-when (:compile-toplevel :load-toplevel :execute)
                 (defconstant +foo+ (cl-def::%reevaluate-constant '+foo+ 1 :test 'eql)
                   "documentation")))
             (macroexpand-1 '(def constant +foo+ 1 "documentation")))))

(deftest test/special-variable ()
  (is (equal '(progn
               (progn
                 (setf (documentation '+foo+ 'variable) "documentation")
                 (defvar +foo+)
                 (makunbound '+foo+)
                 (setf +foo+ 1)))
             (macroexpand-1 '(def special-variable +foo+ 1 "documentation"))))
  (is (equal '(progn
               (progn
                 (setf (documentation '+foo+ 'variable) "documentation")
                 (defvar +foo+)
                 (makunbound '+foo+)))
             (macroexpand-1 '(def (special-variable :documentation "documentation") +foo+)))))

(def special-variable *foo*)

(def with-macro with-foo1 (foo)
  (let ((*foo* foo))
    (-body-)))

(def with-macro with-foo2 (foo bar)
  (let* ((local (* 2 foo))
         (*foo* (+ local bar)))
    ;; using this syntax, LOCAL is "transferred" into the lexical scope of the body
    (-body- local)))

(def with-macro* with-foo3 (foo &key bar)
  (let* ((local (* 2 foo))
         (*foo* (+ local bar)))
    (-body- local)))

(def with-macro* with-foo4 (new-var-name foo &key bar)
  (let* ((local (* 2 foo))
         (*foo* (+ local bar)))
    (-body- (local new-var-name))))

(def with-macro* with-foo5 (new-var-name foo &rest args &key bar (keyword-defaulting (+ 2 2)) &allow-other-keys)
  (let* ((local (* 2 foo))
         (*foo* (+ local bar)))
    (-body- (local new-var-name))
    (list* keyword-defaulting args)))

(deftest test/with-macro ()
  (with-foo1 42
    (is (= *foo* 42)))
  (with-foo2 2 6
    (is (= local 4))
    (is (= *foo* 10)))
  (with-foo3 (2 :bar 6) ; there's a full arglist, because it's a with-macro*
    (is (= local 4))    ; LOCAL is visible in the lexical scope
    (is (= *foo* 10)))
  (with-foo4 (zork 2 :bar 6) ; there's a full arglist, because it's a with-macro*
    (is (= zork 4))    ; LOCAL is visible in the lexical scope by the name given in VAR-NAME
    (is (= *foo* 10)))
  (is (equal '(4 :bar 6 :baz 42)
             (with-foo5 (zork 2 :bar 6 :baz 42)
               (is (= zork 4))
               (is (= *foo* 10))))))

