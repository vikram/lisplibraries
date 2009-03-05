;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-syntax-sugar-test)

(defsuite* (test/feature-cond :in test))

(defmacro with-features (features &body body)
  `(bind ((*features* (list* ,@features *features*)))
     ,@body))

(defmacro define-feature-cond-test (name test-form &body tests)
  (with-unique-names (form)
    `(deftest ,name ()
       (enable-feature-cond-syntax)
       (bind ((,form ,test-form))
         ,@(loop
              :for entry :in tests
              :collect `(with-features ,(first entry)
                          (is (equal ,(second entry) (read-from-string ,form)))))))))

(define-feature-cond-test test/feature-cond/simple
    ｢#*((:foo "it's foo")
        (:bar "it's bar")
        (t "it's default"))｣
  ((:foo :bar) "it's foo")
  ((:bar) "it's bar")
  (() "it's default"))

(define-feature-cond-test test/feature-cond/without-default
    ｢#*((:foo "it's foo")
        (:bar "it's bar"))｣
  ((:foo :bar) "it's foo")
  ((:bar) "it's bar")
  (() nil))

(define-feature-cond-test test/feature-cond/with-and-or-not
    ｢#*(((and :foo :bar) "(and :foo :bar)")
        ((and (or :foo :bar) :baz)  "(and (or :foo :bar) :baz)")
        ((and (or :foo :bar) (not :zork)) "(and (or :foo :bar) (not :zork))")
        ((or :foo :bar) "(or :foo :bar)")
        (t "default"))｣
  ((:foo :bar) "(and :foo :bar)")
  ((:foo :zork) "(or :foo :bar)")
  ((:foo) "(and (or :foo :bar) (not :zork))")
  (() "default"))

(define-feature-cond-test test/feature-cond/with-custom-expressions
    ｢#*(((and :foo (find-symbol "DEFTEST" :stefil)) "(and :foo (find-symbol '#:deftest :stefil))")
        ((and (or :foo (< 5 10)) :baz)  "(and (or :foo (< 5 10)) :baz)")
        ((or :foo :bar t) "(or :foo :bar t)"))｣
  ((:foo) "(and :foo (find-symbol '#:deftest :stefil))")
  ((:baz) "(and (or :foo (< 5 10)) :baz)")
  ((:bar) "(or :foo :bar t)")
  (() "(or :foo :bar t)"))
