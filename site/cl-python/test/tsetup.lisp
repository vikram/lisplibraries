;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLPYTHON.TEST -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;;;; CLPython test harness setup

(defpackage :clpython.test
  (:use :common-lisp :clpython)
  (:import-from :clpython #:in-syntax)
  (:import-from :clpython.parser #:parse)
  (:import-from :ptester #:*announce-test* #:with-tests
                #:test #:test-warning)
  (:export #:run))

(in-package :clpython.test)

(defun form-without-compiler-warnings (form)
  ;; Prevent warnings and compiler notes from failing a test.
  `(handler-bind ((warning #'muffle-warning)
                  #+sbcl (sb-int:simple-compiler-note #'muffle-warning)
                  ;; Allegro's excl::compiler-note does not come with a muffle restart, unfortunately.
                  ;; e.g. "Closure .. will be stack allocated." interferes with test-no-error...
                  )
     ,form))

(defmacro test-no-error (form &rest args)
  `(ptester:test-no-error ,(form-without-compiler-warnings form) ,@args))

(defmacro test-error (form &rest args)
  `(ptester:test-error ,(form-without-compiler-warnings form) ,@args))
    
(defmacro test-true (val &rest options)
  "Only tests first value returned"
  `(test t (not (null ,val)) ,@options))

(defmacro test-false (val &rest options)
  "Only tests first value returned"
  `(test t (not ,val) ,@options))

(defmacro test-equal (&rest args)
  `(test ,@args :test 'equal))

(defconstant +compilation-warnings-muffled-by-compiler+
    #.(progn
        (define-condition c1 (warning) ())
        (defmacro m1 () (warn 'c1))
        (defun foo1 () (m1))
        (let (muffled)
          (handler-case
              (with-output-to-string (*standard-output*)
                (progn (compile 'foo1) (setf muffled t)))
            (c1 () (setf muffled nil)))
          muffled))
    "Whether warnings signalled during compilation by a macro-expansion can be
caught outside the COMPILE form. ANSI 3.2.5 \"Exceptional Situations in the Compiler\"
seems to give implementations some freedom here. (In practice: Allegro=NIL, LisWorks=T")

(defmacro test-some-warning (&rest args)
  `(test-warning ,@args
                 :known-failure +compilation-warnings-muffled-by-compiler+
                 :fail-info (format nil "~S = ~S"
                                    '+compilation-warnings-muffled-by-compiler+
                                    +compilation-warnings-muffled-by-compiler+)))
  
(defvar *test-active* nil)

(defmacro with-subtest (options &body body)
  ;; Enable nesting WITH-TESTS, with total results only being printed once
  ;; at the end.
  `(flet ((run-body () ,@body))
     (if *test-active*
	 (progn (format t "~%[subtest: ~A]~%" ,(getf options :name))
		(run-body))
       (let ((*test-active* t)
             (*announce-test* t))
	 (with-tests ,options (run-body))))))

(defun seq-equal (x y &key (test 'eq) ignore silent)
  "Are X and Y equal sequences (apart from items in IGNORE)"
  (let* ((i2 (coerce ignore 'list))
	 (x2 (set-difference (coerce x 'list) i2 :test test))
	 (y2 (set-difference (coerce y 'list) i2 :test test)))
    (let ((sd1 (set-difference x2 y2 :test test))
          (sd2 (set-difference y2 x2 :test test)))
      (if (and (null sd1)
               (null sd2))
          t
        (progn (unless silent
                 (when (or sd1 sd2)
                   (format t "~%seq-equal deviations between ~A and ~A: ~A; ~A~%"
                           x y sd1 sd2)))
               nil)))))

(defun seq-member (item seq)
  (not (null (position item seq))))

(defun test-comp-testfunc ()
  "Test the test functions"
  (with-subtest (:name "CLPython-Testfunc")
    (test-false (seq-equal '(a) () :silent t))
    (test-true  (seq-equal '(a) () :ignore #(a)))
    (test-false (seq-equal '(a) '(b) :silent t))
    (test-false (seq-equal '(a b) '(b) :silent t))
    (test-false (seq-equal #(a b) #(b) :silent t))
    (test-true  (seq-equal #(a b) #(b) :ignore '(a)))
    (test-true  (seq-member 'a #(a b)))
    (test-true  (seq-member 'a '(a b)))
    (test-false (seq-member 'a #(b)))
    (test-false (seq-member 'a '(b)))))

(defun run-tests ()
  (with-subtest (:name "CLPython")
    (test-comp-testfunc)
    (run-parser-test)
    (run-compiler-test)
    (run-code-walker-test)
    (run-pretty-printer-test)
    (run-lang-test)
    (run-builtin-test)
    (run-mod-string-test)
    (run-mod-math-test)))
