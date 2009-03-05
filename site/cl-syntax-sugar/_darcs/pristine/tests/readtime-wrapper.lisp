;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-syntax-sugar-test)

(defsuite* (test/readtime-wrapper :in test))

(defmacro define-readtime-wrapper-test (name args &body body)
  `(deftest ,name ,args
     ,@(mapcar (lambda (entry)
                 (if (member (first entry) '(signals is))
                     entry
                     (bind (((comparator expected string) entry))
                       `(is (,comparator ,expected
                                         (read-from-string ,string))))))
               body)))

(define-readtime-wrapper-test test/readtime-wrapper/with-package ()
  (eq 'cl-syntax-sugar::foo
      "{(with-package :cl-syntax-sugar)
        foo}")
  (equal '(progn
           cl-syntax-sugar::foo
           cl-syntax-sugar::bar)
         "{(with-package :cl-syntax-sugar)
           foo
           bar}")
  (string= "foo"
           "{(with-package :cl-syntax-sugar)
             \"foo\"}"))

(define-readtime-wrapper-test test/readtime-wrapper/sharp-boolean ()
  (signals reader-error (read-from-string "#t"))
  (eq t
      "{with-sharp-boolean-syntax
        #t}")
  (eq nil
      "{with-sharp-boolean-syntax
        #f}")
  (signals reader-error (read-from-string "#f")))
