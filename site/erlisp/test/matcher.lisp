;;;; Copyright (c) 2004-2005, Dirk H.P. Gerrits
;;;; All rights reserved.
;;;;
;;;; This software is licensed under the terms of the BSD license, stated in the
;;;; file named LICENSE.
;;;;
;;;;----------------------------------------------------------------------------
;;;;
;;;; This file tests the pattern matcher for messages.
;;;;
;;;;----------------------------------------------------------------------------

(in-package :erlisp)
(def-suite :erlisp.matcher :in :erlisp)
(in-suite :erlisp.matcher)

(test match-with-matcher
  (test match-with-matcher.cond-matcher
    (let ((object (list 1 2 3 4 5)))
      (is (eq (match-with-matcher (cond-matcher) object)
              nil))
      (is (eq (match-with-matcher (cond-matcher) object
                (nil 'foo))
              nil))
      (is (eq (match-with-matcher (cond-matcher) object
                (t 'foo))
              'foo))
      (is (eql (match-with-matcher (cond-matcher) object
                 ((null it) 0)
                 ((= (length it) 5) 1)
                 ((= (apply #'+ it) 15) 2)
                 (t 3))
               1))
      (is (eql (match-with-matcher (cond-matcher foo) object
                 ((null foo) 0)
                 ((= (length foo) 5) 1)
                 ((= (apply #'+ foo) 15) 2)
                 (t 3))
               1))))

  (test match-with-matcher.case-matcher
    (let ((object 42))
      (is (eq (match-with-matcher (case-matcher) object)
              nil))
      (is (eq (match-with-matcher (case-matcher) object
                (nil 'foo))
              nil))
      (is (eq (match-with-matcher (case-matcher) object
                (t 'foo))
              'foo))
      (is (eq (match-with-matcher (case-matcher) object
                (bar 0)
                (42 it)
                ((41 42 43) 2)
                (otherwise 3))
              object))
      (is (eq (match-with-matcher (case-matcher foo) object
                (bar 0)
                (42 foo)
                ((41 42 43) 2)
                (otherwise 3))
              object)))))

(test match
  (let ((*default-pattern-matcher* 'cond-matcher))
    (is (eql (eval '(let ((object (list 1 2 3 4 5)))
                     (match object
                       ((null it) 0)
                       ((= (length it) 5) 1)
                       ((= (apply #'+ it) 15) 2)
                       (t 3))))
             1))))
