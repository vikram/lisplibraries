;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          random.lisp
;;;; Purpose:       Random number functions for KMRCL package
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Apr 2000
;;;;
;;;; $Id$
;;;;
;;;; This file, part of KMRCL, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;;
;;;; KMRCL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:kmrcl)

(defun seed-random-generator ()
  "Evaluate a random number of items"
  (let ((randfile (make-pathname
                   :directory '(:absolute "dev")
                   :name "urandom")))
    (setf *random-state* (make-random-state t))
    (if (probe-file randfile)
        (with-open-file
            (rfs randfile :element-type 'unsigned-byte)
          (let*
              ;; ((seed (char-code (read-char rfs))))
              ((seed (read-byte rfs)))
            ;;(format t "Randomizing!~%")
            (loop
                for item from 1 to seed
                do (loop
                       for it from 0 to (+ (read-byte rfs) 5)
                       do (random 65536))))))))


(defmacro random-choice (&rest exprs)
  `(case (random ,(length exprs))
     ,@(let ((key -1))
         (mapcar #'(lambda (expr)
                     `(,(incf key) ,expr))
                 exprs))))

