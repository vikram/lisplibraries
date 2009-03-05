;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          functions.lisp
;;;; Purpose:       Function routines for KMRCL package
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Apr 2000
;;;;
;;;; $Id: functions.lisp 8573 2004-01-29 23:30:50Z kevin $
;;;;
;;;; This file, part of KMRCL, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;;
;;;; KMRCL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package :kmrcl)

(defun memo-proc (fn)
  "Memoize results of call to fn, returns a closure with hash-table"
  (let ((cache (make-hash-table :test #'equal)))
    #'(lambda (&rest args)
        (multiple-value-bind (val foundp) (gethash args cache)
          (if foundp
              val
	    (setf (gethash args cache) (apply fn args)))))))

(defun memoize (fn-name)
  (setf (fdefinition fn-name) (memo-proc (fdefinition fn-name))))

(defmacro defun-memo (fn args &body body)
  "Define a memoized function"
  `(memoize (defun ,fn ,args . ,body)))

(defmacro _f (op place &rest args)
  (multiple-value-bind (vars forms var set access) 
                       (get-setf-expansion place)
    `(let* (,@(mapcar #'list vars forms)
            (,(car var) (,op ,access ,@args)))
       ,set)))

(defun compose (&rest fns)
  (if fns
      (let ((fn1 (car (last fns)))
            (fns (butlast fns)))
        #'(lambda (&rest args)
            (reduce #'funcall fns 
                    :from-end t
                    :initial-value (apply fn1 args))))
      #'identity))

