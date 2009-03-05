;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          math.lisp
;;;; Purpose:       General purpose math functions
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Nov 2002
;;;;
;;;; $Id: math.lisp 10470 2005-04-26 21:46:09Z kevin $
;;;;
;;;; This file, part of KMRCL, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;;
;;;; KMRCL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************


(in-package #:kmrcl)

(defun deriv (f dx)
  #'(lambda (x)
      (/ (- (funcall f (+ x dx)) (funcall f x))
	 dx)))

(defun sin^ (x)
    (funcall (deriv #'sin 1d-8) x))

;;; (sin^ pi)

(defmacro ensure-integer (obj)
  "Ensure object is an integer. If it is a string, then parse it"
  `(if (stringp ,obj)
      (parse-integer ,obj)
     ,obj))

(defun histogram (v n-bins &key min max)
  (declare (fixnum n-bins))
  (when (listp v)
    (setq v (coerce v 'vector)))
  (when (zerop (length v))
    (return-from histogram (values nil nil nil)) )
  (let ((n (length v))
	(bins (make-array n-bins :element-type 'integer :initial-element 0))
	found-min found-max)
    (declare (fixnum n))
    (unless (and min max)
      (setq found-min (aref v 0)
	    found-max (aref v 0))
      (loop for i fixnum from 1 to (1- n)
	  do
	    (let ((x (aref v i)))
	      (cond
	       ((> x found-max)
		(setq found-max x))
	       ((< x found-min)
		(setq found-min x)))))
      (unless min
	(setq min found-min))
      (unless max
	(setq max found-max)))
    (let ((width (/ (- max min) n-bins)))
      (setq width (+ width (* double-float-epsilon width)))
      (dotimes (i n)
	(let ((bin (nth-value 0 (truncate (- (aref v i) min) width))))
	  (declare (fixnum bin))
	  (when (and (not (minusp bin))
		     (< bin n-bins))
	    (incf (aref bins bin))))))
    (values bins min max)))
	      

(defun fixnum-width ()
  (nth-value 0 (truncate (+ (/ (log (1+ most-positive-fixnum)) (log 2)) .5))))

(defun scaled-epsilon (float &optional (operation '+))
  "Return the smallest number that would return a value different from
  FLOAT if OPERATION were applied to FLOAT and this number.  OPERATION
  should be either + or -, and defauls to +."
  (multiple-value-bind (significand exponent)
      (decode-float float)
    (multiple-value-bind (1.0-significand 1.0-exponent)
	(decode-float (float 1.0 float))
      (if (and (eq operation '-)
	       (= significand 1.0-significand))
	  (scale-float (typecase float
			 (short-float short-float-negative-epsilon)
			 (single-float single-float-negative-epsilon)
			 (double-float double-float-negative-epsilon)
			 (long-float long-float-negative-epsilon))
		       (- exponent 1.0-exponent))
	(scale-float (typecase float
		       (short-float short-float-epsilon)
		       (single-float single-float-epsilon)
		       (double-float double-float-epsilon)
		       (long-float long-float-epsilon))
		     (- exponent 1.0-exponent))))))

(defun sinc (x)
  (if (zerop x)
      1d0
    (let ((x (coerce x 'double-float)))
      (/ (sin x) x))))


(defun numbers-within-percentage (a b percent)
  "Determines if two numbers are equal within a percentage difference."
  (let ((abs-diff (* 0.01 percent 0.5 (+ (abs a) (abs b)))))
    (< (abs (- a b)) abs-diff)))
