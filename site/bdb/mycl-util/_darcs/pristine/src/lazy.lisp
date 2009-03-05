;; This software is Copyright (c) 2006, Steffen Siering.
;; <COMPANY> grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :mycl-util)

;;;; ** Lazy Evaluation

(defconstant +unforced+ (gensym))

;(defstruct delay (value +unforced+) (function nil))
(defun make-delay (fn)
  "creates a new delay object"
  (cons +unforced+ fn))

(defun delay-function (delay)
  "returns the function used by a delay"
  (cdr delay))

(defun delay-value (delay)
  "returns the value hold by a delay"
  (car delay))

(defun (setf delay-value) (val delay)
  (setf (car delay) val))

(defun (setf delay-function) (val delay)
  (setf (cdr delay) val))

(defun delay-p (x)
  (and (consp x)
       (or (eq (delay-value x) +unforced+)
	   (null (delay-function x)))))

(defmacro delay (&body body)
  "delays body for lazy evaluation"
  `(make-delay (lambda () . ,body)))

(defun force (x)
  "forecs a delay-object to be executed"
  (if (not (delay-p x))
      x
      (progn
	(when (delay-function x)
	  (setf (delay-value x)
		(funcall (delay-function x)))
	  (setf (delay-function x) nil))
	(delay-value x))))
