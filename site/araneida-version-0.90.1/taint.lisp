(defpackage "CL-TAINT"
  (:use "COMMON-LISP" #+fiveam :it.bese.fiveam)
  (:export :taint :untaint :tainted-p)
  (:documentation "Simple package to taint and untaint values.
The basic idea is to wrap a value in a function, making it unusable.
This means you have to explicitly untaint it to get back a useful value.

Usage follows this pattern: your functions that return data from the outside
(such as parameters from a web client) return their value via (taint value).
Later, when the function is desired, you use (untaint #'parse-integer tainted-value)
or the like."))

(in-package cl-taint)

(defun taint (value)
  "Taints a value
Wraps a value in a lambda so that it no longer makes sense by itself."
  (lambda () value))

(defun untaint (func tainted-value)
  "Untaints a value, returning it
Takes a function of one argument which will be passed the tainted value."
  (funcall func (funcall tainted-value)))

(defun tainted-p (value)
  (functionp value))