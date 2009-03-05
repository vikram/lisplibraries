;; This software is Copyright (c) 2006, Steffen Siering.
;; <COMPANY> grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :mycl-util)

;;;; ** Anaphoric macros

(defmacro if-bind (var expr then &optional else)
  `(let ((,var ,expr))
    (if ,var
	,then
	,(when else
	      else))))

(defmacro aif (expr then &optional else)
  `(if-bind it ,expr ,then ,else))

(defmacro when-bind (var expr &body then)
  `(if-bind ,var ,expr (progn ,@then)))

(defmacro awhen (expr &body body)
  `(when-bind it ,expr ,@body))
