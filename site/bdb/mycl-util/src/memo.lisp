;; This software is Copyright (c) 2006, Steffen Siering.
;; <COMPANY> grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :mycl-util)

;;;; ** Memoization

(defun gen-memo (fn key test)
  (let ((table (make-hash-table :test test)))
    (values table
	    (lambda (&rest args)
	      (let ((k (funcall key args)))
		(multiple-value-bind (val found-p) (gethash k table)
		  (if found-p
		      val
		      (setf (gethash k table)
			    (apply fn args)))))))))

(defun memo (fn name key test)
  (multiple-value-bind (table fun)
      (gen-memo fn key test)
    (setf (get name 'memo) table)
    fun))

(defun memoize (fn-name &key (key #'first) (test #'eql))
  (setf (symbol-function fn-name)
	(memo (symbol-function fn-name) fn-name key test)))

(defun clear-memoize (fn-name)
  (let ((table (get fn-name 'memo)))
    (when table (clrhash table))))

(defun tmp-memoize (fn &key (key #'first) (test #'eql))
  (multiple-value-bind (val fun)
      (gen-memo fn key test)
    fun))
