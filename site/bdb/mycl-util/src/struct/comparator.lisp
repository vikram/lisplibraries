;; This software is Copyright (c) 2006, Steffen Siering.
;; <COMPANY> grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :mycl-util)

;;;; ** Comparator

(defvar *comparator* nil)

(defun build-comparator (eql &key < >)
  (if (and (not <)
	   (not >))
      (error "needs < or > key")
      (lambda (v1 v2)
	(cond ((funcall eql v1 v2) 'equals)
	      (< (if (funcall < v1 v2)
		     'less-than
		     'greater-than))
	      (> (if (funcall > v1 v2)
		     'greater-than
		     'lesser-than))))))

(defun comp-= (v1 v2 &optional (comp *comparator*))
  (eq 'equals (funcall comp v1 v2)))

(defun comp-< (v1 v2 &optional (comp *comparator*))
  (eq 'less-than (funcall comp v1 v2)))

(defun comp-> (v1 v2 &optional (comp *comparator*))
  (eq 'greater-than (funcall comp v1 v2)))

(setf (symbol-function 'comp-<=) (complement #'comp->))
(setf (symbol-function 'comp->=) (complement #'comp-<))
(setf (symbol-function 'comp-/=) (complement #'comp-=))

(defmacro with-comparator (comp &body body)
  `(let ((*comparator* ,comp))
    (declare (special *comparator*))
    ,@body))

(defmacro number-comparator ()
  `(build-comparator #'= :< #'<))

(defmacro char-comparator ()
  `(build-comparator #'= :< #'<))

(defmacro string-comparator ()
  `(build-comparator #'string= :< #'string<))
