;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          cl-symbols.lisp
;;;; Purpose:       Returns all defined Common Lisp symbols
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Apr 2000
;;;;
;;;; $Id: symbols.lisp 9652 2004-06-17 20:32:00Z kevin $
;;;;
;;;; This file, part of KMRCL, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;;
;;;; KMRCL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:kmrcl)

(defun cl-symbols ()
  (append (cl-variables) (cl-functions)))

(defun cl-variables ()
  (let ((vars '()))
    (do-symbols (s 'common-lisp)
      (multiple-value-bind (sym status)
	  (find-symbol (symbol-name s) 'common-lisp)
	(when (and (or (eq status :external)
		       (eq status :internal))
		   (boundp sym))
	  (push sym vars))))
    (nreverse vars)))

(defun cl-functions ()
  (let ((funcs '()))
    (do-symbols (s 'common-lisp)
      (multiple-value-bind (sym status)
	(find-symbol (symbol-name s) 'common-lisp)
	(when (and (or (eq status :external)
		       (eq status :internal))
		   (fboundp sym))
	  (push sym funcs))))
    (nreverse funcs)))

;;; Symbol functions

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (char= #\a (schar (symbol-name '#:a) 0))
    (pushnew :kmrcl-lowercase-reader *features*))
  (when (not (string= (symbol-name '#:a)
		      (symbol-name '#:A)))
    (pushnew :kmrcl-case-sensitive *features*)))

(defun string-default-case (str)
  #+(and (not kmrcl-lowercase-reader)) (string-upcase str)
  #+(and kmrcl-lowercase-reader) (string-downcase str))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setq cl:*features* (delete :kmrcl-lowercase-reader *features*))
  (setq cl:*features* (delete :kmrcl-case-sensitive *features*)))

(defun concat-symbol-pkg (pkg &rest args)
  (declare (dynamic-extent args))
  (flet ((stringify (arg)
           (etypecase arg
             (string
              (string-upcase arg))
             (symbol
              (symbol-name arg)))))
    (let ((str (apply #'concatenate 'string (mapcar #'stringify args))))
      (nth-value 0 (intern (string-default-case str)
			   (if pkg pkg *package*))))))


(defun concat-symbol (&rest args)
  (apply #'concat-symbol-pkg nil args))

(defun ensure-keyword (name)
  "Returns keyword for a name"
  (etypecase name
    (keyword name)
    (string (nth-value 0 (intern (string-default-case name) :keyword)))
    (symbol (nth-value 0 (intern (symbol-name name) :keyword)))))

(defun ensure-keyword-upcase (desig)
  (nth-value 0 (intern (string-upcase
			(symbol-name (ensure-keyword desig))) :keyword)))

(defun ensure-keyword-default-case (desig)
  (nth-value 0 (intern (string-default-case
			(symbol-name (ensure-keyword desig))) :keyword)))

(defun show (&optional (what :variables) (package *package*))
  (ecase what
    (:variables (show-variables package))
    (:functions (show-functions package))))

(defun show-variables (package)
  (do-symbols (s package)
    (multiple-value-bind (sym status)
	(find-symbol (symbol-name s) package)
      (when (and (or (eq status :external)
		     (eq status :internal))
		 (boundp sym))
	(format t "~&Symbol ~S~T -> ~S~%"
		sym
		(symbol-value sym))))))

(defun show-functions (package)
  (do-symbols (s package)
    (multiple-value-bind (sym status)
	(find-symbol (symbol-name s) package)
      (when (and (or (eq status :external)
		     (eq status :internal))
		 (fboundp sym))
	(format t "~&Function ~S~T -> ~S~%"
		sym
		(symbol-function sym))))))

(defun find-test-generic-functions (instance)
  "Return a list of symbols for generic functions specialized on the
class of an instance and whose name begins with the string 'test-'"
  (let ((res)
	(package (symbol-package (class-name (class-of instance)))))
    (do-symbols (s package)
      (multiple-value-bind (sym status)
	  (find-symbol (symbol-name s) package)
	(when (and (or (eq status :external)
		       (eq status :internal))
		   (fboundp sym)
		   (eq (symbol-package sym) package)
		   (> (length (symbol-name sym)) 5)
		   (string-equal "test-" (subseq (symbol-name sym) 0 5))
		   (typep (symbol-function sym) 'generic-function)
		   (plusp 
		    (length 
		     (compute-applicable-methods 
		      (ensure-generic-function sym)
		      (list instance)))))
	  (push sym res))))
    (nreverse res)))

(defun run-tests-for-instance (instance)
  (dolist (gf-name(find-test-generic-functions instance))
    (funcall gf-name instance))
  (values))
