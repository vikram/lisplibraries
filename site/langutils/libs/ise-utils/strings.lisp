;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          strings.lisp
;;;; Purpose:       Random string utilities
;;;; Programmer:    Ian S Eslick
;;;; Date Started:  March 2004
;;;;
;;;; *************************************************************************

(in-package :utils)

(defmacro-exported with-string-stream ((label) &rest body)
  "Binds label to a string output stream and returns the
   string representing the writes to that stream"
  `(let ((,label (make-string-output-stream)))
     (progn 
       ,@body
      (get-output-stream-string ,label))))

(defun-exported mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun-exported symb (&rest args)
  (values (intern (apply #'mkstr args))))

;; -----------------------------------
;; Rule and data file utilities

(defun-exported strncmp ( string1 string2 count start )
  (declare #-mcl (type fixnum count start)
	   #-mcl (type simple-string string1 string2)
	   (optimize speed (safety 0)))
  (ignore-errors 
    (loop for i fixnum from start to (+ start count -1)
          finally (return t) do
;;      (format t "~A ~A" (char string1 i) (char string2 i))
      (when (neq (schar string1 i) (schar string2 i))
	(return nil)))))

(defun-exported strncmp-end2 ( string1 string2 count start )
  (declare #-mcl (type fixnum count start)
	   #-mcl (type simple-string string1 string2)
	   (inline schar)
	   (optimize speed (safety 0)))
;;  (ignore-errors 
    (loop for i fixnum from 0 to (1- count) 
          finally (return t) do
;;      (format t "~A ~A" (char string1 i) (char string2 i))
      (when (neq (schar string1 i) (schar string2 (+ i start)))
	(return nil))))


				       