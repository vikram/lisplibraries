;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          arrays.lisp
;;;; Purpose:       Random array utilities
;;;; Programmer:    Ian S Eslick
;;;; Date Started:  May 2005
;;;;
;;;; *************************************************************************

(in-package :utils)

(defun-exported fast-array-copy (a1 a2 start count)
  "Unsafe array copy"
  (declare #-mcl (type (simple-vector fixnum) a1 a2)
	   #-mcl (type fixnum start count)
	   (optimize speed (safety 0)))
  (loop for pos fixnum from start to (- count 1) do
       (setf (svref a2 pos) (svref a1 pos))))
