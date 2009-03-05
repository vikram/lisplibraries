;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; package: kmrcl -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          strings.lisp
;;;; Purpose:       Strings utility functions for KMRCL package
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Apr 2000
;;;;
;;;; $Id: strmatch.lisp 8573 2004-01-29 23:30:50Z kevin $
;;;;
;;;; This file, part of KMRCL, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;;
;;;; KMRCL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:kmrcl)


(defun score-multiword-match (s1 s2)
  "Score a match between two strings with s1 being reference string.
S1 can be a string or a list or strings/conses"
  (let* ((word-list-1 (if (stringp s1)
			  (split-alphanumeric-string s1)
			s1))
	 (word-list-2 (split-alphanumeric-string s2))
	 (n1 (length word-list-1))
	 (n2 (length word-list-2))
	 (unmatched n1)
	 (score 0))
    (declare (fixnum n1 n2 score unmatched))
    (decf score (* 4 (abs (- n1 n2))))
    (dotimes (iword n1)
      (declare (fixnum iword))
      (let ((w1 (nth iword word-list-1))
	    pos)
	(cond
	 ((consp w1)
	  (let ((first t))
	    (dotimes (i-alt (length w1))
	      (setq pos
		(position (nth i-alt w1) word-list-2
			  :test #'string-equal))
	      (when pos
		(incf score (- 30
			       (if first 0 5)
			       (abs (- iword pos))))
		(decf unmatched)
		(return))
	      (setq first nil))))
	 ((stringp w1)
	  (kmrcl:awhen (position w1 word-list-2
			       :test #'string-equal)
		       (incf score (- 30 (abs (- kmrcl::it iword))))
		       (decf unmatched))))))
    (decf score (* 4 unmatched))
    score))


(defun multiword-match (s1 s2)
  "Matches two multiword strings, ignores case, word position, punctuation"
  (let* ((word-list-1 (split-alphanumeric-string s1))
	 (word-list-2 (split-alphanumeric-string s2))
	 (n1 (length word-list-1))
	 (n2 (length word-list-2)))
    (when (= n1 n2)
      ;; remove each word from word-list-2 as walk word-list-1
      (dolist (w word-list-1)
	(let ((p (position w word-list-2 :test #'string-equal)))
	  (unless p
	    (return-from multiword-match nil))
	  (setf (nth p word-list-2) "")))
      t)))


	       
  
  
