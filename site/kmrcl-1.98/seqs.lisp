;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          seqs.lisp
;;;; Purpose:       Sequence functions for KMRCL package
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Apr 2000
;;;;
;;;; $Id$
;;;;
;;;; This file, part of KMRCL, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;;
;;;; KMRCL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package :kmrcl)


(defun nsubseq (sequence start &optional end)
  "Return a subsequence by pointing to location in original sequence"
  (unless end (setq end (length sequence)))
  (make-array (- end start)
              :element-type (array-element-type sequence)
              :displaced-to sequence
              :displaced-index-offset start))
