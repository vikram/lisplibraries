;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          console.lisp
;;;; Purpose:       Console interactiion
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Dec 2002
;;;;
;;;; $Id: console.lisp 8573 2004-01-29 23:30:50Z kevin $
;;;;a
;;;; This file, part of KMRCL, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;; and by onShore Development, Inc.
;;;;
;;;; KMRCL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:kmrcl)

(defvar *console-msgs* t)

(defvar *console-msgs-types* nil)

(defun cmsg (template &rest args)
  "Format output to console"
  (when *console-msgs*
    (setq template (concatenate 'string "~&;; " template "~%"))
    (apply #'format t template args)))

(defun cmsg-c (condition template &rest args)
  "Push CONDITION keywords into *console-msgs-types* to print console msgs
   for that CONDITION.  TEMPLATE and ARGS function identically to
   (format t TEMPLATE ARGS) "
  (when (or (member :verbose *console-msgs-types*)
            (member condition *console-msgs-types*))
    (apply #'cmsg template args)))

(defun cmsg-add (condition)
  (pushnew condition *console-msgs-types*))

(defun cmsg-remove (condition)
  (setf *console-msgs-types* (remove condition *console-msgs-types*)))

(defun fixme (template &rest args)
  "Format output to console"
  (setq template (concatenate 'string "~&;; ** FIXME ** " template "~%"))
  (apply #'format t template args)
  (values))
