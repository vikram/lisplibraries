;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          utils.lisp
;;;; Purpose:       General purpose utilities
;;;; Author:        Kevin M. Rosenberg
;;;; Date Started:  June 2002
;;;;
;;;; $Id$
;;;;
;;;; This file, part of LML2, is copyrighted and open-source software.
;;;; Rights of modification and redistribution are in the LICENSE file.
;;;;
;;;; *************************************************************************

(in-package #:lml2)


(defun lml-quit (&optional (code 0))
  "Function to exit the Lisp implementation."
  (kmrcl:quit code))

(defun lml-cwd ()
  "Returns the current working directory."
  (kmrcl:cwd))

(defmacro fformat (stream control-string &rest args)
  (if stream
      `(funcall (formatter ,control-string) ,stream ,@args)
      `(format nil ,control-string ,@args)))

