;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: utils -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          math.lisp
;;;; Purpose:       Math related utility functions
;;;; Programmer:    Ian S. Eslick
;;;; Date Started:  August 2004

(in-package :utils)

;; -------------------------
;; Simple math utilities

(eval-when (compile eval load)
  (proclaim '(optimize speed)))

(defun-exported limit-max (limit value)
  (if (> value limit) limit value))

(defun-exported limit-min (limit value)
  (if (< value limit) limit value))
