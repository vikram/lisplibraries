;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: utils -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          imports.lisp
;;;; Purpose:       Import and re-export cllib and kmrcl utilities
;;;; Programmer:    Ian S. Eslick
;;;; Date Started:  August 2004
;;;;

(in-package :utils)

(eval-when (compile load eval)
  (use-package-noshadow-exported :cllib :utils)
  (use-package-noshadow-exported :kmrcl :utils))
