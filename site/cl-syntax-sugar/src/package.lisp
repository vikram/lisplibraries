;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(cl:in-package :cl-user)

(defpackage :cl-syntax-sugar
  (:use #:common-lisp :alexandria :metabang-bind)

  (:export
   #:cl-source-file-with-readtable
   #:system-with-readtable
   #:system-connection-with-readtable
   #:define-syntax
   #:*toplevel-readtable*
   #:*quasi-quote-depth*
   #:*quasi-quote-nesting-level*
   #:get-macro-character*
   #:with-local-readtable
   #:list-readers

   ;; export it early, we don't know in which order the integrations are loaded
   #:register-readtable-for-swank
   )
  (:shadow
   #:log))
