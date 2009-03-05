;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2007 by the authors.
;;;
;;; See LICENCE for details.

(cl:in-package :cl-user)

(defpackage :cl-def

  (:use
   :common-lisp
   :metabang-bind
   :alexandria
   :iterate
   :cl-def.system
   :cl-syntax-sugar
   )

  (:export
   #:def
   #:definer
   #:find-definer
   #:with-standard-definer-options

   #:-whole-
   #:-environment-
   #:-definer-
   #:-options-
   #:-body-
   #:-self-))
