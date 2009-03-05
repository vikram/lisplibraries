;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(cl:in-package :cl-user)

(defpackage :cl-syntax-sugar-unicode
  (:use #:common-lisp :alexandria :metabang-bind :cl-syntax-sugar)

  (:export
   #:√
   #:∛
   #:∜

   #:λ
   #:α
   #:β
   #:γ
   #:δ
   #:ε
   #:Σ
   #:Π
   ))
