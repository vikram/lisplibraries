;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2007 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-def)

(def (definer :available-flags "e") function/cc ()
  (function-like-definer -definer- 'cl-delico:defun/cc -whole- -environment- -options-))

(integrated-export '(cl-delico:defun/cc function/cc) :cl-def)
