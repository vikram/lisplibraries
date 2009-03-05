;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2007 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-def)

(defun definer-lookup-hook (form)
  (when (typep form 'definer-name)
    (awhen (find-definer form nil)
      (values it t))))

(awhen (find-symbol (symbol-name '#:*inspector-dwim-lookup-hooks*) :swank)
  (pushnew 'definer-lookup-hook (symbol-value it)))

(register-readtable-for-swank
 '("CL-DEF" "CL-DEF-TEST") 'setup-readtable)
