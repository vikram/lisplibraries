;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-syntax-sugar)

(shadowing-import
 (bind ((*package* (find-package :cl-walker)))
   (read-from-string "(result then body arguments name parent illegal-lambda-list)"))
 :cl-syntax-sugar)

(use-package :cl-walker :cl-syntax-sugar)
