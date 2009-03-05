;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(in-package :common-lisp-user)

(defpackage #:pythononlisp.system
  (:use #:cl 
        #:asdf)) 

(in-package #:pythononlisp.system)


(defsystem #:pythononlisp
  :description "Jeremey Smith's Python-On-Lisp package, a bridge for using Python and Python libraries from Common Lisp"
  :version "0.2"
  :components ((:file "packages")
	       (:file "pythononlisp"))
  :depends-on (cffi))

;; PythonOnLisp has special loading requirements because of its use of
;; macros to define callback functions. The source must be loaded
;; before it can be compiled. Therefore, to load it, it is not enough
;; to do a standard (ASDF:OPERATE 'LOAD-OP :PYTHONONLISP), since this
;; calls (COMPILE-FILE ...) before (LOAD ...). The ASDF:LOAD-SOURCE-OP
;; operation simulates the effect of doing a (LOAD ...) command
;; instead of a (COMPILE-FILE ...) command.
;;
;; Gary King, the asdf maintainer, graciously sent in the package
;; below, which causes asdf to do the right thing with load-op instead
;; of the the thing it would normally do.
;;
;; The ideal solution would be to modify pythononlisp so it didn't
;; require a special loading procedure. But this is beyond the skill
;; of the current developers.
(defmethod component-depends-on
    ((operation load-op) (c (eql (find-system  '#:pythononlisp))))
  (list (list 'load-source-op (component-name c)))) 

