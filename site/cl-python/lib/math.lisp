;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLPYTHON.MODULE.MATH -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :clpython.module.math)

(defgeneric conv (x)
  (:method ((x number)) x)
  (:method (x)          (clpython::py-float x)))
  
(defconstant-once |e| (cl:exp 1))
(defconstant-once |pi| cl:pi)
(set-impl-status '(|pi| |e|) t)

(defmacro def-unary-conv-func (math cl)
  `(defun ,math (x) (,cl (conv x))))

(def-unary-conv-func |sin| cl:sin)
(def-unary-conv-func |cos| cl:cos)
(def-unary-conv-func |tan| cl:tan)
(def-unary-conv-func |asin| cl:asin)
(def-unary-conv-func |acos| cl:acos)
(def-unary-conv-func |atan| cl:atan)
(def-unary-conv-func |sinh| cl:sinh)
(def-unary-conv-func |cosh| cl:cosh)
(def-unary-conv-func |tanh| cl:tanh)

(set-impl-status '(|sin| |cos| |tan| |asin| |acos| |atan| |sinh| |cosh| |tanh|) t)
(set-impl-status '|atan2| :todo "CL does not have it?")

(defun |ceil| (x) (ceiling (conv x)))
(set-impl-status '(|ceil| |floor|) t)

(defun |pow| (x y) (expt (conv x) (conv y)))
(defun |log10| (x) (log (conv x) 10))
(set-impl-status '(|pow| |exp| |log| |log10| |sqrt|) t)
                 
(set-impl-status '(|degrees| |radians|) :todo)

(defun |fmod| (x y) (mod (conv x) (conv y)))
(defun |fabs| (x) (abs (conv x)))
(defun |modf| (x) (make-tuple-from-list (multiple-value-list (truncate (conv x)))))
(set-impl-status '(|fmod| |fabs| |modf|) t)

(set-impl-status '|frexp| :todo)

(defun |hypot| (x y) (abs (complex (conv x) (conv y))))
(defun |ldexp| (x y) (* x (expt 2 (conv y))))
(set-impl-status '(|hypot| |ldexp|) t)
