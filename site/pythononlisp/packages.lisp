;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(in-package :common-lisp-user)

(defpackage #:pythononlisp
  (:use #:cl 
	#:cffi)
  (:nicknames #:py)
  (:documentation "Python On Lisp allows you to call Python libraries from Common Lisp")) 

