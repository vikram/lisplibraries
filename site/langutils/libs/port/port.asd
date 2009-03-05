;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; ASD File for Think Utilities Module

(defpackage #:port.system
  (:use #:cl #:asdf))

(in-package #:port.system)

(defsystem #:port
    :description "The CLOCC PORT package for portable basic services"
    :version "1.0"
    :author "?"
    :licence "Public Domain"
    :components ((:file "ext")
		 (:file "shell" :depends-on ("ext"))
	         (:file "net" :depends-on ("ext" "sys" 
						 ))
		 (:file "path" :depends-on ("ext"))
	         (:file "proc" :depends-on ("ext" 
					    ))
                 (:file "sys" :depends-on ("ext" "path")))
    :in-order-to ((load-op (compile-op :port))))

