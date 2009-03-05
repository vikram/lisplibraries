;; This software is Copyright (c) 2006, Steffen Siering.
;; <COMPANY> grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :cl-user)

;;;; * Package description

(defpackage bdb-playground
  (:use :cl :cl-store :bdb)
  (:shadow #:db-put #:db-get
	   #:db-cursor-get #:db-cursor-put
	   #:db-del
	   #:db-associate))

(defpackage bdb-ext-playground
  (:use :cl :cl-store :bdb))