;;;; -*- mode: Lisp -*-

(cl:defpackage #:fsvd.system
  (:use #:cl #:asdf))

(in-package #:fsvd.system)

(defsystem #:fsvd
  :name "FSVD"
  :description "Simon Funk's quasi SVD"
  :long-description "This is a Common Lisp implementation of Simon
Funk's quasi svd as described at http://sifter.org/~simon/journal/20061211.html"
  :author "Gabor Melis"
  :version "0.0.3"
  :licence "MIT"
  :components ((:file "package")
               (:file "fsvd"))
  :serial t)
