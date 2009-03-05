;;; -*- Lisp -*-
(in-package :cl)

(asdf:defsystem #:cl-zlib
  :depends-on (:uffi :fiveam)
  :components ((:file "packages") (:file "cl-zlib"))
  :serial t)
