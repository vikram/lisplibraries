;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(cl:in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:operate 'asdf:load-op :cl-syntax-sugar))

(in-package #:cl-syntax-sugar-system)

;;; must be a seperate .asd file because it uses cl-syntax-sugar:cl-source-file-with-readtable
(use-package :cl-syntax-sugar '#:cl-syntax-sugar-system)

(defsystem :cl-syntax-sugar-test
  :description "Tests for the cl-syntax-sugar system."
  :depends-on (:cl-syntax-sugar
               :cl-syntax-sugar-unicode
               :cl-walker
               :stefil
               :swank
               )
  :class system-with-readtable
  :default-component-class cl-source-file-with-readtable
  :setup-readtable-function "cl-syntax-sugar-test::setup-readtable"
  :components
  ((:module "tests"
            :serial t
            :components ((:file "package")
                         (:file "test-environment")
                         (:file "readtime-wrapper")
                         (:file "quasi-quote")
                         (:file "string-quote")
                         (:file "feature-cond")
                         (:file "lambda")
                         (:file "sharp-l" :depends-on ("lambda"))))))
