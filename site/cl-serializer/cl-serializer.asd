;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy 
;;; of this software and associated documentation files (the "Software"), to deal 
;;; in the Software without restriction, including without limitation the rights 
;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell 
;;; copies of the Software, and to permit persons to whom the Software is furnished 
;;; to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in 
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE 
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS 
;;; IN THE SOFTWARE.

(in-package :cl-user)

(defpackage :cl-serializer-system
  (:use :cl :asdf :cl-user)

  (:export #:*load-with-debug-p*))

(in-package :cl-serializer-system)

(defvar *load-with-debug-p* nil)

(defclass local-cl-source-file (cl-source-file)
  ())

(defmethod perform :around ((op operation) (component local-cl-source-file))
  (let ((*readtable* *readtable*)
        (setup-readtable-function
         (ignore-errors
           (fdefinition (read-from-string "cl-serializer::setup-readtable")))))
    (when setup-readtable-function
      (funcall setup-readtable-function))
    (call-next-method)))

(defsystem :cl-serializer
  :version "0.1"
  :description "Object serializer and deserializer"
  :default-component-class local-cl-source-file
  :depends-on (:cl-def
               :metabang-bind
               :alexandria
               :babel
               :closer-mop
               )
  :serial t
  :components
  ((:file "package")
   (:file "duplicates")
   (:file "serializer")))

(defmethod perform ((op test-op) (system (eql (find-system :cl-serializer))))
  (operate 'load-op :cl-serializer-test)
  (in-package :cl-serializer-test)
  (eval (read-from-string "(stefil:funcall-test-with-feedback-message 'test)")))

(defmethod operation-done-p ((op test-op) (system (eql (find-system :cl-serializer))))
  nil)
