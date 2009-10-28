;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; babel-tests.asd --- ASDF system definition for Babel unit tests.
;;;
;;; Copyright (C) 2007, Luis Oliveira  <loliveira@common-lisp.net>
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

(defpackage #:babel-tests-system
  (:use #:cl #:asdf))
(in-package #:babel-tests-system)

(defvar *tests-dir* (append (pathname-directory *load-truename*) '("tests")))

(defsystem babel-tests
  :description "Unit tests for Babel."
  :depends-on (babel rt)
  :components
  ((:module "tests"
    :serial t
    :components
    ((:file "tests")))))

(defmethod perform ((o test-op) (c (eql (find-system :babel-tests))))
  (oos 'load-op :babel-tests)
  (let ((runner (find-symbol (string '#:run) '#:babel-tests)))
    (unless (and (funcall runner :compiled nil)
                 (funcall runner :compiled t))
      (error "test-op failed."))))

(defmethod operation-done-p ((o test-op) (c (eql (find-system :babel-tests))))
  nil)

;;; vim: ft=lisp et
