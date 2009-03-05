;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          lml2-tests.asd
;;;; Purpose:       ASDF system definitionf for lml2 testing package
;;;; Author:        Kevin M. Rosenberg
;;;; Date Started:  Apr 2003
;;;;
;;;; $Id$
;;;; *************************************************************************

(defpackage #:lml2-tests-system
  (:use #:asdf #:cl))
(in-package #:lml2-tests-system)

(defsystem lml2-tests
    :depends-on (:rt :lml2)
    :in-order-to ((test-op (load-op lml2-tests)))
    :components ((:file "tests")))

(defmethod perform ((o test-op) (c (eql (find-system 'lml2-tests))))
  (or (funcall (intern (symbol-name '#:do-tests)
		       (find-package '#:regression-test)))
      (error "test-op failed")))

