;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          kmrcl-tests.asd
;;;; Purpose:       ASDF system definitionf for kmrcl testing package
;;;; Author:        Kevin M. Rosenberg
;;;; Date Started:  Apr 2003
;;;;
;;;; $Id$
;;;; *************************************************************************

(defpackage #:kmrcl-tests-system
  (:use #:asdf #:cl))
(in-package #:kmrcl-tests-system)

(defsystem kmrcl-tests
    :depends-on (:rt :kmrcl)
    :components
    ((:file "tests")))

(defmethod perform ((o test-op) (c (eql (find-system 'kmrcl-tests))))
  (or (funcall (intern (symbol-name '#:do-tests)
		       (find-package '#:regression-test)))
      (error "test-op failed")))

