;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          lml2.asd
;;;; Purpose:       ASDF definition file for Lisp Markup Language Version 2
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Aug 2002
;;;;
;;;; $Id$
;;;;
;;;; This file, part of LML2, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;;
;;;; LML2 users are granted the rights to distribute and use this software
;;;; as governed by the terms of the GNU General Public License v2
;;;; (http://www.gnu.org/licenses/gpl.html)
;;;; *************************************************************************

(in-package #:cl-user)
(defpackage #:lml2-system (:use #:asdf #:cl))
(in-package #:lml2-system)

(defsystem lml2
  :name "lml2"
  :author "Kevin M. Rosenberg <kevin@rosenberg.net>"
  :version "1.0"
  :maintainer "Kevin M. Rosenberg <kmr@debian.org>"
  :licence "GNU General Public License"
  :description "Lisp Markup Language"
  :long-description "LML2 provides creation of XHTML for Lisp programs."

  :depends-on (kmrcl)

  :components
  ((:file "package")
   (:file "data" :depends-on ("package"))
   (:file "htmlgen" :depends-on ("data"))
   (:file "utils" :depends-on ("package"))
   (:file "files" :depends-on ("utils" "htmlgen"))
   (:file "base" :depends-on ("files"))
   #+ignore (:file "read-macro" :depends-on ("base"))
   (:file "stdsite" :depends-on ("base"))
   (:file "downloads" :depends-on ("base"))
   (:file "apache-dir" :depends-on ("base"))
   ))

(defmethod perform ((o test-op) (c (eql (find-system 'lml2))))
  (operate 'load-op 'lml2-tests)
  (operate 'test-op 'lml2-tests))

(defmethod operation-done-p ((o test-op) (c (eql (find-system 'lml2-tests))))
  (values nil))
