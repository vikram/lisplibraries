;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2007 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-user)

;;; try to load asdf-system-connections
(eval-when (:compile-toplevel :load-toplevel :execute)
  (flet ((try (system)
           (unless (asdf:find-system system nil)
             (warn "Trying to install required dependency: ~S" system)
             (when (find-package :asdf-install)
               (funcall (read-from-string "asdf-install:install") system))
             (unless (asdf:find-system system nil)
               (error "The ~A system requires ~A." (or *compile-file-pathname* *load-pathname*) system)))
           (asdf:operate 'asdf:load-op system)))
    (try :cl-syntax-sugar)
    (try :asdf-system-connections)))

(defpackage #:cl-def.system
  (:use :common-lisp :asdf :cl-syntax-sugar))

(in-package #:cl-def.system)

(defsystem :cl-def
  :version "0.1"
  :author ("Attila Lendvai <attila.lendvai@gmail.com>"
	   "Levente Mészáros <levente.meszaros@gmail.com>")
  :licence "BSD / Public domain"
  :description "cl-def - (def function ioe name (arg1) ...)"
  :depends-on (:alexandria :iterate :metabang-bind :cl-syntax-sugar)
  :class system-with-readtable
  :default-component-class cl-source-file-with-readtable
  :setup-readtable-function "cl-def::setup-readtable"
  :serial t
  :components
  ((:file "package")
   (:file "configuration")
   (:file "duplicates")
   (:file "def")
   (:file "definers")))

(defsystem-connection cl-def-and-stefil
  :requires (:cl-def :stefil)
  :components ((:module :integration
                        :components ((:file "stefil")))))

(defsystem-connection cl-def-and-slime
  :requires (:cl-def :swank)
  :components ((:module :integration
                        :components ((:file "slime")))))

(defsystem-connection cl-def-and-contextl
  :requires (:cl-def :contextl)
  :components ((:module :integration
                        :components ((:file "contextl")))))

(defsystem-connection cl-def-and-cl-delico
  :requires (:cl-def :cl-delico)
  :components ((:module :integration
                        :components ((:file "cl-delico")))))

(defsystem-connection cl-def-and-cl-l10n
  :requires (:cl-def :cl-l10n)
  :components ((:module :integration
                        :components ((:file "cl-l10n")))))

(defsystem :cl-def-test
  :description "Tests for the cl-def test system."
  :depends-on (:cl-def :stefil)
  :default-component-class cl-source-file-with-readtable
  :class system-with-readtable
  :setup-readtable-function "cl-def::setup-readtable"
  :components
  ((:file "test")))

(defmethod perform ((op test-op) (system (eql (find-system :cl-def))))
  (operate 'load-op :cl-def-test)
  (in-package :cl-def-test)
  (declaim (optimize (debug 3)))
  (warn "(declaim (optimize (debug 3))) was issued to help later C-c C-c'ing")
  (eval (read-from-string "(stefil:funcall-test-with-feedback-message 'cl-def-test:test)"))
  (values))

(defmethod operation-done-p ((op test-op) (system (eql (find-system :cl-def))))
  nil)
