;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          kmrcl.asd
;;;; Purpose:       ASDF system definition for KMRCL package
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Apr 2000
;;;;
;;;; $Id: kmrcl.asd 11464 2007-01-08 03:04:52Z kevin $
;;;;
;;;; This file, part of KMRCL, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;;
;;;; KMRCL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:cl-user)
(defpackage #:kmrcl-system (:use #:asdf #:cl))
(in-package #:kmrcl-system)

#+(or allegro cmu clisp lispworks sbcl scl openmcl)
(pushnew :kmr-mop cl:*features*)

(defsystem kmrcl
    :name "kmrcl"
    :author "Kevin M. Rosenberg <kevin@rosenberg.net>"
    :maintainer "Kevin M. Rosenberg <kmr@debian.org>"
    :licence "LLGPL"
    :depends-on (#+sbcl sb-posix)
    :components
    ((:file "package")
     (:file "ifstar" :depends-on ("package"))
     (:file "byte-stream" :depends-on ("package"))
     (:file "macros" :depends-on ("package"))
     (:file "functions" :depends-on ("macros"))
     (:file "lists" :depends-on ("macros"))
     (:file "seqs" :depends-on ("macros"))
     (:file "impl" :depends-on ("macros"))
     (:file "io" :depends-on ("macros" "impl"))
     (:file "console" :depends-on ("macros"))
     (:file "strings" :depends-on ("macros" "seqs"))
     (:file "strmatch" :depends-on ("strings"))
     (:file "buff-input" :depends-on ("macros"))
     (:file "random" :depends-on ("macros"))
     (:file "symbols" :depends-on ("macros"))
     (:file "datetime" :depends-on ("macros"))
     (:file "math" :depends-on ("macros"))
     (:file "color" :depends-on ("macros"))
     #+kmr-mop (:file "mop" :depends-on ("macros"))
     ;; #+kmr-mop (:file "attrib-class" :depends-on ("seqs" "mop"))
     (:file "equal" :depends-on ("macros" #+kmr-mop "mop"))
     (:file "web-utils" :depends-on ("macros" "strings"))
     (:file "xml-utils" :depends-on ("macros"))
     (:file "sockets" :depends-on ("strings"))
     (:file "processes" :depends-on ("macros"))
     (:file "listener" :depends-on ("sockets" "processes" "console"))
     (:file "repl" :depends-on ("listener" "strings"))
     (:file "os" :depends-on ("macros" "impl"))
     (:file "signals" :depends-on ("package"))
     ))

(defmethod perform ((o test-op) (c (eql (find-system 'kmrcl))))
  (operate 'load-op 'kmrcl-tests)
  (operate 'test-op 'kmrcl-tests :force t))

