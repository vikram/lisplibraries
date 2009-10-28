;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/hunchentoot/hunchentoot.asd,v 1.62 2008/05/21 14:38:47 edi Exp $

;;; Copyright (c) 2004-2008, Dr. Edmund Weitz.  All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :cl-user)

(defpackage :hunchentoot-asd
  (:use :cl :asdf))

(in-package :hunchentoot-asd)

(defvar *hunchentoot-version* "0.15.7"
  "A string denoting the current version of Hunchentoot.  Used
for diagnostic output.")

(export '*hunchentoot-version*)

(asdf:defsystem :hunchentoot
  :serial t
  :version #.*hunchentoot-version*
  :depends-on (:chunga
               :cl-base64
               :cl-fad
               :cl-ppcre
               #-(or :lispworks :hunchentoot-no-ssl) :cl+ssl
               :md5
               :rfc2388
               #+:sbcl :sb-bsd-sockets
               #+:sbcl :sb-posix
               :url-rewrite)
  :components ((:file "packages")
               (:file "conditions")
               #+:allegro (:file "port-acl")
               #+:clisp (:file "port-clisp")
               #+:cmu (:file "port-cmu")
               #+:lispworks (:file "port-lw")
               #+:openmcl (:file "port-mcl")
               #+:sbcl (:file "port-sbcl")
               #+:ecl (:file "port-ecl")
               (:file "specials")
               (:file "mime-types")
               (:file "util")
               (:file "log")
               (:file "cookie")
               (:file "reply")
               (:file "request")
               (:file "session")
               (:file "misc")
               (:file "easy-handlers")
               (:file "headers")
               #+(and :allegro :unix) (:file "unix-acl")
               #+(and :clisp :unix) (:file "unix-clisp")               
               #+(and :cmu :unix) (:file "unix-cmu")
               #+(and :lispworks :unix) (:file "unix-lw")
               #+(and :openmcl :unix) (:file "unix-mcl")
               #+(and :sbcl :unix (not :win32)) (:file "unix-sbcl")
               (:file "server")))
