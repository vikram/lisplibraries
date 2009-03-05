;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/tbnl/tbnl.asd,v 1.53 2006/10/09 23:46:40 edi Exp $

;;; Copyright (c) 2004-2006, Dr. Edmund Weitz.  All rights reserved.

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

(defpackage :tbnl-asd
  (:use :cl :asdf))

(in-package :tbnl-asd)

(defvar *tbnl-version* "0.11.4"
  "A string denoting the current version of TBNL.  Used for diagnostic
output.")

(export '*tbnl-version*)

(asdf:defsystem :tbnl
  :serial t
  :version #.*tbnl-version*
  :depends-on (:md5
               :cl-base64
               :cl-ppcre
               #-:lispworks :kmrcl
               :rfc2388
               :url-rewrite)
  :components ((:file "packages")
               (:file "specials")
               (:file "mime-types")
               (:file "util")
               (:file "log")
               (:file "cookie")
               (:file "reply")
               (:file "request")
               (:file "session")
               (:file "html")
               (:file "easy-handlers")
               (:file "modlisp")
               (:file "araneida")))
