;; -*- lisp -*-

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :rfc2388.system)
    (defpackage :rfc2388.system
      (:documentation "ASDF System package for rfc2388")
      (:use :common-lisp :asdf))))

(in-package :rfc2388.system)

(defsystem :rfc2388
  :components ((:static-file "rfc2388.asd")
               (:module :source
                :components ((:file "packages")
                             (:file "rfc2388" :depends-on ("packages"))))))

(defsystem :rfc2388.test
  :components ((:module :test
                :components ((:file "packages")
                             (:file "test" :depends-on ("packages")))))
  :depends-on (:rfc2388 :fiveam))


;;;; * Parsing rfc2888 formatted data

;;;; This library provides code for parsing multipart/form-data data
;;;; streams.

;;;; The main entry-point is the function READ-MIME. Due the various
;;;; application specific ways in which the content should be treated
;;;; our parser uses application supplied calbacks to deal with the
;;;; actual data.

;;;; The function PARSE-MIME is provided as a convenient wrapper
;;;; around READ-MIME which assumse that all data can fit in memory
;;;; and that it can be converted using nothing more that #'code-char.

;;;;@include "source/packages.lisp"

;;;;@include "source/rfc2388.lisp"

;;;; * Known Issues

;;;; ** Non US-ASCII field names

;;;; Currently we assume that the names of all form fields are
;;;; US-ASCII characters. Should a developer create a form whose name
;;;; is "&pi;" (greek small letter pi) it is
;;;; browser+server+implementation specific how this will be
;;;; translated by this code.

;; Copyright (c) 2003 Janis Dzerins
;; Copyright (c) 2005 Edward Marco Baringer
;; 
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;; IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
