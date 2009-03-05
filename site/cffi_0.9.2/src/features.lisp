;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; features.lisp --- CFFI-specific features.
;;;
;;; Copyright (C) 2006, Luis Oliveira  <loliveira@common-lisp.net>
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
;;;

(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :cffi *features*))

(defpackage #:cffi-features
  (:export
   ;; Features related to the CFFI-SYS backend.
   ;; Why no-*? This reflects the hope that these symbols will
   ;; go away completely and all lisps support long-long's and
   ;; the foreign-funcall primitive.
   #:no-long-long
   #:no-foreign-funcall
   #:no-finalizers

   ;; Only SCL support long-double...
   ;;#:no-long-double
   
   ;; Features related to the operating system.
   ;; Currently only these are pushed to *features*, more should be added.
   #:darwin
   #:unix
   #:windows

   ;; Features related to the processor.
   ;; Currently only these are pushed to *features*, more should be added.
   #:ppc32
   #:x86
   #:x86-64
   ))
