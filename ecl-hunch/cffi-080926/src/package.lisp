;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; package.lisp --- Package definition for CFFI.
;;;
;;; Copyright (C) 2005-2006, James Bielman  <jamesjb@jamesjb.com>
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

(defpackage #:cffi
  (:use #:common-lisp #:cffi-sys #:babel-encodings)
  (:import-from #:alexandria
                #:ensure-list #:featurep #:format-symbol #:if-let
                #:make-gensym-list #:once-only #:parse-body #:symbolicate
                #:when-let #:with-unique-names)
  (:export
   ;; Types.
   #:foreign-pointer

   ;; Primitive pointer operations.
   #:foreign-free
   #:foreign-alloc
   #:mem-aref
   #:mem-ref
   #:pointerp
   #:pointer-eq
   #:null-pointer
   #:null-pointer-p
   #:inc-pointer
   #:incf-pointer
   #:with-foreign-pointer
   #:make-pointer
   #:pointer-address

   ;; Shareable vectors.
   #:make-shareable-byte-vector
   #:with-pointer-to-vector-data

   ;; Foreign string operations.
   #:*default-foreign-encoding*
   #:foreign-string-alloc
   #:foreign-string-free
   #:foreign-string-to-lisp
   #:lisp-string-to-foreign
   #:with-foreign-string
   #:with-foreign-strings
   #:with-foreign-pointer-as-string

   ;; Foreign function operations.
   #:defcfun
   #:foreign-funcall
   #:foreign-funcall-pointer

   ;; Foreign library operations.
   #:*foreign-library-directories*
   #:*darwin-framework-directories*
   #:define-foreign-library
   #:load-foreign-library
   #:load-foreign-library-error
   #:use-foreign-library
   #:close-foreign-library

   ;; Callbacks.
   #:callback
   #:get-callback
   #:defcallback

   ;; Foreign type operations.
   #:defcstruct
   #:defcunion
   #:defctype
   #:defcenum
   #:defbitfield
   #:define-foreign-type
   #:define-parse-method
   #:define-c-struct-wrapper
   #:foreign-enum-keyword
   #:foreign-enum-keyword-list
   #:foreign-enum-value
   #:foreign-bitfield-symbol-list
   #:foreign-bitfield-symbols
   #:foreign-bitfield-value
   #:foreign-slot-pointer
   #:foreign-slot-value
   #:foreign-slot-offset
   #:foreign-slot-names
   #:foreign-type-alignment
   #:foreign-type-size
   #:with-foreign-object
   #:with-foreign-objects
   #:with-foreign-slots
   #:convert-to-foreign
   #:convert-from-foreign
   #:free-converted-object

   ;; Extensible foreign type operations.
   #:translate-to-foreign
   #:translate-from-foreign
   #:free-translated-object
   #:expand-to-foreign-dyn
   #:expand-to-foreign
   #:expand-from-foreign

   ;; Foreign globals.
   #:defcvar
   #:get-var-pointer
   #:foreign-symbol-pointer
   ))
