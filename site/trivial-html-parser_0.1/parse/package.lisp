;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CL-USER; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: Package definition for the SGML parser
;;;   Created: perhaps 1996
;;;    Author: Gilbert Baumann <gilbert@base-engineering.com>
;;;   License: MIT style (see below)
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 2005 by Gilbert Baumann

;;;  Permission is hereby granted, free of charge, to any person obtaining
;;;  a copy of this software and associated documentation files (the
;;;  "Software"), to deal in the Software without restriction, including
;;;  without limitation the rights to use, copy, modify, merge, publish,
;;;  distribute, sublicense, and/or sell copies of the Software, and to
;;;  permit persons to whom the Software is furnished to do so, subject to
;;;  the following conditions:
;;; 
;;;  The above copyright notice and this permission notice shall be
;;;  included in all copies or substantial portions of the Software.
;;; 
;;;  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. 
;;;  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;;  CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;;  TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;;  SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(in-package :CL-USER)

(defpackage :sgml
  (:use :cl :glisp :runes)
  (:export #:SGML-PARSE 
           #:PPT 
           #:SGML-UNPARSE 
           #:PARSE-DTD
           #:*OPTIONS/PARSER-SILENT-P*
           #:PT-NAME 
           #:PT-CHILDREN 
           #:PT-PARENT 
           #:PT-ATTRS 
           #:SLURP-CATALOG
           ;; in pt-utils:
           #:map-pt
           #:pt-cdata
           #:pt-attr
           #:pt-root
           #:pt-root-property
           #:gi
           #:flat-find-element
           #:flat-find-elements
           #:pt-full-name-path
           #:lhtml->pt
           ;;
           #:html-parse-file
           ))

