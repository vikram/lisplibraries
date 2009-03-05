;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: GLISP; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: ACL-4.3 dependent stuff + fixups
;;;   Created: 1999-05-25 22:33
;;;    Author: Gilbert Baumann <gilbert@base-engineering.com>
;;;   License: MIT style (see below)
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 1998,1999 by Gilbert Baumann

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

(defun glisp::read-byte-sequence (&rest ap)
  (apply #'read-sequence ap))

(defun glisp::read-char-sequence (&rest ap)
  (apply #'read-sequence ap))

(defmacro glisp::with-timeout ((&rest options) &body body)
  `(mp:with-timeout ,options . ,body))

(defun glisp::g/make-string (length &rest options)
  (apply #'make-array length :element-type 'base-char options))

(defun glisp:run-unix-shell-command (cmd)
  (excl:shell cmd))

(defun glisp::getenv (string)
  (sys:getenv string))
