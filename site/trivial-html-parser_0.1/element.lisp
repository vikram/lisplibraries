;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLOSURE-PROTOCOL; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: The Closure Element Protocol
;;;   Created: 2002-08-07 03:29
;;;    Author: Gilbert Baumann <gilbert@base-engineering.com>
;;;   License: LGPL (See file COPYING for details).
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 2002 by Gilbert Baumann

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
 
(in-package :CLOSURE-PROTOCOL)

(defgeneric element-p (object)
  )

(defgeneric element-parent (element)
  )

(defgeneric element-children (element)
  )

(defgeneric element-attribute (element attribute-name)
  )

(defgeneric element-gi (element)
  )

(defgeneric text-element-p (element)
  )

(defgeneric element-text (element)
  )

;;; Fall through predicate definitions

(defmethod element-p ((object t))
  (declare (ignorable object))
  nil)

(defmethod text-element-p ((object t))
  (declare (ignorable object))
  nil)