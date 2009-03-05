;; -*- lisp -*-

(in-package :it.bese.yaclml)

;;;; * The [ reader

;;;; A read macro for easily embedding text and YACLML markup in lisp
;;;; code.

(defun |[ reader| (stream char)
  "Read a form using YACLML's [ syntax.

Everything between the #\[ and the #\] is read as text. ~FORM
prints (using <:as-html) the value returned by FORM while $FORM
simply evaluates FORM and ignore the result."
  (declare (ignore char))
  (with-collector (forms)
    (loop
       for char = (read-char stream t nil t)
       do (case char
            (#\\ (forms (read-char stream t nil t)))
            (#\] (return-from |[ reader| `(yaclml-quote ,@(forms))))
            (#\$ (forms (read stream t nil t)))
            (#\~ (forms `(<:as-html ,(read stream t nil t))))
            (t (forms char))))))

(defvar *readers* '())

(defun enable-yaclml-syntax ()
  (set-macro-character #\[ #'|[ reader| nil)
  (set-syntax-from-char #\] #\)))

(defun disable-yaclml-syntax ()
  (setf *readtable* (copy-readtable nil nil)))

;; Copyright (c) 2002-2005, Edward Marco Baringer
;; All rights reserved. 
;; 
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;; 
;;  - Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 
;;  - Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;;  - Neither the name of Edward Marco Baringer, nor BESE, nor the names
;;    of its contributors may be used to endorse or promote products
;;    derived from this software without specific prior written permission.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
