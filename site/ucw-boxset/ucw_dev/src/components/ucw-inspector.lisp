;; -*- lisp -*-

(in-package :it.bese.ucw)

;;;; ** UCW Inspector

(defclass ucw-inspector ()
  ((datum :accessor datum :initarg :datum :initform nil))
  (:documentation "Component for inspecting random lisp values.

Based on SLIME's inspector.")
  (:metaclass standard-component-class))

(defaction call-action ((inspector ucw-inspector) action)
  "Call an inspector action."
  (funcall action))

(defaction call-inspector ((component component) datum)
  "Call an inspector for DATUM on the component COMPONENT."
  (call 'ucw-inspector :datum datum))

(defun inspect-anchor (from datum &optional (string (write-to-string datum :circle t :pretty nil)))
  (<ucw:a :action (call-inspector from datum) (<:as-html string)))

(defun inspect-anchor-string (from datum)
  (with-output-to-string (*yaclml-stream*)
    (inspect-anchor from datum)))

(defmethod render ((insp ucw-inspector))
  (multiple-value-bind (title content)
      (swank::inspect-for-emacs (slot-value insp 'datum)
                                (swank::make-default-inspector))
    (<:h2 (<:as-html title))
    (dolist (part content)
      (etypecase part
        (null nil)
        (string (if (< (length part) 100)
                    (<:as-html part)
                    (<:pre (<:as-html part))))
        (cons (swank::destructure-case part
                ((:newline) (<:br))
                ((:value obj &optional (str (prin1-to-string obj)))
                 (<ucw:a :action (call-inspector insp obj)
                         (<:as-html str)))
                ((:action text lambda)
                 (<ucw:a :action (call-action insp lambda)
                         (<:as-html text)))))))
    (when (and (slot-boundp insp 'calling-component)
               (slot-value insp 'calling-component))
      (<:p (<ucw:a :action (ok insp) "Ok.")))))

;; Copyright (c) 2003-2005 Edward Marco Baringer
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
