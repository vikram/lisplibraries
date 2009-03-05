;;;; -*- lisp -*-

(in-package :it.bese.ucw)

(defclass application-inspector (ucw-inspector)
  ((tal-to-inspect :accessor tal-to-inspect :initform nil :initarg :tal-to-inspect))
  (:metaclass standard-component-class))

;;;; TAL debugging

(defclass template-inspector ()
  ((template-name :initarg :template-name)
   (template-expansion :initarg :template-expansion))
  (:metaclass standard-component-class))

(defaction describe-tal ((insp application-inspector))
  (call 'template-inspector
        :template-name (tal-to-inspect insp)
        :template-expansion (preprocess-template (tal-to-inspect insp) nil (datum insp))))
  
(defmethod render ((c template-inspector))
  (<:html
   (<:body
    (<:p "Expansion of template " (<:as-html (slot-value c 'template-name )))
    (<:pre 
     (<:as-html
      (let ((*print-length* nil)
            (*print-level* nil)
            (*print-pretty* nil)
            (*print-miser-width* nil)
            (*package* (find-package :it.bese.yaclml.tags)))
        (with-output-to-string (exp)
          (pprint (slot-value c 'template-expansion) exp)))))
    (<ucw:a :action (answer-component c t) "Done."))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 2003-2005 Edward Marco Baringer
;;; All rights reserved. 
;;; 
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions are
;;; met:
;;; 
;;;  - Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 
;;;  - Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;; 
;;;  - Neither the name of Edward Marco Baringer, nor BESE, nor the names
;;;    of its contributors may be used to endorse or promote products
;;;    derived from this software without specific prior written permission.
;;; 
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
