;; -*- lisp -*-

(in-package :it.bese.ucw)

;;;; ** UCW Extensions to tal

(defmacro with-attribute-value ((var attribute-name tag) &body body)
  (rebinding (attribute-name tag)
    (with-unique-names (value-string)
    `(let ((,value-string (getf (cdar ,tag) ,attribute-name)))
       (unless ,value-string
         (error "~S not found in ~S." ,attribute-name ',tag))
       (let ((,var (read-tal-expression-from-string ,value-string)))
         (remf (cdar ,tag) ,attribute-name)
         ,@body)))))

(def-tag-handler <ucw:select (tag)
  (let ((test '#'eql)
        (key '#'identity)
        (on-change nil))
    (awhen (getf (cdar tag) '<ucw::test)
      (remf (cdar tag) '<ucw::test)
      (setf test (read-tal-expression-from-string it)))
    (awhen (getf (cdar tag) '<ucw::key)
      (remf (cdar tag) '<ucw::key)
      (setf key (read-tal-expression-from-string it)))
    (awhen (getf (cdar tag) '<ucw::on-change)
       (remf (cdar tag) '<ucw::on-change)
       (setf on-change (read-tal-expression-from-string it)))
    (transform-lxml-form `((<ucw::%select :on-change ,on-change :test ,test :key ,key ,@(cdar tag)) ,@(cdr tag)))))

(def-tag-handler <ucw:option (tag)
  (with-attribute-value (value '<ucw::value tag)
    (transform-lxml-form `((<ucw::%option :value ,value ,@(cdar tag)) ,@(cdr tag)))))

(def-attribute-handler <ucw::action (tag)
  (with-attribute-value (action '<ucw::action tag)
    (transform-lxml-form
     (ecase (caar tag)
       ;; an an A tag
       ((<:a <ucw:a)
        ;; remove the href attribute, if it exists
        (remf (cdar tag) '<:href)
        `((<ucw::a :action ,action ,@(cdar tag)) ,@(cdr tag)))
       ;; on a submit, image or button
       ((<:input <ucw:input) `((<ucw:input :action ,action ,@(cdar tag)) ,@(cdr tag)))
       ;; on a form
       ((<:form <ucw:form) `((<ucw:form :action ,action ,@(cdar tag)) ,@(cdr tag)))))))

(def-attribute-handler <ucw::accessor (tag)
  (with-attribute-value (accessor '<ucw::accessor tag)
    (transform-lxml-form
     (ecase (caar tag)
       (<:input `((<ucw:input :accessor ,accessor
                              ,@(cdar tag))
                  ,@(cdr tag)))
       (<ucw:select `((<ucw:select :accessor ,accessor
                                   ,@(cdar tag))
                      ,@(cdr tag)))
       (<:select `((<:select :name (make-new-callback (lambda (v) (setf ,accessor v)))
                             ,@(cdar tag))
                   ,@(cdr tag)))
       (<:textarea `((<ucw:textarea :accessor ,accessor
                                    ,@(cdar tag))
                     ,@(cdr tag)))))))

(def-tag-handler <ucw:render-component (tag)
  (with-attribute-value (comp '<ucw::component tag)
    `(render ,comp)))

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
