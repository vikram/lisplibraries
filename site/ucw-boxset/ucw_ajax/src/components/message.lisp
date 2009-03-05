;; -*- lisp -*-

(in-package :it.bese.ucw)

;;;; ** Message Dialog Component

(defclass info-message ()
  ((message :initarg :message :accessor message)
   (ok-text :initarg :ok-text :accessor ok-text :initform "Ok."))
  (:documentation "Component for showing a message to the user.

If the OK-TEXT slot is non-NIL component will use that as the
text for a link which, when clicked, causes the component to
answer. It follows that if OK-TEXT is NIL this component will
never answer.")
  (:metaclass standard-component-class))

(defmethod render ((self info-message))
  (flet ((body ()
           (<:p (<:as-html (message self)))
           (when (and (ok-text self)
                      (was-called-p self))
             ;; TODO this should be factored out from here
             (<:p (<ucw:a :action (if (typep (context.application *context*) 'dirty-component-tracking-application-mixin)
                                      (register-ajax-action ()
                                        (with-call/cc
                                          (answer-component self t)))
                                      (register-action (:with-call/cc t)
                                        (answer-component self t)))
                          (<:as-html (ok-text self)))))))
    (if (parent self)
        (body)
        (<:html
         (<:body
          (body))))))

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
