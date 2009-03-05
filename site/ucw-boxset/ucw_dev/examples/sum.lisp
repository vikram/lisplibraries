(in-package :it.bese.ucw-user)

;;;; This is an example which demonstrates component re use and a
;;;; control flow which doesn't have a well defined 'starting'
;;;; component.

(defcomponent read-a-number (widget-component)
  ((label  :accessor label  :initarg :label :initform "A number please")))

(defmethod render ((reader read-a-number))
  (let ((number-string ""))
    (<ucw:form :action (ok reader number-string)
      (<:p (<:as-html (label reader)) (<ucw:input :type "text" :accessor number-string))
      (<:p (<:input :type "submit" :value "Ok.")))))

(defaction ok ((reader read-a-number) &optional number-string)
  (let ((num (parse-integer number-string :junk-allowed t)))
    (when num (answer num))))

(defcomponent sum (task-component)
  ())

(defaction start ((s sum))
  (loop
     for how-many = (call 'read-a-number :label "How many numbers should we read?")
     initially (backtrack (ucw::context.current-frame *context*) (make-place how-many))
     do (loop
           for count below how-many
           initially (backtrack (ucw::context.current-frame *context*)
                                (make-place count))
           sum (call 'read-a-number) into total
           finally (call 'info-message
                         :message (format nil "The sum is: ~D." total)))))

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
