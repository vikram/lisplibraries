;; -*- lisp -*-

(in-package :it.bese.ucw)

;;;; ** Generic Query/Option dialog

(defclass option-dialog (template-component)
  ((message :accessor message :initarg :message)
   (options :accessor options :initarg :options)
   (confirm :accessor confirm :initarg :confirm :initform nil))
  (:default-initargs :template-name "option-dialog.tal")
  (:documentation "Component for querying the user.

The value of the slot MESSAGE is used as a general heading.

The OPTIONS slot must be an alist of (VALUE . LABEL). LABEL (a
string) will be used as the text of a link which, when clikced,
will answer VALUE.

If the CONFIRM slot is T the user will be presented with a second
OPTION-DIALOG asking the user if they are sure they want to
submit that value.")
  (:metaclass standard-component-class))

(defmethod template-component-environment nconc ((dialog option-dialog))
  (make-standard-environment
   `((options . ,(mapcar (lambda (value-cons)
                           (tal-env 'text (cdr value-cons)
                                    'value (car value-cons)))
                         (options dialog))))
   dialog))

(defaction respond ((dialog option-dialog) value)
  (if (confirm dialog)
      (if (call 'option-dialog
                :message (format nil "Are you sure you want to answer ~S to the question ~S?"
                                 (cdr (assoc value (options dialog)))
                                 (message dialog))
                :options '((t . "Yes")
                           (nil . "No")))
          (answer value)
          ;; repeat the question
          nil)
      (answer value)))

(defmacro option-dialog ((message-spec &rest message-args) &body options)
  `(call 'option-dialog :message ,(if message-args
				      `(format nil ,message-spec ,@message-args)
				      message-spec)
	 :options (list ,@options)))

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
