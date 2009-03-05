;; -*- lisp -*-

(in-package :it.bese.ucw)

;;;; ** RERL Conditions

(define-condition rerl-error (error)
  ((context :initarg :context :accessor rerl-error.context :initform *context*))
  (:documentation "An error signalled during the request processing chain."))

;;;; Conditions relating to badly formed requests. Generally this means
;;;; that some essential piece of information was either missing or
;;;; unrecognizable.

(define-condition inexistent-request-part (rerl-error)
  ((query-path :initarg :query-path :accessor rerl-error.query-path
               :initform "#<unknown>"))
  (:documentation "Class of errors signaled when a particular
part of the action chain (session, frame, param-action or action)
is specified in the request but the corresponding server side
object can't be found."))

(defmethod shared-initialize :after ((err inexistent-request-part) slot-names &rest other-args)
  (declare (ignore slot-names other-args))
  (when (and (boundp '*context*) *context*)
    (setf (rerl-error.query-path err) (query-path (context.request *context*)))))

(define-condition inexistent-application-name (inexistent-request-part)
  ()
  (:documentation "Signalled when the server can't determine an
application for a particular request."))

(define-condition inexistent-session-id (inexistent-request-part)
  ((application :initarg :application)
   (session-id :initarg :session-id)))

(define-condition inexistent-frame-id (inexistent-request-part)
  ((session :initarg :session)
   (frame-id :initarg :frame-id)))

(define-condition inexistent-callback-id (inexistent-request-part)
  ((application :initarg :application)
   (session :initarg :session)
   (frame :initarg :frame)
   (callback-id :initarg :callback-id)))

(define-condition inexistent-action-id (inexistent-request-part)
  ((frame :initarg :frame)
   (action-id :initarg :action-id)))

(define-condition badly-formatted-request (rerl-error)
  ()
  (:documentation "Class of errors signaled when a particular
  id (application, session, frame or action) is not found in the
  request."))

(define-condition session-id-missing (badly-formatted-request)
  ())

(define-condition frame-id-missing (badly-formatted-request)
  ())

(define-condition action-id-missing (badly-formatted-request)
  ())

(define-condition callback-error (rerl-error)
  ()
  (:documentation "An error has occured while handling a callback."))

(define-condition action-error (rerl-error)
  ()
  (:documentation "An error has occured during the execution of an action."))

(define-condition render-error (rerl-error)
  ()
  (:documentation "An error has occured while rendering a component."))

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
