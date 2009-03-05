;;;; -*- lisp -*-

(in-package :it.bese.ucw)

(defclass standard-application (application)
  ((url-prefix :accessor application.url-prefix
               :initarg :url-prefix
               :initform ""
               :documentation "A string specifying the
start (prefix) of all the urls this app should handle.

This value is used by the standard-server to decide what app a
particular request is aimed at and for generating links to
actions within the app. ")
   (session-type :accessor application.session-type
                 :initarg :session-type
                 :initform 'standard-session
                 :documentation "The name of the class, passed
directly to MAKE-INSTANCE) of session objects.")
   (tal-generator :accessor application.tal-generator
                  :initarg :tal-generator
                  :documentation "A tal-generator object used to
lookup and compile tal pages for template-components.")
   (www-roots :accessor application.www-roots
	      :initarg :www-roots
	      :initform nil
              :documentation "A list of directories (pathname
specifiers) or cons-cell (URL-subdir . pathname) to use when looking for static files.")
   (charset :accessor application.charset
            :initarg :charset
            :initform #-:sb-unicode nil #+:sb-unicode :utf-8
            :documentation "Default charset for sent text/html documents.")
   (dispatchers :initform (standard-dispatchers)
		:initarg :dispatchers
                :documentation "A list of request
dispatchers. The user supplied list of dispatchers is extended
with other dispatchers that are required for UCW to function
properly (action-dispatcher, a parenscript-dispatcher, etc). If
you want full control over the active dispatchers use the (setf
application.dispatchers) accessor or, if you want control over
the order of the dispathcers, (slot-value instance
'dispatchers).")
   (session-table :accessor application.session-table
                  :initform (make-hash-table :test 'equal))
   (server :accessor application.server
	   :initform *default-server*
	   :initarg :server)
   (debug-on-error :accessor debug-on-error
                   :initarg :debug-on-error)
   (dispatcher-cache :accessor dispatcher-cache
                     :initform (make-hash-table :test 'equal)
                     :documentation "A hash table mapping
                      query-paths to dispatchers."))
  (:documentation "The default UCW application class.")) 

(defmethod print-object ((app standard-application) stream)
  (print-unreadable-object (app stream :type t :identity t)
    (format stream "~S" (application.url-prefix app))))

(defclass standard-request-context (request-context)
  ((request     :accessor context.request     :initarg :request     :initform nil)
   (response    :accessor context.response    :initarg :response    :initform nil)
   (application :accessor context.application :initarg :application :initform nil)
   (session     :accessor context.session     :initarg :session     :initform nil)
   (action      :accessor context.action      :initarg :action)))

(defclass standard-server (server)
  ((applications :accessor server.applications
                 :initform nil)
   (applications-lock :accessor server.applications-lock
                      :initform (swank::make-lock :name "Application list lock."))
   (started :accessor server.started :initform nil :initarg :started)
   (backend :accessor server.backend
            :initform nil
            :initarg :backend)))

(defclass standard-session-frame (session-frame)
  ((actions :accessor frame.actions :initform (make-hash-table :test 'equal)
            :documentation "A hash table mapping action ids to 0 argument functions.")
   (callbacks :accessor frame.callbacks :initform (make-hash-table :test 'equal)
              :documentation "A hash table mapping callback ids to 1 argument functions.")
   (window-component :accessor frame.window-component :initarg :window-component :initform nil
                     :documentation "The root component for this
frame. The standard-server calls render on this component when
the frame is ready to be presented to the user.")
   (id :initarg :id :accessor frame.id :initform nil)
   (backtracks :initarg :backtracks :accessor frame.backtracks :initform '())))

(defmethod print-object ((frame standard-session-frame) stream)
  (print-unreadable-object (frame stream :type t :identity t)
    (format stream "~A ~D/~D/~D"
            (frame.id frame)
            (hash-table-count (frame.actions frame))
            (hash-table-count (frame.callbacks frame))
            (length (frame.backtracks frame)))))

(defclass expired-session-frame (standard-session-frame)
  ())

(defclass place ()
  ((getter :accessor place.getter :initarg :getter)
   (setter :accessor place.setter :initarg :setter)
   (copyer :accessor place.copyer :initarg :copyer)
   (form   :accessor place.form   :initarg :form))
  (:documentation "A \"pointer\" or \"locative\", an object
encapsulating a settable and readable place"))

(defmethod print-object ((p place) stream)
  (print-unreadable-object (p stream :type t :identity t)
    (write (place.form p) :stream stream :readably nil :circle t)))

(defclass standard-session (session)
  ((frames :initform (make-instance 'frame-queue :size +session-backtracking-max-depth+)
           :accessor session.frames
           :documentation "The table of session-frame objects
generated in this session.")
   (current-frame :initform nil
                  :accessor session.current-frame)
   (id :initarg :id :initform nil :accessor session.id)
   (last-access :initarg :last-access
                :accessor session.last-access
                :initform (get-universal-time))
   (object-table :initarg :object-pool
                 :accessor session.object-pool
                 :initform (make-hash-table :test 'eql))))

(defmethod print-object ((s standard-session) stream)
  (print-unreadable-object (s stream :type t :identity t)
    (format stream "~D ~S" (queue-count (session.frames s)) (session.current-frame s))))

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
