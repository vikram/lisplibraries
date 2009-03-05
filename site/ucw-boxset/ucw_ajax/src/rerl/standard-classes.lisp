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
   (session-class :accessor session-class-of
                  :documentation "Caches the class of the effective session.")
   (request-context-class :accessor request-context-class-of
                          :documentation "Caches the class of the effective request context.")
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
   (dispatchers :initform nil
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
   (lock :accessor lock-of
         :initform (make-recursive-lock "Application lock"))
   (debug-on-error :initarg :debug-on-error)
   (javascript-debug-level :accessor javascript-debug-level
                           :initform #+debug "debug" #-debug nil
                           :initarg :javascript-debug-level
                           :documentation "When nil, turn off js debug output. Otherwise use it as a dojo logging level."))
  (:default-initargs :dispatchers (make-standard-ucw-dispatchers)
                     :www-roots (make-standard-ucw-www-root-list))
  (:documentation "The default UCW application class."))

;; TODO drop eventually
(defun make-standard-ucw-www-roots ()
  (ucw-logger.error "Has been renamed to make-standard-ucw-www-root-list, will be removed")
  (make-standard-ucw-www-root-list))

(defun make-standard-ucw-www-root-list ()
  (let ((ucw-home (asdf:component-pathname (asdf:find-system :ucw))))
    (list (cons "ucw/" (merge-pathnames "wwwroot/ucw/" ucw-home))
          (cons "dojo/" (merge-pathnames "wwwroot/dojo/" ucw-home)))))

(defun make-standard-ucw-tal-dir-list ()
  (let ((ucw-home (asdf:component-pathname (asdf:find-system :ucw))))
    (list (merge-pathnames "wwwroot/ucw/tal/" ucw-home))))

(defprint-object (app standard-application)
  (write (application.url-prefix app)))

(defclass standard-request-context (request-context)
  ((request     :accessor context.request     :initarg :request     :initform nil)
   (response    :accessor context.response    :initarg :response    :initform nil)
   (application :accessor context.application :initarg :application :initform nil)
   (session     :accessor context.session     :initarg :session     :initform nil)
   (action      :accessor context.action      :initarg :action)))

(defclass standard-server (server)
  ((applications :accessor server.applications
                 :initform nil)
   (lock :accessor lock-of :initform (make-lock "Server lock"))
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

(defprint-object (frame standard-session-frame)
  (format *standard-output* "~A ~D/~D/~D"
          (frame.id frame)
          (hash-table-count (frame.actions frame))
          (hash-table-count (frame.callbacks frame))
          (length (frame.backtracks frame))))

(defclass expired-session-frame (standard-session-frame)
  ())

(defclass place ()
  ((getter :accessor place.getter :initarg :getter)
   (setter :accessor place.setter :initarg :setter)
   (copyer :accessor place.copyer :initarg :copyer)
   (form   :accessor place.form   :initarg :form))
  (:documentation "A \"pointer\" or \"locative\", an object
encapsulating a settable and readable place"))

(defprint-object (p place)
  (write (place.form p) :readably nil :circle t))

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
                 :initform (make-hash-table :test 'eql))
   (lock :accessor lock-of :initform (make-recursive-lock "Session lock"))
   (event-condition-variable :accessor event-condition-variable-of
                             :initform (when *supports-threads-p*
                                         (make-condition-variable)))
   (latest-polling-thread :accessor latest-polling-thread-of :initform nil)))

(defprint-object (s standard-session)
  (format *standard-output* "~D ~S" (queue-count (session.frames s)) (session.current-frame s)))

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
