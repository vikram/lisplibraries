;; -*- lisp -*-

(in-package :it.bese.ucw)

;;;; ** STANDARD-APPLICATION

(defmethod (setf application.dispatchers) (dispatchers (app standard-application))
  (setf (slot-value app 'dispatchers)
        (sort (copy-list dispatchers)
              #'> :key #'priority)))

(defmethod debug-on-error ((app standard-application))
  (if (slot-boundp app 'debug-on-error)
      (slot-value app 'debug-on-error)
      (debug-on-error nil)))

(defmethod (setf javascript-debug-level) (value (app standard-application))
  (unless (equal (slot-value app 'javascript-debug-level)
                 value)
    (setf (slot-value app 'javascript-debug-level) value)
    ;; invalidate the cache of all parenscript-dispatchers. but even after this they need
    ;; a browser refresh to get recompiled while the date sent in the expires header is not over.
    (let ((parenscript-dispatcher-class (find-class 'parenscript-dispatcher)))
      (dolist (dispatcher (remove-if (lambda (el)
                                       (not (typep el parenscript-dispatcher-class)))
                                     (application.dispatchers app)))
        (setf (cached-javascript dispatcher) nil)))))

(defmethod request-context-class list ((app standard-application))
  'standard-request-context)

(defmethod shared-initialize :after ((app standard-application) slot-names &key session-type &allow-other-keys)
  (when session-type
    (error "The API has changed, instead of :session-type use the session-class method."))
  (setf (application.dispatchers app) (application.dispatchers app)) ; trigger the accessor to ensure it's sorted
  (setf (request-context-class-of app)
        (make-instance 'standard-class
                       :direct-superclasses (mapcar #'find-class (request-context-class app))
                       :name (intern-concat (list (class-name-of app) "-REQUEST-CONTEXT"))))
  (setf (session-class-of app)
        (make-instance 'standard-class
                       :direct-superclasses (mapcar #'find-class (session-class app))
                       :name (intern-concat (list (class-name-of app) "-SESSION")))))

(defmethod make-request-context ((app standard-application)
				 (request request)
				 (response response))
  (make-instance (request-context-class-of app)
                 :request request
                 :response response
                 :application app))

(defmethod find-session ((app standard-application) (context request-context))
  "Returns the session with ID (find-session-id CONTEXT) in APP,
NIL if there is no session with that id."
  (let ((session-id (find-session-id context)))
    (if session-id
        (gethash session-id (application.session-table app))
        nil)))

(defmethod session-class list ((app standard-application))
  'standard-session)

(defmethod make-new-session ((app standard-application))
  "Returns a new session object.

The slot SESSION-TYPE controls the class of session created."
  (ucw.rerl.application.dribble "Making a new session.")
  (let ((new-session (make-instance (session-class-of app))))
    (setf (session.id new-session)
          (insert-with-new-key (application.session-table app)
                               +session-id-length+
                               new-session))
    (ucw.rerl.application.dribble "New Session id ~S." (session.id new-session))
    new-session))

(defmethod remove-expired-sessions ((app application))
  "Loops over all the sessions in APP, calls EXPIRE-SESSION on
those for which EXPIREDP returns T. Then drops them from the
APP's session-table hash."
  (iterate 
    (for (session-id session) in-hashtable (application.session-table app))
    (when (expiredp session)
      (ucw.rerl.application.dribble "Removing expired session ~S." session)
      (expire-session session)
      (remhash session-id (application.session-table app)))))

(defmethod delete-session ((app standard-application) (session session) &optional (expire t))
  "Remove SESSION from the set of known sessions. When EXPIRE is
  true the expire-session method will be called on SESSION
  before removing it."
  (with-lock-held-on-session session
    (when expire
      (expire-session session))
    (remhash (session.id session) (application.session-table app)))
  session)

(defmethod ensure-session ((app standard-application)
			   (context standard-request-context)
			   &optional (session (or (find-session app context)
						  (make-new-session app))))
  "If CONTEXT's request specifies a session then put it in the
  context, otherwise create a new context.

Updates the session's last-access time."
  (unless (eq (context.session context) session)
    (setf (context.session context) session))
  (setf (session.last-access (context.session context)) (get-universal-time)))

(defmethod iterate-sessions-with-lock-held ((app standard-application) (visitor function))
  ;; one way to make it safe from deadlocks is to lock the app first and keep it
  ;; locked until all sessions are visited
  (prog2
      (ucw.rerl.ajax.dribble "Entering iterate-sessions-with-lock-held for app ~S in thread ~S" app (current-thread))
      (with-recursive-lock-held ((lock-of app))
        (iter (for (nil session) in-hashtable (application.session-table app))
              (with-recursive-lock-held ((lock-of session))
                (funcall visitor session))))
    (ucw.rerl.ajax.dribble "Leaving iterate-sessions-with-lock-held for app ~S in thread ~S" app (current-thread))))

(defmethod service ((app standard-application) (context request-context))
  "Service a request for this application.

The CONTEXT is inspected and session is either created (if an
existing one can not be found), or retrieved form the
application's session table. The session is created by
make-new-session and is passed to SERVICE. CONTEXT is updated to
contain the session object."
  ;; remove expired sessions
  (when (= 0 (random 50))
    (remove-expired-sessions app))
  (dolist (dispatcher (application.dispatchers app) nil)
    (when (dispatch dispatcher app context)
      (ucw.rerl.dispatcher.dribble "~S handled the request, returning t from app service" dispatcher)
      (return-from service t))))

(defprint-object (app application :identity nil)
  (format *standard-output* "~A ~S"
          (application.url-prefix app)
          (hash-table-count (application.session-table app))))

(defmethod handle-action-error-using-application ((app standard-application) error backtrace)
  (setf (frame.window-component (context.current-frame *context*))
        (make-instance 'error-component
                       :condition error
                       :message (princ-to-string error)
                       :backtrace backtrace)))

(defmethod startup-application ((app standard-application))
  "Simply clears out the app's session-table."
  ;; why is this done in startup-application and not
  ;; shutdown-application?
  (clrhash (application.session-table app)))

(defmethod shutdown-application ((app standard-application))
  (loop
     for session being the hash-values of (application.session-table app)
     do (delete-session app session t)))

(defmethod restart-application ((app standard-application))
  "Calls shutdown-application and then startup-application on
APP."
  (shutdown-application app)
  (startup-application app))

(defmethod compute-url ((app standard-application) &key action-id)
  "Creates the default url for APP which, when requested, will
  cause the action with id ACTION-ID to be called.

The generated URL's path will be the same as that of the current
request, as per (context.request *context*)). The parameters of
the created request or those 'internal' parameters ucw needs to
find the action, session-id, frame-id and action-id."
  (let ((query (list (cons +session-parameter-name+
                           (session.id (context.session *context*)))
                     (cons +frame-parameter-name+
                           (frame.id (context.current-frame *context*)))))
        (path (application.url-prefix app)))
    (when action-id
      (nconc query (list (cons +action-parameter-name+
                               action-id))))
    (make-instance 'uri :path path :query query)))

(defmethod application.dispatchers ((app standard-application))
  (slot-value app 'dispatchers))

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
