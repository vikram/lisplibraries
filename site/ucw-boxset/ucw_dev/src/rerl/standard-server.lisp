;; -*- lisp -*-

(in-package :it.bese.ucw)

;;;; ** STANDARD-SERVER

(defmethod print-object ((server standard-server) stream)
  (print-unreadable-object (server stream :type t :identity t)
    (format stream "~S ~S" (type-of (server.backend server)) (length (server.applications server)))))

(defmethod startup-server ((server standard-server))
  "Startup SERVER. This calls startup-backend on the server's
backend and startup-application on all the application objcets
registered with SERVER."
  (ucw.rerl.server.info "Starting up standard server ~S." server)
  (setf *random-state* (make-random-state t))
  (initialize-backend (server.backend server) :server server)
  (startup-backend (server.backend server))
  (setf (server.started server) t)
  (with-lock-held (server.applications-lock server)
    (dolist (app (server.applications server))
      (startup-application app))))

(defmethod shutdown-server ((server standard-server))
  "First call SHUTDOWN-APPLICATION on all the apps registered
with SERVER, then call SHUTDOWN-BACKEND on SERVER's backend."
  (with-lock-held (server.applications-lock server)
    (dolist (app (server.applications server))
      (shutdown-application app)))
  (shutdown-backend (server.backend server))
  (setf (server.started server) nil))

(defmethod restart-server ((server standard-server))
  (shutdown-server server)
  (startup-server server))

(defmethod register-application ((server standard-server) (app application))
  (with-lock-held (server.applications-lock server)
    (setf (server.applications server)
          (delete app (server.applications server) :test #'eq))
    (setf (server.applications server)
          (sort (cons app (server.applications server)) #'>
                :key (lambda (app) (length (application.url-prefix app)))))
    (setf (application.server app) server)
    (dolist (path (application.www-roots app))
      (if (consp path)
          (publish-directory (server.backend server) 
                             (cdr path)
                             (concatenate 'string 
                                          (application.url-prefix app)
                                          (car path)))
          (publish-directory (server.backend server) 
                             path
                             (application.url-prefix app))))))

(defmethod unregister-application ((server standard-server) (app application))
  (with-lock-held (server.applications-lock server)
    (setf (server.applications server) (delete app (server.applications server) :test #'eq))))

(defmethod associated-application ((server standard-server) (request request))
  (find (query-path request)
        (server.applications server)
        :test #'starts-with :key #'application.url-prefix))

(defmethod handle-request ((server standard-server)
                           (request request)
                           (response response))
  "Service REQUEST and create a response in RESPONSE.

This method creates the catch tag abort-request and a restart for
sending clients error messaging."
  (macrolet ((with-server-restarts (&body body)
               `(restart-case
                    (let ((swank::*sldb-quit-restart* 'fail-miserably))
                      ,@body)
                  (fail-miserably ()
                    :report "Pretend this request never happend and fail."
                    (ucw.rerl.server.info "Calling fail-miserably restart.")
                    nil)
                  (try-again ()
                    :report "Play this request over from the top."
                    (handle-request server request response))))
             (with-handle-request-error (&body body)
               `(handler-bind ((error (lambda (condition)
                                        (ucw.rerl.server.error "Got request error: ~A." condition)
                                        (setf *current-condition* condition)
                                        (when (and (debug-on-error (context.application *context*))
                                                   (swank::default-connection))
                                          (ucw.rerl.server.info "Debugging error: ~A." condition)
                                          (setf *current-backtrace* (collect-backtrace condition))
                                          (swank:swank-debugger-hook condition nil))
                                        (ucw.rerl.server.info "Aborting request.")
                                        (handle-request-error *current-condition* *current-backtrace*)
                                        (setf handled t)
                                        (throw 'abort-request t))))
                  ,@body))
             (with-action-restarts (&body body)
               `(restart-case
                    (progn ,@body)
                  (server-error ()
                    :report "Send the client an internal server error page."
                    (ucw.rerl.server.info "Calling server-error restart.")
                    (handle-request-error (make-instance 'rerl-error) '())
                    (ucw.rerl.server.info "Aborting request.")
                    (throw 'abort-request nil))
                  (generate-backtrace-for-emacs ()
                    :report "Generate a bug report in Emacs."
                    (ucw.rerl.server.info "Calling generate-backtrace-for-emacs restart.")
                    (send-backtrace-to-emacs server *current-condition* *current-backtrace*)
                    (handle-request-error (make-instance 'rerl-error) '())
                    (ucw.rerl.server.info "Aborting request.")
                    (setf handled t)
                    (throw 'abort-request t)))))
    (let ((handled nil))
      (with-server-restarts
          ;; we give *context* a default value here in case, for whatever
          ;; reason, we're not able to create the "real" *context* below.
          (let ((*context* (make-instance 'standard-request-context
                                          :request request
                                          :response response)))
            (when
                (catch 'abort-request
                  (with-handle-request-error
                      (with-action-restarts
                          ;; find the application which may handle this reuest
                          (when-bind app (associated-application server request)
                            (setf *context* (make-request-context app request response))
                            (ucw.rerl.server.dribble "Application ~S with handle context ~S." app *context*)
                            (setf (get-header response "Status") "200"
                                  (get-header response "Content-Type") "text/html"
                                  (get-header response "Date") (date:universal-time-to-http-date (get-universal-time)))
                            ;; bind the yaclml-stream here so that any
                            ;; error handling code (even in the
                            ;; application's service method) can use
                            ;; yaclml tags.
                            (with-yaclml-stream (html-stream response)
                              (setf handled (service app *context*))))))
                  nil)
              (flush-request-response *context*)))
        handled))))

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
