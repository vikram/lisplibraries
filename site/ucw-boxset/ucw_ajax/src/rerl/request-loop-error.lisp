;; -*- lisp -*-

(in-package :it.bese.ucw)

;;;; ** Error handling

;;;; *** Our simple wrapper around slime's backtrace lists.

(defstruct backtrace-frame
  index
  description
  locals
  source-location)

(defun collect-backtrace (condition)
  (let ((swank::*swank-debugger-condition* condition)
        (swank::*buffer-package* *package*))
    (swank::call-with-debugging-environment 
     (lambda () 
       (loop
          for (index desc) in (swank:backtrace 4 500)
          collect (make-backtrace-frame :index index
                                        :description desc
                                        :source-location (if (numberp index)
                                                             (swank:frame-source-location-for-emacs index)
                                                             index)
                                        :locals (swank-backend::frame-locals index)))))))

;;;; *** Handling internal UCW errors

(defgeneric handle-request-error (error backtrace))

(defmethod handle-request-error :before ((error error) backtrace)
  (declare (ignore backtrace))
  (let ((response (context.response *context*)))
    (setf *context* (clear-context *context*)
          (get-header response "Status") "500"
          (get-header response "Content-Type") "text/html")))

(defmethod handle-request-error :around ((error error) backtrace)
  (declare (ignore backtrace))
  (handler-bind ((error (lambda (c)
                          (ucw.rerl.error "Recursive request error ~S." c)
                          (restart-case 
                              (invoke-debugger c)
                            (abort ()
                              :report "Abort this request"
                              (throw 'abort-request nil))))))
    (call-next-method)))

(defmacro with-error-page ((&key title) &body contents)
  (rebinding (title)
    `(with-yaclml-stream (html-stream (context.response *context*))
       (<:html
        (<:head
         (<:title ,title))
        (<:body
         (<:h1 ,title)
         ,@contents)))))

(defmethod handle-request-error ((error error) backtrace)
  (if (and (not (eq *context* :unbound))
           (context.session *context*)
           (context.current-frame *context*))
      (progn
        (setf (frame.window-component (context.current-frame *context*))
              (make-instance 'error-component :condition error
                             :message (princ-to-string error)
                             :backtrace backtrace))
        (with-yaclml-stream (html-stream (context.response *context*))
          (render (frame.window-component (context.current-frame *context*)))))
      (with-error-page (:title "An internal server error has occured.")
        (<:p "An internal server error has occured.")
        (block walk-backtrack
          (handler-bind ((error (lambda (c)
                                  (return-from walk-backtrack c))))
            (<:table :border
                     (<:tr (<:th "Index") (<:th "Description") (<:th "Locals") (<:th "Source"))
                     (dolist (b backtrace)
                       (<:tr (<:td (<:as-html (backtrace-frame-index b)))
                             (<:td (<:as-html (backtrace-frame-description b)))
                             (<:td (<:as-html (backtrace-frame-locals b)))
                             (<:td (<:as-html (backtrace-frame-source-location b)))))))))))


(defmethod handle-request-error ((err rerl-error) backtrace)
  (declare (ignore backtrace))
  (with-error-page (:title "An internal server error has occured.")
    (<:p "Sorry, an internal server error has occured.")))

(defmethod handle-request-error ((err inexistent-request-part) backtrace)
  (declare (ignore backtrace))
  (with-error-page (:title (strcat "Missing Request Part." err))
    (<:p "Sorry, don't know what to do with " (<:as-html (rerl-error.query-path err)))))

(defmethod handle-request-error ((err inexistent-application-name) backtrace)
  (declare (ignore backtrace))
  (with-error-page (:title "Inexistent Application Name.")
    (<:p "Error: The URL " (<:tt (<:as-html (rerl-error.query-path err)))
         " does not name a known application.")))

(defmethod handle-request-error ((err inexistent-action-id) backtrace)
  (declare (ignore backtrace))
  (with-error-page (:title (strcat "Inexistent Action Id: " (slot-value err 'action-id)))
    (<:p "No action associated with the action-id "
         (<:tt (<:as-html (slot-value err 'action-id))))))

;;;; *** Handling user errors

(defvar *current-condition* nil)

(defvar *current-backtrace* nil)

(defgeneric handle-action-error (error backtrace)
  (:documentation "Method called when a user defined action
  signals an error."))

(defgeneric handle-action-error-using-application
    (application error backtrace)
  (:documentation
   "Hook for application specific error handilng code.

Please rename me. Please, God please!"))

;; TODO error handling needs special care when processing an ajax action
(defmethod handle-action-error ((error error) backtrace)
  (ucw.rerl.error "Handling action error ~A." error)
  (handler-bind ((error (lambda (c)
                          (ucw.rerl.error "Recursive action error ~A." c)
                          (restart-case 
                              (invoke-debugger c)
                            (abort ()
                              :report "Abort this action"
                              (throw 'abort-action nil))))))
    (ucw.rerl.error "Error ~A while serving action." error)
    (when (debug-on-error (context.application *context*))
      (setf *current-condition* error
            *current-backtrace* backtrace)
      (restart-case
          (swank:swank-debugger-hook error nil)
        (show-backtrace ()
          :report "Send the client a backtrace page."
          t)))
    ;; if we get here it's because *debug-on-error* was T but we
    ;; weren't connectod to slime.
    (ucw.rerl.info "Not debugging.")
    (if (and *context* (context.application *context*))
        ;; let the application handle the error
        (handle-action-error-using-application (context.application *context*) error backtrace)
        ;; no application :(. man we're lost...
        (setf (frame.window-component (context.current-frame *context*))
              (make-instance 'error-component
                             :condition error
                             :message (princ-to-string error)
                             :backtrace backtrace)))
    (ucw.rerl.error "Aborting action.")
    (throw 'abort-action nil)))

(defmacro with-action-error-handler ((&key (error-type 'error)) &body body)
  "Execute BODY with a handle for conditions of type ERROR-TYPE
which calls HANDLE-ACTION-ERROR."
  (with-unique-names (condition block)
    `(block ,block
       (handler-bind ((,error-type (lambda (,condition)
                                     (catch 'abort-action
                                       (handle-action-error ,condition
                                                            (collect-backtrace ,condition)))
                                     (return-from ,block nil))))
         ,@body))))

;;;; *** Generating bug reports in emacs

(defun send-backtrace-to-emacs (server condition backtrace)
  (let ((swank::*emacs-connection* (or swank::*emacs-connection*
                                       (swank::default-connection))))
    (swank::eval-in-emacs
     `(save-excursion
        (loop
           with buffer-name = "*UCW Backtrace <%d>*"
           for id upfrom 0
           for backtrace-buffer = (get-buffer (format buffer-name id))
           while backtrace-buffer
           finally do (switch-to-buffer-other-window (format buffer-name id)))
        (insert ,(generate-backtrace-for-emacs server condition backtrace))))))

(defun generate-backtrace-for-emacs (server condition backtrace)
  (let ((*print-circle* t)
        (*print-pretty* nil))
    (with-output-to-string (s)
      (flet ((show-obj (label object)
               (format s "~A: ~S~%" label object)
               (describe object s)))
        (write-line   "--- UCW Backtrace" s)
        (show-obj     "---    Condition" condition)
        (multiple-value-bind (second minute hour date month year )
            (decode-universal-time (get-universal-time))
          (format s   "---    Date: ~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0D~%"
                  year month date hour minute second))
        (format s     "---    Lisp: ~S ~S~%" (lisp-implementation-type) (lisp-implementation-version))
        (show-obj     "---    Server" server)
        (show-obj     "---    Backend" (server.backend server))
        (show-obj     "---    Application" (context.application *context*))
        (show-obj     "---    Request" (context.request *context*))
        (show-obj     "---    Response" (context.response *context*))
        (write-line   "--- BACKTRACE" s)
        (dolist (frame backtrace)
          (format s   "--- FRAME ~D~%" (backtrace-frame-index frame))
          (write-line (backtrace-frame-description frame) s)
          (write-line "---   Locals:" s)
          (dolist (local (backtrace-frame-locals frame))
            (format s "~S ==> ~S~%" (getf local :name) (getf local :value)))
          (write-line "---   Source:" s)
          (format s "~S~%" (backtrace-frame-source-location frame)))))))

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
