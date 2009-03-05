;;;; -*- lisp -*-

(in-package :it.bese.ucw)

(defvar *admin-application*
  (make-instance 'standard-application
                 :url-prefix "/admin/"
                 :tal-generator (make-instance 'yaclml:file-system-generator
                                               :cachep t
                                               :root-directories (list *ucw-tal-root*))
                 :www-roots (list (merge-pathnames "./admin/" *ucw-tal-root*))
                 :dispatchers (list (action-dispatcher)
				    (url-dispatcher "index.ucw"
				      (call 'admin-app)))))

(defcomponent admin-app (simple-window-component)
  ((body :initarg :body
         :accessor admin-app.body
         :initform nil
         :component admin-login))
  (:default-initargs :title "UCW Administration" :stylesheet "/ucw/ucw.css"))

(defmethod render ((app admin-app))
  (<:h1 "UCW Administration.")
  (render (admin-app.body app))
  (<:br)
  (<:A :href "index.ucw" "Back to admin entry."))

(defclass admin-login (login)
  ()
  (:metaclass standard-component-class))

(defmethod check-credentials ((login admin-login))
  (and (string= (login.username login) "admin")
       (string= (login.password login) "admin")))

(defaction login-successful ((l admin-login))
  (let* ((control-panel (make-instance 'admin-control-panel))
         (server-repl (make-instance 'admin-repl))
         (applications-contents (mapcar (lambda (app)
                                          (cons (application.url-prefix app)
                                                (make-instance 'application-inspector
                                                               :datum app)))
                                        (server.applications *default-server*)))
         (applications (make-instance 'tabbed-pane
                                      :contents applications-contents
                                      :current-component-key (caar applications-contents))))
    (call 'tabbed-pane
          :contents (list
                     (cons "Control Panel" control-panel)
                     (cons "Server REPL" server-repl)
                     (cons "Applications" applications))
          :key-test #'string=
          :current-component-key "Control Panel")))

;;;; Control Panel

(defclass admin-control-panel ()
  ()
  (:metaclass standard-component-class))

(defmethod render ((c admin-control-panel))
  (<:ul
   (<:li (<ucw:a :action (admin-room c) "ROOM"))
   (<:li (<ucw:a :action (start-slime-server c) "Start a SLIME server."))
   (<:li (<ucw:a :action (toggle-inspectors c) 
                 (<:as-html (if *inspect-components*
                                "Deactivate "
                                "Activate "))
                 "inspectors on all components."))
   (<:li (<ucw:a :action (shutdown-ucw c) "Shutdown this instance of UCW."))))

(defaction toggle-inspectors ((c admin-control-panel))
  (setf *inspect-components* (not *inspect-components*))
  (call 'info-message :message (strcat "Inspectors " (if *inspect-components*
                                                         ""
                                                         "de")
                                       "activated.")))

(defaction shutdown-ucw ((c admin-control-panel))
  (call 'info-message
        :message "I. Don't. Think. So."
        :ok-text "Sorry, I didn't really mean it."))

;;; ROOM

(defclass admin-room ()
  ()
  (:metaclass standard-component-class))

(defaction admin-room ((c admin-control-panel))
  (call 'admin-room))

(defmethod render ((room admin-room))
  (<ucw:a :action (answer-component room t) "OK.")
  (<:pre
   (<:as-is (with-output-to-string (*standard-output*)
              (room t))))
  (<ucw:a :action (answer-component room t) "OK."))

;;;; Slime integration

(defaction start-slime-server ((c admin-control-panel))
  (ucw.admin.info "Starting slime server.")
  (call 'info-message
        :message (format nil "Swank server started on port ~D." (swank:create-server))))

;;;; The REPL

(defclass package-select-field (select-field)
  ()
  (:default-initargs
   :data-set (sort (list-all-packages)
                   #'string< :key #'package-name)))

(defmethod render-value ((field package-select-field) (package package))
  (<:as-html (package-name package)))

(defcomponent admin-repl (template-component)
  ((package-select :accessor package-select :initform (make-instance 'package-select-field))
   (input :accessor input :initform (make-instance 'textarea-field
                                                   :rows 10
                                                   :cols 60))
   (form-value :accessor admin-repl.form-value :initarg :form-value :initform nil))
  (:default-initargs :template-name "ucw/admin/admin-repl.tal"))

(defun admin-do-eval (repl)
  "Evaluate FORM in PACKAGE. We can't do this directly from the
  EXECUTE-FORM action due to variable renaming issues."
  (with-slots (package-select input form-value)
      repl
    (let* ((*package* (value package-select))
           (form (read-from-string (value input))))
      (setf form-value (eval form)))))

(defaction submit ((repl admin-repl))
  (ucw.admin.info "Executing ~S." (client-value (input repl)))
  ;; parse the form values
  (admin-do-eval repl))

(defaction new-repl ((repl admin-repl))
  (setf (place (component.place repl)) (make-instance 'admin-repl)))

(defmethod safe-print-repl-value ((repl admin-repl))
  (let ((*print-circle* t))
    (labels ((abort-print (new-value)
               (format t "Calling ABORT-PRINT with value ~S.~%" new-value)
               (setf (admin-repl.form-value repl) new-value)
               (return-from safe-print-repl-value
                 "#<ERROR PRINTING VALUE>")))
      (restart-case
          (handler-bind ((error #'abort-print))
            (princ-to-string (admin-repl.form-value repl)))
        (:return-condition ()
          :report "Continue using NIL as the value."
          (abort-print nil))))))

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
