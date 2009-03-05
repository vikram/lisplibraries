;;; Copyright (c) 2005-2006, Mu Aps. All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; WRITTEN BY: KLAUS HARBO klaus@harbo.net / klaus@mu.dk

(in-package :cl-muproc.generic-server)

;;; MACROS
;;; ============================================================================

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defparameter *debug-level* 0
    "Integer indicating level of debug output, the higher the number
the more debug output."))

(defmacro dbgformat (level &rest args)
  "Log `args' with `muproc-log-errorstream' iff `*muproc-dedbgformat-level*'
      is not NIL and `level' >= `*muproc-dedbgformat-level*'."
  `(when (and *debug-level* (>= *debug-level* ,level))
    (muproc-log-errorstream (format nil "GENSRV(~a): ~a" ,level 
			     (format nil ,@args)))))

;;; CONDITIONS
;;; ============================================================================

(define-condition muproc-generic-server-condition (cl-muproc::muproc-condition) ())
(define-condition muproc-generic-server-error (muproc-generic-server-condition error) ())
(define-condition muproc-exit-after-this-condition (muproc-generic-server-condition) 
  ((unsettingp       :initarg :unsettingp
		    :reader exit-after-this-unsettingp)
   (reason          :initarg :reason
		    :reader exit-after-this-reason)))

(defun gs-error (&rest args)
  (apply #'error (cons 'muproc-generic-server-error args)))

;;; DEFVARS
;;; ============================================================================

(defvar *state*)
(defvar *muproc-generic-server-package*)
(defvar *muproc-generic-server-port-name*)

;;; UTILITY FUNCTIONS
;;; ============================================================================

(defun ensure-server-function (symbol package)
  (assert (symbolp symbol))
  (unless (find-package package)
    (error "Cannot find server package: ~s." package))
  (let ((fsym (find-symbol (symbol-name symbol) package)))
    (unless fsym
      (error "Cannot find server function symbol ~s in package ~s." 
	     symbol package))
    (unless (fboundp fsym)
      (error "No function binding for symbol ~s in package ~s." 
	     fsym package))
    (symbol-function fsym)))

;;; START
;;; ============================================================================

(defun %generic-start (port-name initargs package) 
  (dbgformat 3 "GEN-SERVER (START) called pkg=~s." package)
  (when port-name
    (setf port-name (intern (symbol-name port-name) :keyword)))
  (when (and port-name 
	     (not (muproc-port-name-p port-name)))
    (error "Not valid a port name: ~s." port-name))
  (dbgformat 5 "PORTNAME=~s PACKAGE=~s." port-name package)
  (setf package (find-package package))
  (unless (typep package 'package)
    (error "Not a package: ~s." package))
  (let* ((init-sym (find-symbol "INITIALIZE" package)))
    (unless (and init-sym (fboundp init-sym))
      (error "Unable to call ~s server initialize function in package ~a." 
	     (muproc-name) package))
    (progv
	'(*state*
	  *muproc-generic-server-package* 
	  *muproc-generic-server-port-name*)
	(list (apply (symbol-function init-sym) initargs)
	      package port-name)
      (main-loop))))

(defmacro muproc-generic-start (&key initargs (package (package-name *package*)) port-name)
  (let ((+port-name+ (gensym "PORT-NAME-"))
	(+pkg+ (gensym "PKG-")))
    `(progn
      (let* ((,+port-name+ (or ,port-name (intern (symbol-name (muproc-name)) :keyword)))
	     (,+pkg+ (find-package ,package)))
	(unless (muproc-port-name-p ,+port-name+)
	  (error "Not a valid muproc port name: ~s." ,+port-name+))
	(unless (packagep ,+pkg+)
	  (error "Unable to find package: ~s" ,package))
	(%generic-start ,+port-name+ ,initargs (package-name ,+pkg+))))))

;;; STOP
;;; ============================================================================

(defun %generic-stop (server-ref)
  (let (ref)
    (when (muproc-name-p server-ref)
      (setf ref (muproc-find server-ref)))
    (unless (muproc-address-p ref)
      (error "GENERIC-STOP -- Not a valid server reference: ~s (found ~s)." server-ref ref))
    (handler-case
	(muproc-with-message-tag (mytag)
	  (mumsg-send ref
		      :stop-tag mytag)
	  (dbgformat 4 "stop-fn receiving...")
	  (mumsg-receive (from)
	    ((stop-status tag) (and (eq stop-status :ok)
				    (muproc-msgtag= tag mytag))
	     (dbgformat 2 "Got :OK STOP-STATUS.")
	     stop-status)
	    ((stop-status tag) (muproc-msgtag= tag mytag)
	     (dbgformat 2 "Got non-OK STOP-STATUS: ~s." stop-status)
	     (typecase stop-status
	       (error      (error stop-status))
	       (condition  (signal stop-status))
	       (t          stop-status)))
	    (() t
	     (muproc-log-errorstream "Received unexpected packet: ~s." 
				     *muproc-packet*))))
      (error (err)
	(muproc-log-errorstream "GEN-SERVER (STOP) -- Error occurred: ~a." err)
	(error err)))))

(defmacro muproc-generic-stop (muproc-or-muproc-name)
  `(%generic-stop ,muproc-or-muproc-name))

(defun %handle-stop-msg (from stop-tag)
  (dbgformat 4 "Got STOP request from ~s." from)
  (flet ((report (result)
	   (mumsg-send from
		       :stop-status result
		       :tag stop-tag))) 
    (let ((fun (ensure-server-function 'terminate *muproc-generic-server-package*)))
      (assert (functionp fun))
      (handler-case
	  (progn
	    (funcall fun *state*)
	    (dbgformat 4 "Terminate OK.")
	    (report :ok))
	(error (err)
	  (muproc-log-errorstream "Error occurred in server termination: ~a" err)
	  (report err)))
      (muproc-exit :stop-command-received))))

;;; CALL -- 'SYNCHRONOUS' SERVER USE - FUNCTION CALL RETURNS VALUE
;;; ============================================================================

(defun muproc-generic-call (server-ref handler-name &rest args)
  ;; This function executes in the client muproc
  (handler-case 
      (let (ref)
	(dbgformat 3 "Check gen-args ref=~s handler-name=~s args=~s." 
		   server-ref handler-name args)
	(unless (or (muproc-name-p server-ref)
		    (muproc-p server-ref))
	  (error "Not valid muproc reference: ~s" server-ref))
	(setf ref (if (muproc-p server-ref) 
		      server-ref
		      (muproc-find server-ref)))
	(dbgformat 3 "receiving muproc=~s." ref)
	(dbgformat 5 "all muproc names=~s." (loop for key being the hash-keys of cl-muproc::*names*
						  collect (cons key (symbol-package key))))
	(dbgformat 7 "all procs=~s." (%all-processes%))
	(unless (%process-p% ref)
	  (error "GENERIC-CALL: Not a valid server reference: ~s (found ~s)." server-ref ref))
	(unless (fboundp handler-name)
	  (error "Undefined call-handler: ~s." handler-name))
	(dbgformat 3 "check-gen-args finished")
	(muproc-with-message-tag (mytag)
	  (handler-case
	      (mumsg-send ref
			  :call handler-name
			  :args args 
			  :tag mytag)
	    (muproc-send-to-terminated-muproc-condition ()
	      (error "Server no longer exists: ~s." ref)))
	  (mumsg-receive (from)
	    ((call-status payload tag) (muproc-msgtag= tag mytag)
	     (dbgformat 4 "GEN-SERVER-CLIENT (CALL): Got CALL-STATUS packet.")
	     (ecase call-status
	       (:normal     (apply #'values payload))
	       (:error      (error payload))
	       (:condition  (signal payload))))
	    (() t
	     (dbgformat 4 "GEN-SERVER-CLIENT (CALL): Unexpected packet received: ~s." *muproc-packet*)
	     (values)))))
    (error (err)
      (muproc-log-errorstream "GEN-SERVER-CLIENT (CALL): INTERNAL ERROR OCCURRED: ~a."
			      err))))

(defun %handle-call-msg (from call args tag)
  ;; This function executes in the server muproc
  (assert (typep from 'muproc-port))
  (assert (symbolp call))
  (flet ((report (status values/error)
	   (dbgformat 4 "GEN-SERVER (CALL): RETURNING packet status=~s val/err=~s."
		      status values/error)
	   (mumsg-send from
		       :call-status status
		       :payload values/error
		       :tag tag)))
    (handler-case
	(progn
	  (dbgformat 4 "GEN-SERVER (CALL): Got CALL message from=~s call=~s args=~s."
		     from call args)
	  (unless (fboundp call)
	    (error "GEN-SERVER (CALL): No function binding for ~s." call))
	  (report :normal
		  (multiple-value-list
		   (apply (symbol-function call) 
			  (cons *state* (cons from args))))))
      (error (err)
	(progn
	  (muproc-log-errorstream "GEN-SERVER (CALL): Error occurred: ~a." err)
	  (report :error err)
	  (error err)))
      (condition (con)
	(report :condition con)
	(signal con)))))
  
;;; CAST -- 'ASYNCHRONOUS' SERVER USE - NO FUNCTION CALL RETURN VALUE
;;; ============================================================================

(defun muproc-generic-cast (server-ref handler-name &rest args)
  ;; This function executes in the client muproc
  (handler-case
      (let (ref)
	(dbgformat 3 "Check gen-args ref=~s handler-name=~s." 
		   server-ref handler-name)
	(unless (or (muproc-name-p server-ref)
		    (muproc-p server-ref))
	  (error "Not valid muproc-name: ~s" server-ref))
	(setf ref (if (muproc-p server-ref)
		      server-ref
		      (muproc-find server-ref)))
	(unless (muproc-address-p ref)
	  (error "Not a valid server reference: ~s (found ~s)." server-ref ref))
	(unless (fboundp handler-name)
	  (error "Undefined cast-handler: ~s." handler-name))
	(dbgformat 3 "check-gen-args finished")
	(handler-case
	    (mumsg-send ref
			:cast handler-name
			:args args)
	  (muproc-send-to-terminated-muproc-condition ()
	    (error "Server no longer exists: ~s." ref))))
    (error (err)
      (muproc-log-errorstream "GEN-SERVER-CLIENT (CAST): INTERNAL ERROR OCCURRED: ~a."
			      err)
      (error err)))
  t)

(defun %handle-cast-msg (from cast args)
  ;; This function executes in the server muproc
  (assert (or (null from) (typep from 'muproc-port)))
  (assert (symbolp cast))
  (handler-case
      (progn
	(dbgformat 4 "Got CAST message.")
	(unless (fboundp cast)
	  (error "No function binding for ~s." cast))
	(apply (symbol-function cast) 
	       (cons *state* (cons from args))))
    (error (err)
      (muproc-log-errorstream
       "GEN-SERVER (CAST): Error occured calling ~s from ~s with args ~s: ~a." 
       cast from args err))
    (condition (con)
      (dbgformat 4 
		 "GEN-SERVER (CAST): Condition was signalled calling ~s from ~s with args ~s: ~a." 
		 cast from args con)
      (signal con))))

;;; TERMINATED/REASON MSG
;;; ============================================================================

(defun %handle-terminated/reason-msg (from terminated reason)
  (dbgformat 1 "Handling terminated/reason message: ~s." 
	     (list from terminated reason))
  ;; the origin of terminate messages is the muproc run-time system
  (handler-case 
      (let* ((sym (find-symbol "LINKED-EXIT" *muproc-generic-server-package*))
	     (fun (and sym (symbol-function sym))))
	(cond
	  (fun  (funcall fun *state* terminated reason))
	  (t    (dbgformat 1 "No LINKED-EXIT handler found")
		(muproc-exit :linked-exit))))
    (error (err)
      (muproc-log-errorstream 
       "GEN-SERVER (TERM/REASON): Error occurred handling term/reason message: ~a"
       err)
      (error err))))

;;; SERVER LOOP
;;; ============================================================================

(defun main-loop ()
  (handler-case
      (progn
	(dbgformat 1 "At top of generic server-loop: name=~s port=~s." 
		   (muproc-name) *muproc-generic-server-port-name*)
	(when *muproc-generic-server-port-name*
	  (muproc-register-port-name *muproc-generic-server-port-name*))
	(unwind-protect
	     (let (exit-after-this exit-after-this-reason)
	       (handler-bind ((muproc-exit-after-this-condition
			       (lambda (con)
				 (dbgformat 0 "Handling EXIT-AFTER-THIS")
				 (setf exit-after-this
				       (not (exit-after-this-unsettingp con)))
				 (setf exit-after-this-reason
				       (exit-after-this-reason con)))))
		 (handler-case
		     (progn
		       (loop
			(when exit-after-this
			  (muproc-exit exit-after-this-reason))
			(dbgformat 4 "Receiving...")
			(mumsg-receive (from)
			  ((stop-tag) t
			   (%handle-stop-msg from stop-tag))
			  ((call args tag) t
			   (dbgformat 5 "generic CALL ~s from ~s, args ~s" call from args)
			   (%handle-call-msg from call args tag))
			  ((cast args) t
			   (dbgformat 5 "generic CAST ~s from ~s, args ~s" cast from args)
			   (%handle-cast-msg from cast args))
			  ((terminated reason) t
			   (dbgformat 5 "generic TERM/REASON received from ~s with reason ~s" terminated reason)
			   (%handle-terminated/reason-msg from terminated reason))
			  (() t
			   (muproc-log-errorstream "GEN-SERVER -- Got unexpected packet: ~s"
						   *muproc-packet*)))))
		   (muproc-exit-condition ()
		     (dbgformat 1 "muproc-exit called - terminating.")))))
	  ;; unwinding
	  (let* ((term-sym (find-symbol "TERMINATE" *muproc-generic-server-package*))
		 (term-fn (and (fboundp term-sym) term-sym)))
	    (cond
	      (term-fn (handler-case
			   (funcall term-fn *state*)
			 (error (err)
			   (muproc-log-errorstream "GEN-SERVER: Error in TERMINATE: ~a." 
						   err))))
	      (t (dbgformat 1 "GEN-SERVER: Can't find fbound TERMINATE symbol in ~s."
			    *muproc-generic-server-package*))))
	  (when *muproc-generic-server-port-name*
	    (muproc-unregister-port-name *muproc-generic-server-port-name*))
	  (dbgformat 1 "GEN-SERVER terminating.")))
    (error (err)
      (muproc-log-errorstream "GEN-SERVER (MAIN-LOOP) -- INTERNAL ERROR ~a." err)
      (muproc-exit (list :internal-error err)))))

;;; SERVER API
;;; ============================================================================

(defvar *default-server-name*)

(defmacro muproc-default-server-name (name)
  (unless (symbolp name)
    (error "Not at symbol: ~s." name))
  `(eval-when (:load-toplevel :compile-toplevel :execute)
    (defparameter ,(intern "*DEFAULT-SERVER-NAME*") ',name)))

(defmacro muproc-define-call-handler (name arglist &body body)
  (let ((doc-string (when (stringp (car body)) (car body)))
	(handler-name (intern (format nil "CALL-HANDLER/~a" 
				      (symbol-name name))))
	;; server-selecting operators have an '*' at appended to the name
	(select-name (intern (format nil "~a*" name))))
    ;; doc-string belongs with marshalling proxy, not with call handler
    (when (stringp (car body)) (setf body (cdr body)))
    (destructuring-bind (state from &rest handler-args) arglist
      `(progn
	(export ',name)
	(export ',select-name)
	,@(if (boundp (intern "*DEFAULT-SERVER-NAME*"))
	      `((defun ,name (,@handler-args)
		  ,@(when (stringp doc-string) (list doc-string))
		  (dbgformat 4 "Calling ~s, ref=~s"
			     ',name ,(intern "*DEFAULT-SERVER-NAME*"))
		  (cl-muproc.generic-server:muproc-generic-call
		   ,(intern "*DEFAULT-SERVER-NAME*") ',handler-name ,@handler-args)))
	      (warn "No *default-server-name* defined."))
	(defun ,select-name (server-ref ,@handler-args)
	  ,@(when (stringp doc-string) (list doc-string))
	  (dbgformat 4 "Calling ~s, ref=~s"
		     ',name server-ref)
	  (cl-muproc.generic-server:muproc-generic-call server-ref ',handler-name ,@handler-args))
	(defun ,handler-name ,arglist
	  (declare (ignorable ,state ,from))
	  ,@body)))))

(defmacro muproc-define-cast-handler (name arglist &body body)
  (let ((doc-string (when (stringp (car body)) (car body)))
	(handler-name (intern (format nil "CAST-HANDLER/~a" 
				      (symbol-name name))))
	;; server-selecting operators have an '*' at appended to the name
	(select-name (intern (format nil "~a*" name))))
    ;; doc-string belongs with marshalling proxy, not with call handler
    (when (stringp (car body)) (setf body (cdr body))) 
    (destructuring-bind (state from &rest handler-args) arglist
      `(progn
	(export ',name)
	(export ',select-name)
	,@(if (boundp (intern "*DEFAULT-SERVER-NAME*"))
	      `((defun ,name (,@handler-args)
		  ,@(when (stringp doc-string) (list doc-string))
		  (dbgformat 4 "Casting to ~s, ref=~s"
			     ',name ,(intern "*DEFAULT-SERVER-NAME*"))
		  (cl-muproc.generic-server:muproc-generic-cast
		   ,(intern "*DEFAULT-SERVER-NAME*") ',handler-name ,@handler-args)))
	      (warn "No *default-server-name* defined."))
	(defun ,select-name (server-ref ,@handler-args)
	  ,@(when (stringp doc-string) (list doc-string))
	  (dbgformat 4 "Casting to ~s, ref=~s" ',name server-ref)
	  (cl-muproc.generic-server:muproc-generic-cast server-ref ',handler-name ,@handler-args))
	(defun ,handler-name ,arglist
	  (declare (ignorable ,state ,from))
	  ,@body)))))

(defun muproc-exit-after-handler (reason &key unsettingp)
  (signal 'muproc-exit-after-this-condition
	  :unsettingp (and unsettingp t)
	  :reason reason))