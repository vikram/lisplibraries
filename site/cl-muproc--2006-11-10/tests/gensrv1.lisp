;;;
;;; This file contains property of Mu Aps.
;;; Copyright (c) 2005.  All rights reserved.
;;;

(in-package :muproc-test.gensrv1)

(muproc-default-server-name :test1)

(defclass state ()
  ((hash    :initarg :hash
	    :reader state-hash)
   (exits   :initform nil
	    :accessor state-exits)))

;;; SERVER INTERFACE / STARTING & STOPPING
;;; ============================================================================

(defun start (&key initargs port-name)
  "Used for testing the defaulting of server-name"
  (muproc-generic-start :port-name port-name
			:initargs initargs))

(defun stop (&optional server-ref)
  (muproc-generic-stop server-ref))

;;; SERVER INTERFACE / HANDLERS 
;;; ============================================================================

(muproc-define-CALL-handler lookup (state from key)
  (gethash key (state-hash state)))

(muproc-define-CALL-handler store (state from key value)
  (setf (gethash key (state-hash state)) value))

(muproc-define-CALL-handler all-values (state from)
  (loop for key being the hash-keys of (state-hash state) using (hash-value val)
	collect (cons key val)))

(muproc-define-CALL-handler port-name (state from)
  "Returns the generic server's port name."
  *muproc-generic-server-port-name*)

(muproc-define-CALL-handler exits (state from)
  "Retrieves the reasons for terminations received from linked processes."
  (copy-list (state-exits state)))

(muproc-define-CAST-handler cast-store (state from key value)
  (setf (gethash key (state-hash state)) value))

(muproc-define-CAST-handler cast-lookup (state from key)
  (mumsg-send from :value (gethash key (state-hash state))))

;;; GEN-SERVER SERVICE FUNCTIONS (CALL-BACKS)
;;; ============================================================================

(defun initialize (&optional (init-key :defkey) (init-value 77))
  (muproc-set-trap-exits t)
  #+(or) (muproc-log-errorstream "INTIALIZING: ~a ~a." init-key init-value)
  (let ((state (make-instance 'state
		 :hash (make-hash-table :test #'eq))))
    (setf (gethash init-key (state-hash state)) init-value)
    state))

(defun terminate (state)
  (declare (ignore state))
  #+(or) (muproc-log-errorstream "TERMINATING."))

(defun linked-exit (state terminated reason)
  (declare (ignore terminated))
  (push reason (state-exits state)))