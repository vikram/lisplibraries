;;;Copyright (c) 2006-2007, Arthur Smyles
;;;All rights reserved.
;;;
;;;Redistribution and use in source and binary forms, with or without
;;;modification, are permitted provided that the following conditions are met:
;;;
;;;1. Redistributions of source code must retain the above copyright notice,
;;;   this list of conditions and the following disclaimer.
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;         documentation and/or other materials provided with the distribution.
;;;
;;;         THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;;;         AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;;         IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;;         ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
;;;         LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;;         CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;;;         SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;;         INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;;;         CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;;;         ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;;         POSSIBILITY OF SUCH DAMAGE.
;;;
(in-package #:cl-berkeley-db.common-lisp.net)

(defstruct (env (:constructor nil))
  pointer
  error-fn
  message-fn
  event-notify
  is-alive
  current-thread
  thread-name
  feedback)

(override (setf env-error-fn) (fn env) 
  (if fn
   (unless (env-error-fn env)
     (setf (env-err-call env) (callback standard-error-callback)))
   (setf (env-err-call env) (null-pointer)))
   (call-next-method fn env))

(override (setf env-message-fn) (fn env &aux (prev (env-message-fn env)))
 (if fn
   (unless prev
     (setf (env-msg-call env) (callback standard-message-callback)))
   (setf (env-msg-call env) (null-pointer)))
 (call-next-method fn env))

(override (setf env-event-notify) (fn env)
   (if fn
     (unless (env-event-notify env)
       (setf (env-event-notify-callback env) (callback standard-event-notify-callback)))
     (setf (env-event-notify-callback env) (null-pointer)))
   (call-next-method fn env))

(override (setf env-feedback) (fn env)
	  (if fn
	    (unless (env-feedback env)
	      (setf (env-feedback-callback env) (callback standard-env-feedback-callback)))
	    (setf (env-feedback-fcn env) (null-pointer)))
	    (call-next-method fn env))

(override (setf env-is-alive) (fn env)
	  (if fn
	    (unless (env-is-alive env)
	      (setf (env-isalive-callback env) (callback standard-isalive-callback)))
	    (setf (env-isalive-callback env) nil))
	    (call-next-method fn env))

(override (setf env-current-thread) (fn env)
	  (if fn
	    (unless (env-current-thread env)
	      (setf (env-thread-id-callback env) (callback standard-thread-id-callback)))
	    (setf (env-thread-id-callback env) nil))
	    (call-next-method fn env))

(override (setf env-thread-name) (fn env)
	  (if fn
	    (unless (env-thread-name env)
	      (setf (env-thread-id-string-callback env) (callback standard-thread-id-string-callback)))
	    (setf (env-thread-id-string-callback env) nil))
	    (call-next-method fn env))
;events
(defstruct (db-event (:constructor nil)) code)
(defstruct (panic (:constructor panic) (:include db-event (code DB_EVENT_PANIC))))
(defstruct (rep-client (:constructor rep-client) (:include db-event (code DB_EVENT_REP_CLIENT))))
(defstruct (rep-master (:constructor rep-master) (:include db-event (code DB_EVENT_REP_MASTER))))
(defstruct (rep-new-master (:constructor rep-new-master (env-id))(:include db-event (code DB_EVENT_REP_NEWMASTER))) env-id)
(defstruct (rep-startup-done (:constructor rep-startup-done) (:include db-event (code DB_EVENT_REP_STARTUPDONE))))
(defstruct (write-failed (:constructor write-failed) (:include db-event (code DB_EVENT_WRITE_FAILED))))

;event constructor
(defun db-event (event-type &rest parameters)
  (ecase event-type
    (DB_EVENT_PANIC (panic))
    (DB_EVENT_REP_CLIENT (rep-client))
    (DB_EVENT_REP_MASTER (rep-master))
    (DB_EVENT_REP_NEWMASTER (rep-new-master (first parameters)))
    (DB_EVENT_REP_STARTUPDONE (rep-startup-done))
    (DB_EVENT_WRITE_FAILED (write-failed))))
    
(cffi:defcallback standard-error-callback :void ((dbenv :pointer)
					   (errpfx :string)
					   (msg :string))
   (with-foreign-slots ((api1_internal) dbenv __db_env)
	(let ((instance (sb-kernel:make-lisp-obj (mem-aref api1_internal :uint))))
	    (funcall (env-error-fn instance) instance errpfx msg))))

(cffi:defcallback standard-message-callback :void ((dbenv :pointer) (msg :string))
   (with-foreign-slots ((api1_internal) dbenv __db_env)
	(let ((instance (sb-kernel:make-lisp-obj (mem-aref api1_internal :uint))))
	    (funcall (env-message-fn instance) instance msg))))

;TODO need to implement the logging api before finishind
(cffi:defcallback standard-app-dispatch :void ((dbenv :pointer)
					       (log_rec :pointer)
					       (lsn :pointer)
					       (op db_recops))
  (let ((env (sb-kernel:make-lisp-obj (mem-aref (foreign-slot-value dbenv '__db_env 'api1_internal) :uint)))
	(log-record (make-instance 'dbt :pointer log_rec)))
    (declare (ignorable env log-record lsn op))))

(cffi:defcallback standard-event-notify-callback :void ((dbenv :pointer)
							(event :uint32)
							(event-info :pointer))
  (let ((env (sb-kernel:make-lisp-obj (mem-aref (foreign-slot-value dbenv '__db_env 'api1_internal) :uint))))
    (funcall (env-event-notify env) 
	     (case event
	       (DB_EVENT_REP_NEWMASTER (db-event event (mem-aref event-info :int)))
	       (otherwise (db-event event))))))

(defcallback standard-env-feedback-callback :void ((dbenv :pointer)
						  (opcode :int)
						  (percent :int))
	     (let ((instance (sb-kernel:make-lisp-obj (mem-aref (foreign-slot-value dbenv '__db_env 'api1_internal) :uint))))
		 (funcall (env-feedback instance) instance opcode percent)))

(defcallback standard-isalive-callback :int ((dbenv :pointer)
					     (pid :uint32)
					     (tid :uint32)
					     (flags :uint32))
   (let ((instance (sb-kernel:make-lisp-obj (mem-aref (foreign-slot-value dbenv '__db_env 'api1_internal) :uint))))
	(if (funcall (env-feedback instance) instance pid tid flags) 1 0)))

(defcallback standard-thread-id-callback :void ((dbenv :pointer)
						(pidp :pointer)
						(tidp :pointer))
(let ((instance (sb-kernel:make-lisp-obj (mem-aref (foreign-slot-value dbenv '__db_env 'api1_internal) :uint))))
	(multiple-value-bind (tid pid) (funcall (env-current-thread instance) instance)
	  (setf (mem-aref tidp :uint32) tid
		(mem-aref pidp :uint32) (or pid 0)))))

(defcallback standard-thread-id-string-callback :string ((dbenv :pointer)
							 (pid :uint32)
							 (tid :uint32)
							 (buffer :pointer))
	(let ((instance (sb-kernel:make-lisp-obj (mem-aref (foreign-slot-value dbenv '__db_env 'api1_internal) :uint))))
	 (lisp-string-to-foreign (funcall (env-thread-name instance) instance pid tid) buffer DB_THREADID_STRLEN)
	 buffer))

(defun env (&rest flags)
  (make-instance 'env :flags flags))

(defmethod initialize-instance ((instance env) &key flags)
  (setf (env-error-fn instance) nil
	(env-message-fn instance) nil
	(env-event-notify instance) nil
	(env-is-alive instance) nil
	(env-current-thread instance) nil
	(env-thread-name instance) nil
	(env-feedback instance) nil)
  (with-foreign-object (result :pointer)
	(handle-db-error (db_env_create result (apply #'logior flags)))
	(setf (env-pointer instance) (mem-aref result :pointer)))
	;set the error and message callbacks
  (with-foreign-slots ((api1_internal) (env-pointer instance) __db_env)
           ;pointer to the instance, used by the callbacks
	   (setf api1_internal (foreign-alloc :uint :initial-element (sb-kernel:get-lisp-obj-address instance))))

  instance)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; METHODS 

(defmacro define-env-method (name (&rest args) (slot &optional handle-errors) &body body)
    `(define-struct-method ,name ,args (,slot __db_env :cstruct-reader env-pointer ,@(when handle-errors `(:handle-errors ,handle-errors)))
	,@body))

(define-env-method env-close (env &rest flags) (close t)
  (ff-call :int (apply #'logior flags)))

(define-env-method env-db-remove (env txn file database &rest flags) (dbremove t)
   (ff-call :pointer txn :string file :string database :uint32 (apply #'logior flags)))

(define-env-method env-db-rename (env txn file database newname &rest flags) (dbrename t)
   (ff-call :pointer txn :string file :string database :string newname :uint32 (apply #'logior flags)))

(define-env-method env-err (env code fmt &rest parameters) (err t)
   (let ((message (apply #'format nil fmt parameters)))
     (ff-call :int code :string message)))

(define-env-method env-errx (env fmt &rest parameters) (errx t)
  (let ((message (apply #'format nil fmt parameters)))
    (ff-call :string message)))

(define-env-method env-fail-check (env &rest flags) (failchk t)
   (ff-call :int (apply #'logior flags)))

(define-env-method env-fileid-reset (env file &rest flags) (fileid_reset t)
   (ff-call :string file :int (apply #'logior flags)))

(define-env-method env-home (env) (get_home t)
  (with-foreign-object (result :pointer)
       (ff-call :pointer result)
       (cffi:foreign-string-to-lisp (mem-aref result :pointer))))

(define-env-method env-open-flags (env) (get_open_flags t)
   (with-foreign-object (result :uint)
      (ff-call :pointer result)
      (mem-aref result :uint)))

(define-env-method env-lsn-reset (env file &rest flags) (lsn_reset t)
    (ff-call :string file :int (apply #'logior flags)))

(define-env-method env-open (env home mode &rest flags) (open t)
   (ff-call :string home :uint32 (apply #'logior flags) :int mode))

(defun env-open* (home mode &rest flags)
  "Creates and opens a new instance in one call"
  (flet ((create-flags (x) (eql x DB_RPCCLIENT)))
  (let ((result (if (member DB_RPCCLIENT flags) (env DB_RPCCLIENT) (env))))
    (apply #'env-open result home mode (remove-if #'create-flags flags))
    result)))

(define-env-method env-remove (env home &rest flags) (remove t)
   (ff-call :string home :int (apply #'logior flags)))

(define-env-method env-stat-print (env &rest flags) (stat_print t)
   (ff-call :int (apply #'logior flags)))

(define-env-method (setf env-alloc) (env malloc realloc free) (set_alloc t)
   (error "not implemented"))

(define-env-method (setf env-app-dispatch) (cb env) (set_app_dispatch)
   (ff-call :pointer cb))

;I'd like to use setf here but not sure how
(define-env-method (setf env-cache-size) (size nocaches env) (set_cachesize t)
   (error "not implemented"))

(define-env-method env-cache-size (env) (get_cachesize t)
      (with-foreign-objects ((gbytes :uint32)
			     (bytes :uint32)
			     (ncache :int))
			    (ff-call :pointer gbytes :pointer bytes :pointer ncache)
			    (values (+ (* (expt 2 32) (mem-aref gbytes :uint32)) (mem-aref bytes :uint32)) ncache)))

(define-env-method env-add-data-dir (env dir) (set_data_dir t)
   (ff-call :string dir))

(define-env-method env-data-dirs (env) (get_data_dirs t)
   (with-foreign-object (result :string)
	(ff-call :pointer result)
	(mem-aref result :string)))

(define-env-method env-encrypt (env password flags) (set_encrypt t)
		   (ff-call :string password :uint32 (apply #'logior flags)))

(define-env-method env-encrypt-flags (env) (get_encrypt_flags t)
		   (with-foreign-object (result :uint)
					(ff-call :pointer result)
					(mem-aref result :uint)))

(define-env-method (setf env-err-call) (call env) (set_errcall)
   (ff-call :pointer call))

(define-env-method (setf env-msg-call) (call env)(set_msgcall)
   (ff-call :pointer call))

(define-env-method (setf env-err-file) (err-file env) (set_errfile t)
		     (error "not implemented"))

(define-env-method env-err-file (env) (get_errfile t)
		     (error "not implemented"))

(define-env-method env-msg-file (env) (get_msgfile)
   (error "unsupported operation"))

(define-env-method (setf env-msg-file) (file env)(set_msgfile)
   (error "unsupoorted operation"))

(define-env-method (setf env-error-prefix) (error-prefix env) (set_errpfx)
		    (ff-call :string error-prefix))

(define-env-method env-error-prefix (env) (get_errpfx)
		  (with-foreign-object (result :string)
				       (ff-call :pointer result)
				       (mem-aref result :string)))

(define-env-method (setf env-event-notify-callback) (cb env) (set_event_notify t)
   (ff-call :pointer cb))

(define-env-method (setf env-feedback-callback) (cb env) (set_feedback t)
		      (ff-call :pointer cb))

(define-env-method (setf env-flags) (on env &rest flags) (set_flags t)
   (ff-call :uint32 (apply #'logior flags) :int (if on 1 0)))

(define-env-method env-flags (env) (get_flags t)
   (with-foreign-object (result :uint32)
       (ff-call :pointer result)
       (mem-aref result :uint32)))

(define-env-method (setf env-isalive-callback) (cb env) (set_isalive t)
  (ff-call :pointer (or cb (null-pointer))))

(define-env-method env-rpc-server (env client host client-timeout server-timout &rest flags) (set_rpc_server t)
   (error "not implemented"))

(define-env-method (setf env-shm-key) (shm_key env) (set_shm_key t)
	(ff-call :long shm_key))

(define-env-method env-shm-key (env) (get_shm_key t)
	(with-foreign-object (result :long)
		(ff-call :pointer result)
		(mem-aref result :long)))

(define-env-method (setf env-thread-id-callback) (cb env) (set_thread_id t)
   (ff-call :pointer (or cb (null-pointer))))

(define-env-method (setf env-thread-count) (count env) (set_thread_count t)
   (ff-call :uint32 count))
;this is mentioned in the documentation but not in the header files
#+(or) (define-env-method env-thread-count (env) (get_thread_count t)
   (with-foreign-object (result :uint32)
	(ff-call :pointer result)
	(mem-aref result :uint32)))

(define-env-method (setf env-thread-id-string-callback) (cb env) (set_thread_id_string t)
   (ff-call :pointer (or cb (null-pointer))))

(define-env-method (setf env-timeout) (timeout env) (set_timeout t)
   (ff-call :uint32 timeout))

(define-env-method env-timeout (env) (get_timeout t)
   (with-foreign-object (result :uint32)
	(ff-call :pointer result)
	(mem-aref result :uint32)))

(define-env-method (setf env-temp-dir) (dir env) (set_tmp_dir t)
  (ff-call :string dir))

(define-env-method env-temp-dir (env) (get_tmp_dir t)
  (with-foreign-object (result :string)
     (ff-call :pointer result)
     (mem-aref result :string)))


(define-env-method (setf env-verbose) (on env which) (set_verbose t)
  (ff-call :uint32 which :int (if on 1 0)))

(define-env-method env-verbose (env which) (get_verbose t)
  (with-foreign-object (result :int)
    (ff-call :uint32 which :pointer result)
    (not (zerop (mem-aref result :int)))))
	
