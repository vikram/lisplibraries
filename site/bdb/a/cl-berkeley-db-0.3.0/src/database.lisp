;;;;Copyright (c) 2006-2007, Arthur Smyles
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

(defstruct (db (:constructor nil)) pointer associate-fn data-compare-fn feedback append-recno-fn)
(defstruct (btree (:constructor nil) (:include db)) key-compare-fn key-prefix-fn)
(defstruct (hash (:constructor nil) (:include db)) fn)
(defstruct (recno (:constructor nil) (:include db)))
(defstruct (queue (:constructor nil) (:include db)))

;;define callbacks
(defcallback standard-associate-callback :int ((secondary :pointer)
					       (key :pointer)
					       (data :pointer)
					       (result :pointer))
   (with-foreign-slots ((api_internal) secondary __db)
	(let* ((instance (sb-kernel:make-lisp-obj (mem-aref api_internal :uint)))
	       (second-key (when (db-associate-fn instance)
		      (funcall (db-associate-fn instance) instance 
			       (make-instance 'dbt :pointer key) 
			       (make-instance 'dbt :pointer data)))))
	  (typecase second-key
	    (number second-key)
	    (otherwise (data-dbt second-key :pointer result))))))

(defcallback standard-data-compare-callback :int ((db :pointer)
					     (dbt-x :pointer)
					     (dbt-y :pointer))
   (let ((dbase (sb-kernel:make-lisp-obj (mem-aref (foreign-slot-value db '__db 'api_internal) :uint)))
	 (dbt-a (make-instance 'dbt :pointer dbt-x))
	 (dbt-b (make-instance 'dbt :pointer dbt-y)))
     (when (db-data-compare-fn dbase)
       (funcall (db-data-compare-fn dbase) dbase dbt-a dbt-b))))

(defcallback standard-db-feedback-callback :void ((db :pointer)
					       (opcode :int)
					       (percent :int))
   (let ((instance (sb-kernel:make-lisp-obj (mem-aref (foreign-slot-value db '__db 'api_internal) :uint))))
	   (funcall (db-feedback instance) instance opcode percent)))

(defcallback standard-append-recno-callback :int ((db :pointer)
						  (dbt :pointer)
						  (recno :uint32))
	 (let ((db (sb-kernel:make-lisp-obj (mem-aref (foreign-slot-value db '__db 'api_internal) :uint)))
	       (dbt (make-instance 'dbt :pointer dbt)))
	   (when (db-append-recno-fn db)
	     (app-dbt (funcall (db-append-recno-fn db) dbt recno) dbt))
	   0))

(defcallback standard-bt-compare-callback :int ((db :pointer)
						(dbt1 :pointer)
						(dbt2 :pointer))
  (let ((db (sb-kernel:make-lisp-obj (mem-aref (foreign-slot-value db '__db 'api_internal) :uint)))
	       (dbt1 (make-instance 'dbt :pointer dbt1))
	       (dbt2 (make-instance 'dbt :pointer dbt2)))
    (funcall (btree-key-compare-fn db) db dbt1 dbt2)))

(defcallback standard-bt-prefix-callback :int ((db :pointer)
						(dbt1 :pointer)
						(dbt2 :pointer))
   (let ((db (sb-kernel:make-lisp-obj (mem-aref (foreign-slot-value db '__db 'api_internal) :uint)))
	 (dbt1 (make-instance 'dbt :pointer dbt1))
	 (dbt2 (make-instance 'dbt :pointer dbt2)))
    (funcall (btree-key-prefix-fn db) db dbt1 dbt2)))

(defcallback standard-hash-callback :uint32 ((db :pointer)
					     (bytes :pointer)
					     (len :uint32))
   (let ((db (sb-kernel:make-lisp-obj (mem-aref (foreign-slot-value db '__db 'api_internal) :uint)))
	 (data (make-array len :element-type '(unsigned-byte 8))))
     (sb-kernel:copy-ub8-from-system-area bytes 0 data 0 len)
     (funcall (hash-fn db) data)))

(defun db (&optional env &rest flags)
  (make-instance 'db :env env :flags flags))

(defun btree (&optional env &rest flags)
  (make-instance 'btree :env env :flags flags))

(defun hash (&optional env &rest flags)
  (make-instance 'hash :env env :flags flags))

(defun recno (&optional env &rest flags)
  (make-instance 'recno :env env :flags flags))

(defun queue (&optional env &rest flags)
  (make-instance 'queue :env env :flags flags))

(defmethod initialize-instance ((instance db) &key env flags)
  (with-foreign-object (result :pointer)
	(handle-db-error (db_create result (if env (env-pointer env) (null-pointer)) (apply #'logior flags)))
	(setf (db-pointer instance) (mem-aref result :pointer)))
  (with-foreign-slots ((api_internal) (db-pointer instance) __db)
	(setf api_internal (foreign-alloc :uint :initial-element (sb-kernel:get-lisp-obj-address instance))))
  instance)

(override (setf db-compare-fn) (fn db)
    (unless (db-compare-fn db)
      (setf (db-flags db) DB_DUP
	    (db-dup-compare db) (callback standard-compare-callback)))
    (funcall super fn db))

(override (setf db-feedback) (fn db)
   (if fn
    (unless (db-feedback db)
      (setf (db-feedback-fcn db) (callback standard-db-feedback-callback)))
    (setf (db-feedback-fcn db) (null-pointer)))
    (call-next-method super fn db))

(override (setf db-append-recno-fn) (fn db)
    (setf (append-recno) (callback standard-append-recno-callback))
    (funcall super fn db))

(override (setf btree-key-compare-fn) (fn db)
    (setf (btree-compare) (if fn (callback standard-bt-compare-callback) (null-pointer)))
    (funcall super fn db))

(override (setf btree-key-prefix-fn) (fn db)
          (setf (btree-prefix) (if fn (callback standard-bt-prefix-callback) (null-pointer)))
	      (funcall super fn db))

(override (setf hash-fn) (fn db)
          (setf (hash-hash) (if fn (callback standard-hash-callback) (null-pointer)))
	      (funcall super fn db))

;;;;;;;methods
(defmacro define-db-method (name (&rest args) (slot &optional handle-errors) &body body)
  `(define-struct-method ,name ,args (,slot __db :cstruct-reader db-pointer ,@(when handle-errors `(:handle-errors ,handle-errors)))
	,@body))

(define-db-method db-env (db) (get_env :pointer)
   (let* ((env-pointer (ff-call))
	  (api_internal (foreign-slot-value env-pointer '__db_env 'api1_internal)))
   (sb-kernel:make-lisp-obj (mem-aref api_internal :uint))))

(define-db-method db-associate (primary secondary &optional txn &rest flags) (associate t)
  (ff-call :pointer (if txn (txn-pointer txn) (null-pointer)) :pointer (db-pointer secondary) :pointer (callback standard-associate-callback) :uint32 (apply #'logior flags)))

(define-db-method db-close (db &rest flags) (close t)
   (ff-call :uint32 (apply #'logior flags)))

(define-db-method db-compact (db start stop &optional compact txn &rest flags) (compact t)
   (error "not implemented"))

;cursor in cursor.lisp

(define-db-method db-delete (db key &optional txn &rest flags) (del t)
   (ff-call :pointer (if txn (txn-pointer txn) (null-pointer)) :uint32 (apply #'logior flags)))

(define-db-method db-fd (db) (fd :int)
   (with-foreign-object (result :int)
      (ff-call :pointer result)
      (mem-aref result :int)))
;Q.  under what circumstances do I need data
;A. to allow for partial returns and more user control
(define-db-method db-get (db key &optional txn (data (db-dbt)) &rest flags) (get t)
   (unless data (setf data (db-dbt)))
   (handler-case (ff-call :pointer (if txn (txn-pointer txn) (null-pointer)) :pointer (dbt-pointer key) :pointer (dbt-pointer data) :uint32 (apply #'logior flags))
     (not-found () (values nil nil))
     (:no-error (x) (declare (ignore x)) (values data key))))
;returns (values (secondary . data) primary)
(define-db-method db-pget (db key &optional txn (data (db-dbt)) &rest flags &aux (primary (db-dbt))) (pget t)
   (unless data (setf data (db-dbt)))
     (handler-case (ff-call :pointer (if txn (txn-pointer txn) (null-pointer)) :pointer (dbt-pointer key) :pointer (dbt-pointer primary)
			    :pointer (dbt-pointer data) :uint32 (apply #'logior flags))
       (not-found () (values nil nil nil))
       (:no-error (x) (declare (ignore x)) (values data key primary))))

(define-db-method db-byteswapped (db) (get_byteswapped t)
  (with-foreign-object (result :boolean)
	(ff-call :pointer result)
	(mem-aref result :boolean)))

(define-db-method db-type (db) (get_type t) 
  (with-foreign-object (result 'DBTYPE)
      (ff-call :pointer result)
      (case (mem-aref result 'DBTYPE)
	(:DB_BTREE 'btree)
	(:DB_HASH 'hash)
	(:DB_RECNO 'recno)
	(:DB_QUEUE 'queue)
	(otherwise 'db))))

(define-db-method db-join (db cursors &rest flags) (join t)
  (error "unimplemented method"))

(define-db-method db-key-range (db key &optional txn &rest flags) (key_range t)
  (error "unimplemented method"))

(define-db-method db-open (db file &rest flags) (open t)
  ;make sure it is greater than 1
  (when (oddp (length flags)) (push 0 flags))
  (let ((type (typecase db
		(btree :DB_BTREE)
		(hash :DB_HASH)
		(recno :DB_RECNO)
		(queue :DB_QUEUE)
		(otherwise :DB_UNKNOWN)))
	(mode (getf flags :mode 0))
	(database (getf flags :database (null-pointer)))
	(txn (getf flags :txn)))
    (remf flags :database)
    (remf flags :txn)
    (remf flags :mode)
  (ff-call :pointer (if txn (txn-pointer txn) (null-pointer))
	   :string file
	   :string database
	   DBTYPE type
	   :uint32 (apply #'logior flags)
	   :int mode)))

(defun db-open* (type env file &rest flags)
  "creates and opens a database handle"
  (flet ((create-flags (x) (eql x DB_XA_CREATE)))
    (let ((result (apply type env (remove-if-not #'create-flags flags))))
      (apply #'db-open result file (remove-if #'create-flags flags))
      result)))

(define-db-method db-name (db) (get_dbname t)
   (with-foreign-objects ((filename :string)
			  (database :string))
	(ff-call :pointer filename :pointer database)
	(values (mem-aref :string filename) (mem-aref :string database))))

(define-db-method db-open-flags (db) (get_open_flags t)
   (with-foreign-object (flags :uint32)
      (ff-call :pointer flags)
      (mem-aref flags :uint32)))

(define-db-method db-transactional (db) (get_transactional :boolean)
  (ff-call))

(define-db-method db-put (db key data &optional txn &rest flags) (put t)
  (ff-call :pointer (if txn (txn-pointer txn) (null-pointer))
           :pointer (dbt-pointer key) :pointer (dbt-pointer data)
	   :uint32 (apply #'logior flags)))

(define-db-method db-remove (db file &optional database &rest flags) (remove t)
   (ff-call :string file :string database :uint32 (apply #'logior flags)))

(define-db-method db-rename (db file database new-name &rest flags) (rename t)
   (error "not implemented, use env-db-rename"))

(define-db-method db-stat (db &optional txn &rest flags) (stat t)
   (error "not implemented"))

(define-db-method db-stat-print (db &rest flags) (stat_print t)
  (ff-call :uint32 (apply #'logior flags)))

(define-db-method db-sync (db &rest flags) (sync t)
  (ff-call :uint32 (apply #'logior flags)))

(define-db-method db-truncate (db &optional txn &rest flags) (truncate t)
  (with-foreign-object (result :uint32)
	(ff-call :pointer (if txn (txn-pointer txn) (null-pointer)) 
		 :uint32 (apply #'logior flags))
	(mem-aref result :uint32)))

(define-db-method db-upgrade (db file &rest flags) (upgrade t)
    (ff-call :string file :uint32 (apply #'logior flags)))

(define-db-method db-verify (db file &optional database outfile &rest flags) (verify t)
   (error "not implemented"))

;I'd like to use setf here but not sure how
(define-db-method db-set-alloc (db malloc realloc free) (set_alloc t)
   (error "not implemented"))

;I'd like to use setf here but not sure how
(define-db-method (setf db-cache-size) (size nocaches db) (set_cachesize t)
   (error "not implemented"))

(define-db-method db-cache-size (db) (get_cachesize t)
   (with-foreign-objects ((gbytes :uint32)
			  (bytes :uint32)
			  (ncache :int))
	(ff-call :pointer gbytes :pointer bytes :pointer ncache)
	(values (+ (* (expt 2 32) (mem-aref gbytes :uint32)) (mem-aref bytes :uint32)) ncache)))

;make this private, used by (setf compare-fn)
(define-db-method (setf db-dup-compare) (compare-cb db) (set_dup_compare t)
   (ff-call :pointer compare-cb))

(define-db-method db-encrypt (db password flags) (set_encrypt t)
  (ff-call :string password :uint32 (apply #'logior flags)))

(define-db-method db-encrypt-flags (db) (get_encrypt_flags t)
  (with-foreign-object (result :uint)
     (ff-call :pointer result)
     (mem-aref result :uint)))

(define-db-method (setf db-err-call) (err-call-cb db) (set_errcall t)
   (error "not implemented, use (setf env-err-call)"))

(define-db-method (setf db-msg-call) (msg-call-cb db) (set_msgcall t)
   (error "not implemented, use (setf env-msg-call)"))

(define-db-method (setf db-err-file) (err-file db) (set_errfile t)
   (error "not implemented"))

(define-db-method db-err-file (db) (get_errfile t)
   (error "not implemented"))

(define-db-method (setf db-msg-file) (msg-file db) (set_msgfile t)
   (error "not implemented"))

(define-db-method db-msg-file (db) (get_msgfile t)
   (error "not implemented"))

(define-db-method (setf db-error-prefix) (error-prefix db) (set_errpfx)
  (ff-call :string error-prefix))

(define-db-method db-error-prefix (db) (get_errpfx)
  (with-foreign-object (result :string)
	(ff-call :pointer result)
	(mem-aref result :string)))

(define-db-method (setf db-feedback-fcn) (cb db) (set_feedback t)
	(ff-call :pointer cb))

(define-db-method (setf db-lorder) (lorder db) (set_lorder t)
   (let ((lorder (typecase lorder
		  (number lorder)
		  (keyword (case lorder
			     ((:big-endian :big) 4321)
			     ((:little-endian :little) 1234))))))
     (ff-call :int lorder)))

(define-db-method db-lorder (db) (get_lorder t)
   (with-foreign-object (result :int)
	(ff-call :pointer result)
	(ecase (mem-aref result :int)
	  (1234 :little-endian)
	  (4321 :big-endian))))

(define-db-method (setf db-page-size) (pagesize db) (set_pagesize t)
  (ff-call :uint32 pagesize))

(define-db-method db-page-size (db) (get_pagesize t)
   (with-foreign-object (result :uint32)
	(ff-call :pointer result)
	(mem-aref result :uint32)))

(define-db-method (setf append-recno) (callback db) (set_append_recno t)
  (ff-call :pointer callback))

(define-db-method (setf btree-compare) (callback db) (set_bt_compare t)
  (ff-call :pointer callback))

(define-db-method (setf btree-min-key) (minkey db) (set_bt_minkey t)
  (ff-call :uint32 minkey))

(define-db-method btree-min-key (db) (get_bt_minkey t)
   (with-foreign-object (result :uint32)
	(ff-call :pointer result)
	(mem-aref result :uint32)))

(define-db-method (setf btree-prefix) (cb db) (set_bt_prefix t)
   (ff-call :pointer cb))

(define-db-method (setf recno-delim) (delim db) (set_re_delim t)
   (ff-call :char delim ))

(define-db-method recno-delim (db) (get_re_delim t)
   (with-foreign-object (result :char)
	(ff-call :pointer result)
	(mem-aref result :char)))

(define-db-method (setf db-record-length) (len db) (set_re_len t)
   (ff-call :uint32 len))

(define-db-method db-record-length (db) (get_re_len t)
   (with-foreign-object (result :uint32)
	(ff-call :pointer result)
	(mem-aref result :uint32)))

(define-db-method (setf db-pad) (pad db) (set_re_pad t)
   (ff-call :char pad))

(define-db-method db-pad (db) (get_re_pad t)
   (with-foreign-object (result :char)
	(ff-call :pointer result)
	(mem-aref result :char)))

(define-db-method (setf recno-source) (source db) (set_re_source t)
   (ff-call :string source))

(define-db-method recno-source (db) (get_re_source t)
   (with-foreign-object (result :string)
	(ff-call :pointer result)
	(mem-aref result :string)))

(define-db-method (setf hash-ffactor) (ffactor db) (set_h_ffactor t)
   (ff-call :uint32 ffactor))

(define-db-method hash-ffactor (db) (get_h_ffactor t)
   (with-foreign-object (result :uint32)
	(ff-call :pointer result)
	(mem-aref result :uint32)))

(define-db-method (setf hash-hash) (cb db) (set_h_hash t)
	(ff-call :pointer cb))

(define-db-method (setf hash-size) (size db) (set_h_nelem t)
   (ff-call :uint32 size))

(define-db-method hash-size (db) (get_h_nelem t)
   (with-foreign-object (result :uint32)
      (ff-call :pointer result)
      (mem-aref result :uint32)))

(define-db-method (setf queue-extent-size) (size db) (set_q_extentsize t)
   (ff-call :uint32 size))

(define-db-method queue-extent-size (db) (get_q_extentsize t)
   (with-foreign-object (result :uint32)
      (ff-call :pointer result)
      (mem-aref result :uint32)))
