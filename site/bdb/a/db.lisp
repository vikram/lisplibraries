;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: BERKELEY-DB -*-

;;; $Revision: 1.10 $
;;; Copyright © 2001 Paul Foley (mycroft@actrix.gen.nz)
;;; All rights reserved.  Use and verbatim redistribution permitted.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE
;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
;;; OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
;;; USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
;;; DAMAGE.
#+CMU (ext:file-comment "$Header: /cvsroot/homepage/db/db.lisp,v 1.10 2004/07/06 06:30:43 anoncvs Exp $")

(in-package #:berkeley-db)

(eval-when (:compile-toplevel :load-toplevel :execute)

(defconstant +db-cursor-put-modes+
  `((:after     .  ,DB_AFTER)
    (:before    .  ,DB_BEFORE)
    (:current   .  ,DB_CURRENT)
    (:keyfirst  .  ,DB_KEYFIRST)
    (:keylast   .  ,DB_KEYLAST)
    (:nodup     .  ,DB_NODUPDATA)))

(defconstant +db-cursor-get-modes+
  `((:current          .  ,DB_CURRENT)
    (:first            .  ,DB_FIRST)
    (:last             .  ,DB_LAST)
    (:get-both         .  ,DB_GET_BOTH)
    (:get-both-range   .  ,DB_GET_BOTH_RANGE)
    (:get-recno        .  ,DB_GET_RECNO)
    (:join-item        .  ,DB_JOIN_ITEM)
    (:next             .  ,DB_NEXT)
    (:prev             .  ,DB_PREV)
    (:next-dup         .  ,DB_NEXT_DUP)
    (:next-nodup       .  ,DB_NEXT_NODUP)
    (:prev-nodup       .  ,DB_PREV_NODUP)
    (:set              .  ,DB_SET)
    (:set-range        .  ,DB_SET_RANGE)
    (:set-recno        .  ,DB_SET_RECNO)))

(defconstant +db-lock-detect-modes+
  `((:default   . ,DB_LOCK_DEFAULT)
    (:expire    . ,DB_LOCK_EXPIRE)
    (:maxlocks  . ,DB_LOCK_MAXLOCKS)
    (:minlocks  . ,DB_LOCK_MINLOCKS)
    (:minwrite  . ,DB_LOCK_MINWRITE)
    (:oldest    . ,DB_LOCK_OLDEST)
    (:random    . ,DB_LOCK_RANDOM)
    (:youngest  . ,DB_LOCK_YOUNGEST)))

(defconstant +db-ciphers+
  `((:unknown   . 0)
    (:AES       . ,DB_ENCRYPT_AES)
    (:Rijndael  . ,DB_ENCRYPT_AES)))

) ; EVAL-WHEN

(define-condition db-error (error)
  ((errno :initarg :errno :reader db-error-errno))
  (:report
    (lambda (condition stream)
      (declare (type db-error condition) (type stream stream))
      (format stream "Berkeley DB error ~A"
	      (%db-strerror (db-error-errno condition))))))

(define-condition db-old-version-error (db-error)
  ()
  (:default-initargs :errno #.DB_OLD_VERSION))

(define-condition db-deadlock-error (db-error)
  ()
  (:default-initargs :errno #.DB_LOCK_DEADLOCK))

;; Note: currently not a subclass of DB-ERROR
(define-condition db-fatal-error (error)
  ()
  ;;(:default-initargs :errno #.DB_RUNRECOVERY)
  (:report
    (lambda (condition stream)
      (declare (ignore condition) (type stream stream))
      (format stream "Berkeley DB fatal error: run database recovery"))))


(defstruct (environment
	     (:conc-name DB-ENV-)
	     (:print-function
	      (lambda (object stream depth)
		(declare (type environment object) (type stream stream)
			 (ignore depth))
		(print-unreadable-object (object stream :type t :identity t)
		  (if (null (DB-ENV-%sap object))
		    (format stream "closed")
		    (format stream "~:[(no home)~;~:*~S~]"
			    (DB-ENV-home object))))))
	     (:constructor %%make-DB-ENV (%sap)))
  (%sap (required-argument) :type (or null pointer))
  (home nil :type (or null string))
  (plist nil :type list))

(defstruct (transaction
	     (:conc-name DB-TXN-)
	     (:print-function
	      (lambda (object stream depth)
		(declare (type transaction object) (type stream stream)
			 (ignore depth))
		(print-unreadable-object (object stream :type t :identity t)
		  (format stream "~:[closed~;open~]" (DB-TXN-%sap object)))))
	     (:constructor %%make-DB-TXN (%sap)))
  (%sap (required-argument) :type (or null pointer))
  (%dbs nil :type list)
  (%tag nil :type symbol))

(defstruct (DB
	     (:print-function
	      (lambda (object stream depth)
		(declare (type DB object) (type stream stream) (ignore depth))
		(print-unreadable-object (object stream :type t :identity t)
		  (if (null (DB-%sap object))
		    (format stream "closed")
		    (format stream "~@[~S in ~]~:[(no database)~;~:*~S~]"
			    (DB-database object)
			    (DB-file object))))))
	     (:constructor %%make-DB (%sap)))
  (%sap (required-argument) :type (or null pointer))
  (%txn nil :type (or null transaction))
  (file nil :type (or null string))
  (database nil :type (or null string))
  (plist nil :type list))

(defstruct (cursor
	     (:print-function
	      (lambda (object stream depth)
		(declare (type cursor object) (type stream stream)
			 (ignore depth))
		(print-unreadable-object (object stream :type t :identity t)
		  (format stream "~:[closed~;open~]" (cursor-%sap object)))))
	     (:constructor %%make-cursor (%sap %txn)))
  (%sap (required-argument) :type (or null pointer))
  (%txn (required-argument) :type (or null transaction)))

(declaim (type (or null environment) *environment*)
	 (type (or null transaction) *transaction*)
	 (type hash-table *cleanup*))

(defvar *environment* nil)
(defvar *transaction* nil)
(defvar *cleanup* (make-hash-table))

(declaim (inline cleanup DB-sap cursor-sap DB-ENV-sap DB-TXN-sap %make-DB
		 %make-cursor %make-DB-ENV %make-DB-TXN
		 environment-plist (setf environment-plist)))

(defun cleanup (sap)
  (mapc (lambda (fn) (ignore-errors (funcall fn)))
	(gethash (sap-int sap) *cleanup*))
  (remhash (sap-int sap) *cleanup*))

(defun DB-sap (db)
  (declare (type DB db))
  (DB-%sap db))

(defun cursor-sap (cursor)
  (declare (type cursor cursor))
  (cursor-%sap cursor))

(defun DB-ENV-sap (env)
  (declare (type (or null environment) env))
  (if env (DB-ENV-%sap env) +null+))

(defun DB-TXN-sap (txn)
  (declare (type (or null transaction) txn))
  (if txn (DB-TXN-%sap txn) +null+))

(defun %make-DB (sap)
  (declare (type pointer sap))
  (let ((db (%%make-DB sap)))
    (finalize db (lambda ()
		   (warn "Closing lost database")
		   (%db-close sap 0)
		   (cleanup sap)
		   nil))
    db))

(defun %make-cursor (sap txn)
  (declare (type pointer sap) (type (or null transaction) txn))
  (let ((cursor (%%make-cursor sap txn)))
    (finalize cursor (lambda ()
		       (warn "Closing lost cursor")
		       (%db-cursor-close sap)
		       (cleanup sap)
		       nil))
    cursor))

(defun %make-DB-ENV (sap)
  (declare (type pointer sap))
  (let ((env (%%make-DB-ENV sap)))
    (finalize env (lambda ()
		    (warn "Closing lost environment")
		    (%db-env-close sap 0)
		    (cleanup sap)
		    nil))
    env))

(defun %make-DB-TXN (sap)
  (declare (type pointer sap))
  (let ((txn (%%make-DB-TXN sap)))
    (finalize txn (lambda ()
		    (warn "Aborting lost transaction")
		    (%db-txn-abort sap)
		    (cleanup sap)
		    nil))
    txn))

(defun environment-plist (env)
  (declare (type environment env))
  (DB-ENV-plist env))

(defun (setf environment-plist) (plist env)
  (declare (type environment env))
  (setf (DB-ENV-plist env) plist))



(defun db-create-environment (&key (flags 0))
  (declare (type (unsigned-byte 32) flags))
  (multiple-value-bind (errno sap)
      (%db-env-create flags)
    (case errno
      (0 (%make-DB-ENV sap))
      (#.DB_RUNRECOVERY (error 'db-fatal-error))
      (otherwise (error 'db-error :errno errno)))))

(defun db-open-environment (environment home &key create (mode #o640)
						  (init-lock t) (init-log t)
						  (init-mpool t) (init-txn t)
						  init-cdb
						  recover
						  (flags 0))
  (declare (type environment environment)
	   (type (or string stream pathname) home)
	   (type (integer 0 #o777) mode)
	   (type (unsigned-byte 32) flags))
  (setq flags (logior flags
		      #+Allegro DB_PRIVATE
		      (if create DB_CREATE 0)
		      (if init-lock DB_INIT_LOCK 0)
		      (if init-log DB_INIT_LOG 0)
		      (if init-mpool DB_INIT_MPOOL 0)
		      (if init-txn DB_INIT_TXN 0)
		      (if init-cdb DB_INIT_CDB 0)
		      (if (eq recover :recover-fatal) DB_RECOVER_FATAL 0)
		      (if recover DB_RECOVER 0)))
  (let ((errno (%db-env-open (DB-ENV-sap environment) (namestring home)
			     flags mode)))
    (case errno
      (0 (setf (DB-ENV-home environment) home)
	 t)
      (#.DB_RUNRECOVERY (error 'db-fatal-error))
      (otherwise (error 'db-error :errno errno)))))

(defun db-remove-environment (environment home &key (flags 0))
  (declare (type environment environment)
	   (type (or string stream pathname) home)
	   (type (unsigned-byte 32) flags))
  (unless (DB-ENV-home environment)
    (cancel-finalization environment)
    (let ((errno (%db-env-remove (DB-ENV-sap environment) (namestring home)
				 flags)))
      (case errno
	(0 (setf (DB-ENV-%sap environment) nil)
	   t)
	(#.DB_RUNRECOVERY (error 'db-fatal-error))
	(otherwise (error 'db-error :errno errno))))))

(defun db-create (&key (environment *environment*) (flags 0))
  (declare (type (or null environment) environment)
	   (type (unsigned-byte 32) flags))
  (multiple-value-bind (errno sap)
      (%db-create (DB-ENV-sap environment) flags)
    (case errno
      (0 (%make-DB sap))
      (#.DB_RUNRECOVERY (error 'db-fatal-error))
      (otherwise (error 'db-error :errno errno)))))

(defun db-open (db file &optional database
			&key (type :unknown) create read-only (mode #o640)
			     (transaction *transaction*) auto-commit (flags 0))
  (declare (type DB db)
	   (type (or string stream pathname) file)
	   (type (or null string) database)
	   (type (integer 0 #o777) mode)
	   (type (or null transaction) transaction)
	   (type (unsigned-byte 32) flags))
  (when create
    (setq flags (logior flags DB_CREATE))
    (when (eq create :excl)
      (setq flags (logior flags DB_EXCL))))
  (when read-only
    (setq flags (logior flags DB_RDONLY)))
  (when auto-commit
    (setq flags (logior flags DB_AUTO_COMMIT)))
  (let ((errno (%db-open (DB-sap db) (DB-TXN-sap transaction)
			 (namestring file) database type flags mode)))
    (case errno
      (0 (setf (DB-file db) (namestring file))
	 (setf (DB-database db) database)
	 (when transaction
	   (setf (DB-%txn db) transaction))
	 t)
      (#.DB_OLD_VERSION (error 'db-old-version-error))
      (#.DB_RUNRECOVERY (error 'db-fatal-error))
      (otherwise (error 'db-error :errno errno)))))

(defun db-remove (db file &optional database &key (transaction *transaction*)
						  auto-commit (flags 0))
  (declare (type (or DB environment) db)
	   (type (or string stream pathname) file)
	   (type (or null string) database)
	   (type (or null transaction) transaction)
	   (type (unsigned-byte 32) flags))
  (when auto-commit (setq flags (logior flags DB_AUTO_COMMIT)))
  (let (errno)
    (if (environment-p db)
      (setq errno (%db-env-dbremove (DB-ENV-sap db) (DB-TXN-sap transaction)
				    (namestring file) database flags))
      (unless (DB-file db)
	(cancel-finalization db)
	(setq errno (%db-remove (DB-sap db) (namestring file)
				database flags))
	(setf (DB-%sap db) nil)))
    (case errno
      (0 t)
      (#.DB_RUNRECOVERY (error 'db-fatal-error))
      (otherwise (error 'db-error :errno errno)))))

(defun db-rename (db file name-or-db &optional name
				     &key (transaction *transaction*)
					  auto-commit (flags 0))
  (declare (type (or DB environment) db)
	   (type (or string stream pathname) file)
	   (type (or string stream pathname) name-or-db)
	   (type (or null string) name)
	   (type (or null transaction) transaction)
	   (type (unsigned-byte 32) flags))
  (when auto-commit (setq flags (logior flags DB_AUTO_COMMIT)))
  (let ((database (if name name-or-db nil))
	(new-name (if name name (namestring name-or-db)))
	errno)
    (if (environment-p db)
      (setq errno (%db-env-dbrename (DB-ENV-sap db) (DB-TXN-sap transaction)
				    (namestring file) database new-name
				    flags))
      (unless (DB-file db)
	(cancel-finalization db)
	(setq errno (%db-rename (DB-sap db) (namestring file) database
				new-name flags))
	(setf (DB-%sap db) nil)))
    (case errno
	(0 t)
	(#.DB_RUNRECOVERY (error 'db-fatal-error))
	(otherwise (error 'db-error :errno errno)))))

(defun db-set-flags (db flags)
  (declare (type DB db) (type (unsigned-byte 32) flags))
  (let ((errno (%db-set-flags (DB-sap db) flags)))
    (case errno
      (0 t)
      (#.DB_RUNRECOVERY (error 'db-fatal-error))
      (otherwise (error 'db-error :errno errno)))))

(defun db-cursor (db &key (transaction *transaction*) (flags 0))
  (declare (type (or DB cursor) db) (type (or null transaction) transaction)
	   (type (unsigned-byte 32) flags))
  (if (cursor-p db)
    (db-copy-cursor db :position t)
    (multiple-value-bind (errno sap)
	(%db-cursor (DB-sap db) (DB-TXN-sap transaction) flags)
      (case errno
	(0 (%make-cursor sap transaction))
	(#.DB_LOCK_DEADLOCK (if (and transaction (DB-TXN-%tag transaction))
			      (throw (DB-TXN-%tag transaction)
				(DB-TXN-%tag transaction))
			      (error 'db-deadlock-error)))
	(#.DB_RUNRECOVERY (error 'db-fatal-error))
	(otherwise (error 'db-error :errno errno))))))

(defun db-copy-cursor (cursor &key position (flags 0))
  (declare (type cursor cursor) (type (unsigned-byte 32) flags))
  (when position (setq flags (logior flags DB_POSITION)))
  (multiple-value-bind (errno sap)
      (%db-cursor-dup (cursor-sap cursor) flags)
    (case errno
      (0 (%make-cursor sap (cursor-%txn cursor)))
      (#.DB_RUNRECOVERY (error 'db-fatal-error))
      (otherwise (error 'db-error :errno errno)))))

(defun db-close (handle &key (flags 0))
  (declare (type (or cursor environment DB) handle)
	   (type (unsigned-byte 32) flags))
  (let ((errno (etypecase handle
		 (cursor (if (cursor-%sap handle)
			   (prog1 (%db-cursor-close (cursor-%sap handle))
			     (cancel-finalization handle)
			     (cleanup (cursor-%sap handle))
			     (setf (cursor-%sap handle) nil))
			   0))
		 (environment (if (DB-ENV-%sap handle)
				(prog1 (%db-env-close (DB-ENV-%sap handle)
						      flags)
				  (cancel-finalization handle)
				  (cleanup (DB-ENV-%sap handle))
				  (setf (DB-ENV-%sap handle) nil))
				0))
		 (DB (if (DB-%sap handle)
		       (if (and (DB-%txn handle)
				(DB-TXN-%sap (DB-%txn handle)))
			 (progn (push handle (DB-TXN-%dbs (DB-%txn handle))) 0)
			 (prog1 (%db-close (DB-%sap handle) flags)
			   (cancel-finalization handle)
			   (cleanup (DB-%sap handle))
			   (setf (DB-%sap handle) nil)))
		       0)))))
    (case errno
      (0 t)
      (#.DB_RUNRECOVERY (error 'db-fatal-error))
      (otherwise (error 'db-error :errno errno)))))

(defun db-put (db key datum &key (start 0) end db-start db-end
				 (transaction *transaction*) auto-commit
				 (flags 0))
  (declare (type (or DB cursor) db)
	   (type string key datum)
	   (type fixnum start) (type (or null fixnum) end)
	   (type (or null (unsigned-byte 32)) db-start db-end)
	   (type (or null transaction) transaction)
	   (type (unsigned-byte 32) flags))
  (when (null end)
    (setq end (- (length datum) start)))
  (when (and db-start (null db-end))
    (setq db-end (+ db-start (- end start))))
  (when auto-commit
    (setq flags (logior flags DB_AUTO_COMMIT)))
  (if (cursor-p db)
    (db-cursor-put db :nodup key datum :start start :end end
		   :db-start db-start :db-end db-end :flags flags)
    (multiple-value-bind (errno recno)
	(%db-put (DB-sap db) key 0 (length key) datum start end
		 (DB-TXN-sap transaction)
		 (if (or db-start db-end) 1 0)
		 (or db-start 0) (or db-end 0)
		 flags)
      (case errno
	(0 (if (zerop (logand flags DB_APPEND))
	     (values datum key)
	     (values datum recno)))
	(#.DB_KEYEXIST nil)
	(#.DB_LOCK_DEADLOCK (if (and transaction (DB-TXN-%tag transaction))
			      (throw (DB-TXN-%tag transaction)
				(DB-TXN-%tag transaction))
			      (error 'db-deadlock-error)))
	(#.DB_RUNRECOVERY (error 'db-fatal-error))
	(otherwise (error 'db-error :errno errno))))))

(defun db-append (db datum &key (start 0) end (transaction *transaction*)
				(flags 0))
  (declare (type DB db)
	   (type string datum)
	   (type fixnum start) (type (or null fixnum) end)
	   (type (or null transaction) transaction)
	   (type (unsigned-byte 32) flags))
  (nth-value 1
    (db-put db "" datum :start start :end end :transaction transaction
	    :flags (logior flags DB_APPEND))))

(defun db-get (db key &optional datum default
		      &key (start 0) end db-start db-end for-update
			   (transaction *transaction*) (flags 0))
  (declare (type (or DB cursor) db)
	   (type string key) (type (or string null) datum)
	   (type fixnum start) (type (or null fixnum) end)
	   (type (or null (unsigned-byte 32)) db-start db-end)
	   (type (or null transaction) transaction)
	   (type (unsigned-byte 32) flags))
  (when for-update (setq flags (logior flags DB_RMW)))
  (if (cursor-p db)
    (multiple-value-bind (datum key size)
	(db-cursor-get db :set key datum default :start start :end end
		       :db-start db-start :db-end db-end
		       :flags flags)
      (declare (ignore key))
      (values datum size))
    (multiple-value-bind (errno sap size)
	(%db-get (DB-sap db) key 0 (length key) datum (if datum start 0)
		 (if datum (or end (- (length datum) start)) 0)
		 (DB-TXN-sap transaction)
		 (if (or db-start db-end) 1 0)
		 (or db-start 0) (or db-end 0) flags)
      (case errno
	(0 (if datum
	     (values datum size)
	     (if (zerop size)
	       (values "" 0)
	       (values (naturalize-string sap size) size))))
	(#.DB_NOTFOUND (values default nil))
	(#.DB_LOCK_DEADLOCK (if (and transaction (DB-TXN-%tag transaction))
			      (throw (DB-TXN-%tag transaction)
				(DB-TXN-%tag transaction))
			      (error 'db-deadlock-error)))
	(#.DB_RUNRECOVERY (error 'db-fatal-error))
	(otherwise (error 'db-error :errno errno))))))

(defun db-cursor-put (cursor mode datum-or-key &optional datum
			     &key (start 0) end db-start db-end (flags 0))
  (declare (type cursor cursor) (type symbol mode)
	   (type string datum-or-key) (type (or null string) datum)
	   (type fixnum start) (type (or null fixnum) end)
	   (type (or null (unsigned-byte 32)) db-start db-end)
	   (type (unsigned-byte 32) flags))
  (let ((mode (cdr (assoc mode +db-cursor-put-modes+)))
	(key (if datum datum-or-key nil))
	(datum (or datum datum-or-key)))
    (declare (type (or null (unsigned-byte 32)) mode))
    (unless mode (error "Invalid cursor-put mode."))
    (when (null end)
      (setq end (- (length datum) start)))
    (when (and db-start (null db-end))
      (setq db-end (+ db-start (- end start))))
    (let ((errno (%db-cursor-put (cursor-sap cursor) key 0 (length key)
				 datum start end
				 (if (or db-start db-end) 1 0)
				 (or db-start 0) (or db-end 0)
				 (logior mode flags))))
      (case errno
	(0 datum)
	(#.DB_KEYEXIST nil)
	(#.DB_LOCK_DEADLOCK (let ((transaction (cursor-%txn cursor)))
			      (if (and transaction (DB-TXN-%tag transaction))
				(throw (DB-TXN-%tag transaction)
				  (DB-TXN-%tag transaction))
				(error 'db-deadlock-error))))
	(#.DB_RUNRECOVERY (error 'db-fatal-error))
	(otherwise (error 'db-error :errno errno))))))

(defun db-cursor-get (cursor mode &optional key datum default
			     &key (start 0) end db-start db-end for-update
				  (flags 0))
  (declare (type cursor cursor) (type symbol mode)
	   (type (or null string) key datum)
	   (type fixnum start) (type (or null fixnum) end)
	   (type (or null (unsigned-byte 32)) db-start db-end)
	   (type (unsigned-byte 32) flags))
  (when for-update (setq flags (logior flags DB_RMW)))
  (let ((mode (cdr (assoc mode +db-cursor-get-modes+))))
    (declare (type (or null (unsigned-byte 32)) mode))
    (unless mode (error "Invalid cursor-get mode."))
    (multiple-value-bind (errno dout dlen kout klen)
	(%db-cursor-get (cursor-sap cursor) key 0 (length key) datum
			(if datum start 0)
			(if datum (or end (- (length datum) start)) 0)
			(if (or db-start db-end) 1 0)
			(or db-start 0) (or db-end 0)
			(logior mode flags))
      (case errno
	(0 (let ((rd "") (rk ""))
	     (cond (datum (setq rd datum))
		   ((zerop dlen) nil)
		   (t (setq rd (naturalize-string dout dlen))))
	     (cond ((zerop klen) nil)
		   (t (setq rk (naturalize-string kout klen))))
	     (values rd rk dlen)))
	(#.DB_NOTFOUND (values default nil))
	(#.DB_LOCK_DEADLOCK (let ((transaction (cursor-%txn cursor)))
			      (if (and transaction (DB-TXN-%tag transaction))
				(throw (DB-TXN-%tag transaction)
				  (DB-TXN-%tag transaction))
				(error 'db-deadlock-error))))
	(#.DB_RUNRECOVERY (error 'db-fatal-error))
	(otherwise (error 'db-error :errno errno))))))

(defun db-key-exists (db key &key (transaction *transaction*) (flags 0))
  (declare (type DB db) (type string key)
	   (type (or null transaction) transaction)
	   (type (unsigned-byte 32) flags))
  (let ((errno (%db-get (DB-sap db) key 0 (length key) nil 0 0
			(DB-TXN-sap transaction) 1 0 0 flags)))
    (case errno
      (0 t)
      (#.DB_NOTFOUND nil)
      (#.DB_LOCK_DEADLOCK (if (and transaction (DB-TXN-%tag transaction))
			    (throw (DB-TXN-%tag transaction)
			      (DB-TXN-%tag transaction))
			    (error 'db-deadlock-error)))
      (#.DB_RUNRECOVERY (error 'db-fatal-error))
      (otherwise (error 'db-error :errno errno)))))

(defun db-count (cursor &key (flags 0))
  (declare (type cursor cursor) (type (unsigned-byte 32) flags))
  (multiple-value-bind (errno count)
      (%db-cursor-count (cursor-sap cursor) flags)
    (case errno
      (0 count)
      (#.DB_RUNRECOVERY (error 'db-fatal-error))
      (otherwise (error 'db-error :errno errno)))))

(defun db-delete (db key &key (transaction *transaction*) auto-commit
			      (flags 0))
  (declare (type (or DB cursor) db) (type string key)
	   (type (or null transaction) transaction)
	   (type (unsigned-byte 32) flags))
  (when auto-commit (setq flags (logior flags DB_AUTO_COMMIT)))
  (let ((errno (etypecase db
		 (cursor (%db-cursor-del (cursor-sap db) flags))
		 (DB (%db-del (DB-sap db) key 0 (length key)
			      (DB-TXN-sap transaction) flags)))))
    (case errno
      (0 t)
      (#.DB_NOTFOUND nil)
      (#.DB_LOCK_DEADLOCK (if (and transaction (DB-TXN-%tag transaction))
			    (throw (DB-TXN-%tag transaction)
			      (DB-TXN-%tag transaction))
			    (error 'db-deadlock-error)))
      (#.DB_RUNRECOVERY (error 'db-fatal-error))
      (otherwise (error 'db-error :errno errno)))))

(defun db-clear (db &key (transaction *transaction*) auto-commit (flags 0))
  (declare (type DB db) (type (or null transaction) transaction)
	   (type (unsigned-byte 32) flags))
  (when auto-commit (setq flags (logior flags DB_AUTO_COMMIT)))
  (multiple-value-bind (errno count)
      (%db-truncate (DB-sap db) (DB-TXN-sap transaction) flags)
    (case errno
      (0 count)
      (#.DB_LOCK_DEADLOCK (if (and transaction (DB-TXN-%tag transaction))
			    (throw (DB-TXN-%tag transaction)
			      (DB-TXN-%tag transaction))
			    (error 'db-deadlock-error)))
      (#.DB_RUNRECOVERY (error 'db-fatal-error))
      (otherwise (error 'db-error :errno errno)))))

(defun db-sync (db &key (flags 0))
  (declare (type DB db) (type (unsigned-byte 32) flags))
  (let ((errno (%db-sync (DB-sap db) flags)))
    (case errno
      (0 t)
      (#.DB_RUNRECOVERY (error 'db-fatal-error))
      (otherwise (error 'db-error :errno errno)))))

(defun db-checkpoint (&key (environment *environment*) (kbytes 0) (minutes 0)
			   (flags 0))
  (declare (type (or null environment) environment)
	   (type (unsigned-byte 32) kbytes minutes flags))
  (let ((errno (%db-checkpoint (DB-ENV-sap environment) kbytes minutes flags)))
    (case errno
      (0 t)
      (#.DB_RUNRECOVERY (error 'db-fatal-error))
      (otherwise (error 'db-error :errno errno)))))

(defun db-lock-detect (atype &key (environment *environment*) (flags 0))
  (declare (type symbol atype) (type (or null environment) environment)
	   (type (unsigned-byte 32) flags))
  (multiple-value-bind (errno aborted)
      (%db-lock-detect (DB-ENV-sap environment) flags
		       (cdr (assoc atype +db-lock-detect-modes+)))
    (case errno
      (0 aborted)
      (#.DB_RUNRECOVERY (error 'db-fatal-error))
      (otherwise (error 'db-error :errno errno)))))

(defun db-set-deadlock-detection (atype &key (environment *environment*))
  (declare (type symbol atype) (type (or null environment) environment))
  (let ((mode (cdr (assoc atype +db-lock-detect-modes+))))
    (declare (type (or null (unsigned-byte 32)) mode))
    (unless mode (error "Invalid lock-detect mode."))
    (let ((errno (%db-set-lk-detect (DB-ENV-sap environment) mode)))
      (case errno
	(0 t)
	(#.DB_RUNRECOVERY (error 'db-fatal-error))
	(otherwise (error 'db-error :errno errno))))))

(defun db-set-encryption (db password &key (cipher :AES) (flags 0))
  (declare (type (or DB environment) db)
	   (type string password) (type symbol cipher)
	   (type (unsigned-byte 32) flags))
  (let ((cipher (cdr (assoc cipher +db-ciphers+))))
    (declare (type (or null (unsigned-byte 32)) cipher))
    (unless cipher (error "Invalid cipher."))
    (let ((errno (if (environment-p db)
		   (%db-env-set-encrypt (DB-ENV-sap db) password
					(logior flags cipher))
		   (%db-set-encrypt (DB-sap db) password
				    (logior flags cipher)))))
      (case errno
	(0 t)
	(#.EOPNOTSUPP nil)
	(#.DB_RUNRECOVERY (error 'db-fatal-error))
	(otherwise (error 'db-error :errno errno))))))

(defun db-add-data-directories (environment dirs)
  (declare (type environment environment))
  (flet ((add-dir (dir)
	   (declare (type (or string stream pathname) dir))
	   (let ((errno (%db-set-data-dir (DB-ENV-sap environment)
					  (namestring dir))))
	     (case errno
	       (0 t)
	       (#.DB_RUNRECOVERY (error 'db-fatal-error))
	       (otherwise (error 'db-error :errno errno))))))
    (if (listp dirs)
      (mapc #'add-dir dirs)
      (add-dir dirs)))
  t)

(defun db-set-log-directory (environment dir)
  (declare (type environment environment)
	   (type (or string stream pathname) dir))
  (let ((errno (%db-set-log-dir (DB-ENV-sap environment) (namestring dir))))
    (case errno
      (0 t)
      (#.DB_RUNRECOVERY (error 'db-fatal-error))
      (otherwise (error 'db-error :errno errno)))))

(defun db-set-tmp-directory (environment dir)
  (declare (type environment environment)
	   (type (or string stream pathname) dir))
  (let ((errno (%db-set-tmp-dir (DB-ENV-sap environment) (namestring dir))))
    (case errno
      (0 t)
      (#.DB_RUNRECOVERY (error 'db-fatal-error))
      (otherwise (error 'db-error :errno errno)))))


(defun db-begin-transaction (&optional (parent *transaction*)
				       &key (environment *environment*)
					    (flags 0))
  (declare (type (or null transaction) parent)
	   (type (or null environment) environment)
	   (type (unsigned-byte 32) flags))
  (multiple-value-bind (errno sap)
      (%db-txn-begin (DB-ENV-sap environment) (DB-TXN-sap parent) flags)
    (case errno
      (0 (%make-DB-TXN sap))
      (#.DB_RUNRECOVERY (error 'db-fatal-error))
      (otherwise (error 'db-error :errno errno)))))

(defun db-commit-transaction (&optional (transaction *transaction*)
					&key (flags 0))
  (declare (type transaction transaction)
	   (type (unsigned-byte 32) flags))
  (let ((errno (%db-txn-commit (DB-TXN-sap transaction) flags)))
    (cancel-finalization transaction)
    (setf (DB-TXN-%sap transaction) nil)
    (dolist (db (DB-TXN-%dbs transaction))
      (setf (DB-%txn db) nil)
      (db-close db))
    (setf (DB-TXN-%dbs transaction) nil)
    (case errno
      (0 t)
      (#.DB_RUNRECOVERY (error 'db-fatal-error))
      (otherwise (error 'db-error :errno errno)))))

(defun db-abort-transaction (&optional (transaction *transaction*))
  (declare (type transaction transaction))
  (let ((errno (%db-txn-abort (DB-TXN-sap transaction))))
    (cancel-finalization transaction)
    (setf (DB-TXN-%sap transaction) nil)
    (dolist (db (DB-TXN-%dbs transaction))
      (setf (DB-%txn db) nil)
      (db-close db))
    (setf (DB-TXN-%dbs transaction) nil)
    (case errno
      (0 t)
      (#.DB_RUNRECOVERY (error 'db-fatal-error))
      (otherwise (error 'db-error :errno errno)))))


(defmacro with-environment ((&optional home &key create (mode #o640)
						 password (cipher :aes)
						 (init-lock t) (init-log t)
						 (init-mpool t) (init-txn t)
						 init-cdb
						 recover
						 deadlock-detection
						 (flags 0))
			    &body body)
  (let ((btmp (gensym))
	(temp (gensym))
	(dtmp (gensym))
	(ptmp (gensym)))
    `(let* ((,btmp ,home)
	    (,temp (if (environment-p ,btmp) ,btmp (db-create-environment)))
	    (,dtmp ,deadlock-detection)
	    (,ptmp ,password)
	    (*environment* ,temp))
       (unwind-protect
	    (progn
	      (when (and ,btmp (not (environment-p ,btmp)))
		(when ,dtmp
		  (db-set-deadlock-detection ,dtmp))
		(when ,ptmp
		  (db-set-encryption ,temp ,ptmp :cipher ,cipher))
		(db-open-environment ,temp ,btmp :create ,create :mode ,mode
				     :init-lock ,init-lock :init-log ,init-log
				     :init-mpool ,init-mpool
				     :init-txn ,init-txn :init-cdb ,init-cdb
				     :recover ,recover :flags ,flags))
	      ,@body)
	 (unless (environment-p ,btmp) (db-close ,temp))))))

(defmacro with-database ((name &optional (file nil file-given) database
			       &key (environment nil env-given)
				    (type :unknown) create read-only
				    (mode #o640)
				    (transaction '*transaction* txn-given)
				    auto-commit
				    password (cipher :aes) encrypt
				    allow-duplicates
				    allow-btree-record-numbers
				    (coalesce-empty-pages t)
				    renumber snapshot
				    (flags 0))
			 &body body)
  (let ((btmp (gensym))
	(temp (gensym))
	(dbflags (let ((int 0)
		       (vars nil))
		   (declare (type (unsigned-byte 32) int))
		   (if (or password encrypt)
		     (setq int (logior int DB_ENCRYPT)))
		   (cond ((null allow-duplicates))
			 ((eq allow-duplicates :sorted)
			  (setq int (logior int DB_DUPSORT)))
			 ((constantp allow-duplicates)
			  (setq int (logior int DB_DUP)))
			 (t
			  (push `(cond ((null allow-duplicates) 0)
				       ((eq allow-duplicates :sorted)
					,DB_DUPSORT)
				       (t ,DB_DUP))
				vars)))
		   (cond ((null coalesce-empty-pages)
			  (setq int (logior int DB_REVSPLITOFF)))
			 ((constantp coalesce-empty-pages))
			 (t (push `(if (not coalesce-empty-pages)
				     ,DB_REVSPLITOFF 0)
				  vars)))
		   (dolist (x `((,allow-btree-record-numbers . ,DB_RECNUM)
				(,renumber . ,DB_RENUMBER)
				(,snapshot . ,DB_SNAPSHOT)))
		     (cond ((null (car x)))
			   ((constantp (car x))
			    (setq int (logior int (the (unsigned-byte 32)
						    (cdr x)))))
			   (t
			    (push 'q vars))))
		   (cond (vars (if (zerop int)
				 vars
				 `(logior ,int ,vars)))
			 (t int)))))
    (when (eq file :environment)
      (setq environment database  env-given t
	    file nil  database nil  file-given nil))
    `(let* ((,btmp ,file)
	    (,temp (if (db-p ,btmp)
		     ,btmp
		     (db-create :environment ,(if env-given
						  environment
						  '*environment*))))
	    (,name ,temp))
       (unwind-protect
	    (progn
	      ,@(when password
		  `((db-set-encryption ,temp ,password :cipher ,cipher)))
	      ,@(unless (zerop dbflags)
		  `((db-set-flags ,temp ,dbflags)))
	      ,@(when file-given
		  `((db-open ,temp ,btmp ,database :type ,type :create ,create
			     :read-only ,read-only :mode ,mode :flags ,flags
			     :auto-commit ,auto-commit
			     :transaction ,transaction)))
	      (locally ,@body))
	 (unless (db-p ,btmp) (db-close ,temp))))))

(defmacro with-databases ((&rest databases) &body body)
  `(with-database ,(first databases)
     ,@(if (rest databases)
	 `((with-databases ,(rest databases) ,@body))
	 body)))

(defmacro with-cursor ((name db &key (transaction nil txn-given) (flags 0))
		       &body body)
  (let ((temp (gensym)))
    `(let* ((,temp (db-cursor ,db
		    :transaction ,(if txn-given transaction '*transaction*)
		    :flags ,flags))
	    (,name ,temp))
       (unwind-protect
	    (locally ,@body)
	 (db-close ,temp)))))

(defmacro with-transaction ((&optional (parent t)
				       &key (environment '*environment*)
					    retry (flags 0))
			    &body body)
  (let ((temp (gensym))
	(retries (gensym)))
    `(let ((,retries 0))
       (block with-transaction
	 (tagbody
	  again
	    (when (eq ',temp (catch ',temp
			       (let* ((,temp (db-begin-transaction
					      ,(if (eq parent t)
						 '*transaction*
						 parent)
					      :environment ,environment
					      :flags ,flags))
				      (*transaction* ,temp))
				 (return-from with-transaction
				   (unwind-protect
					(multiple-value-prog1
					    (progn
					      ,@(when retry
						  `((setf (DB-TXN-%tag ,temp)
							  ',temp)))
					      ,@body)
					  (db-commit-transaction ,temp))
				     (when (DB-TXN-%sap ,temp)
				       (db-abort-transaction ,temp)))))))
	      (if (= (incf ,retries) 100)
		(error "Too many retries.")
		(go again))))))))


(defun mapdb (function db &optional keys)
  (declare (type function function)
	   (type (or DB cursor) db)
	   (type list keys))
  (let ((result '()))
    (if keys
      (dolist (key keys)
	(let ((datum (db-get db key)))
	  (push (funcall function key datum) result)))
      (with-cursor (cursor db)
	(loop
	  (multiple-value-bind (datum key) (db-cursor-get cursor :next)
	    (if (null key)
	      (return)
	      (push (funcall function key datum) result))))))
    (nreverse result)))

(defmacro do-database ((key datum db &optional return) &body body)
  (let ((cursor (gensym)))
    `(with-cursor (,cursor ,db)
       (loop
	 (multiple-value-bind (,datum ,key) (db-cursor-get ,cursor :next)
	   (when (null ,key)
	     (return ,return))
	   (let ((,key ,key) (,datum ,datum))
	     ,@body))))))

#+CMU
(defun loop-database-iteration-path (variable data-type pps &key which)
  (when (cdr pps)
    (ansi-loop::loop-error "Only expecting one prepositional phrase here."))
  (unless (member (caar pps) '(:of :in))
    (ansi-loop::loop-error "Unknown preposition: ~S." (cadr pps)))
  (unless (or (null data-type) (subtypep data-type 'string))
    (ansi-loop::loop-error "Invalid DB-entry datatype: ~S." data-type))
  (let* ((cursor (ansi-loop::loop-gentemp 'loop-cursor-))
	 (data-type (if data-type `(or null ,data-type)))
	 (other (ansi-loop::named-variable (if (eq which 'db-key)
					     'db-value
					     'db-key)))
	 (key (if (eq which 'db-key) variable other))
	 (datum (if (eq which 'db-key) other variable)))
    (multiple-value-bind (txn txn-p)
	(ansi-loop::named-variable 'transaction)
      (push `(db-close ,cursor) ansi-loop::*loop-epilogue*)
      `(((,key nil ,data-type)
	 (,datum nil ,data-type)
	 (,cursor (db-cursor ,(cadar pps) ,@(if txn-p
					      `(:transaction ,txn)))))
	()
	()
	((,datum ,key) (multiple-value-list (db-cursor-get ,cursor :next)))
	(null ,key)
	()))))

#+CMU
(ansi-loop::add-loop-path '(db-key db-keys)
			  'loop-database-iteration-path
			  ansi-loop::*loop-ansi-universe*
			  :preposition-groups '((:of :in))
			  :inclusive-permitted nil
			  :user-data '(:which db-key))

#+CMU
(ansi-loop::add-loop-path '(db-value db-values)
			  'loop-database-iteration-path
			  ansi-loop::*loop-ansi-universe*
			  :preposition-groups '((:of :in))
			  :inclusive-permitted nil
			  :user-data '(:which db-value))

#+LispWorks
(defun loop-database-iteration-path (name variable data-type pps inc all-preps d
ata)
  (declare (ignore inc all-preps))
  (when (cdr pps)
    (error "Only expecting one prepositional phrase here"))
  (unless (or (null data-type) (subtypep data-type 'string))
    (error "Invalid DB-entry datatype: ~S." data-type))
  (let* ((db (cdr (loop::get-prep '(loop::of loop::in) pps)))
         (cursor (gensym "LOOP-CURSOR-"))
         (data-type (if data-type `(or null ,data-type)))
         (other (lw:named-variable (car data)))
         (key (if (or (eq name 'loop::db-key)
                      (eq name 'loop::db-keys))
                variable
                other))
         (datum (if (or (eq name 'loop::db-key)
                        (eq name 'loop::db-keys))
                  other
                  variable)))
    (multiple-value-bind (txn txn-p)
        (and (assoc 'loop::transaction loop::*using-variables*)
             (values (lw:named-variable 'transaction) t))
      (values nil
              `((with-cursor (,cursor ,db
                                      ,@(and txn-p `(:transaction ,txn)))))
              `((,key nil ,data-type)
                (,datum nil ,data-type))
              nil
              `((null (progn
                        (setf (values ,datum ,key)
                              (db-cursor-get ,cursor :next))
                        ,key)))))))

#+LispWorks
(lw:define-loop-method (loop::db-key loop::db-keys)
                       loop-database-iteration-path
                       (:of :in)
                       loop::db-value)

#+LispWorks
(lw:define-loop-method (loop::db-value loop::db-values)
                       loop-database-iteration-path
                       (:of :in)
                       loop::db-key)

(provide :berkeley-db)
