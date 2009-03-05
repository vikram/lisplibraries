;; This software is Copyright (c) 2006, Steffen Siering.
;; <COMPANY> grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :bdb)

;;;; * Databases

(defun db-open (file &key
		env txn db (mode 0) (type :db-btree)
		;;flags
		auto-commit create exclusive no-mmap
		read-only threaded truncate
		;;db-set-flags flags:
		checksum encrypt dup dup-sort
		recnum revsplitoff inorder
		renumber snapshot)
  "creates and opens a new Database
   params: env = Environment the database shall be opened in
           txn = Transaction
           db = Database in File (optional)
           mode = file mode, use create-umask or 0 for default
           type = :db-btree :db-hash :db-recno or :db-queue
           flags... see BDB documentation or db.h"
  (let ((dbp (bdb-create (when env
			   (slot-value env 'db-env-handle)) 0)))
    (%db-set-flags dbp
		  :checksum checksum
		  :encrypt encrypt
		  :dup dup :dup-sort dup-sort
		  :recnum recnum :revsplitoff revsplitoff
		  :inorder inorder :renumber renumber
		  :snapshot snapshot)
    (%db-open dbp
	      (when (and txn (slot-value txn 'txn-handle))
		(slot-value txn 'txn-handle))
	      file db (flag-value type)
	      mode
	      :auto-commit auto-commit
	      :create create
	      :exclusive exclusive
	      :no-mmap no-mmap
	      :read-only read-only
	      :threaded threaded
	      :truncate truncate)
    (initialize-db-instance env dbp txn)))

(defmethod initialize-db-instance ((env (eql nil)) db-handle txn)
  (declare (ignore env txn))
  (make-instance 'db-std :handle db-handle))

(defmethod initialize-db-instance ((env db-env) db-handle txn)
  (declare (ignore env txn))
  (make-instance (if (txn-support-for-handle db-handle)
		     'db-txn
		     'db-std)
		 :handle db-handle))

(defmethod initialize-db-instance ((env db-ext-env) db-handle txn)
  (declare (ignore txn))
  (when (not (txn-support-for-handle db-handle))
    (%db-close db-handle)
    (error "transaction for this database is not enabled..."))
  (make-instance 'db-ext
		 :handle db-handle
		 :env env))

(defun adjust-secondary-flags (flags)
  flags)

(defun adjust-sec-open-flags (flags)
  (clr-flag flags '(:exclusive :create :truncate :auto-commit)))

(defmethod db-open-sec ((db db-txn) &key txn)
  (let* ((handle (db-handle db))
	 (env (%db-get-env handle))
	 (dbp (bdb-create env 0)))
    (%%db-set-flags dbp (adjust-secondary-flags (%db-get-flags db)))
    (multiple-value-bind (db-file-name db-name)
	(db-dbname db)
      (%%db-open dbp
		 (if txn (txn-handle txn) (null-pointer))
		 (if (not db-file-name) (null-pointer) db-file-name)
		 (if (not db-name) (null-pointer) db-name)
		 (%db-get-type db)
		 (adjust-sec-open-flags (%db-open-flags db))
		 0)
      (setf (db-sec-handle db) dbp)
      db)))

(defun txn-support-for-handle (handle)
  (let ((env (make-instance 'db-env
			    :handle (%db-get-env handle))))
    (if (member :init-txn (db-env-open-flags env))
	t
	nil)))

(defmethod db-open-flags ((db db))
  (let ((val (%db-open-flags db)))
    (with-flag-system (find-flag-system :bdb-flags)
      (flag-list val
		 :flags '(:init-cdb :init-lock :init-log
			  :init-mpool :init-rep :init-txn
			  :recover :recover-fatal
			  :use-environ :use-environ-root
			  :lockdown :private
			  :system-mem :create :auto-commit
			  :read-only :exclusive
			  :threaded)))))

(defmethod db-get-type ((db db))
  (let ((val (%db-get-type db)))
    (with-flag-system (find-flag-system :bdb-flags)
      (flag-list val
		 :flags '(:db-btree
			  :db-hash
			  :db-recno
			  :db-queue
			  :db-unknown)))))

(defmethod db-close ((db db) &key no-sync)
  (%db-close (db-handle db) :no-sync no-sync)
  (setf (slot-value db 'db-handle) nil))

(defmethod db-close ((db db-txn) &key no-sync)
  (when (not (null (db-sec-handle db)))
    (%db-close (db-sec-handle db) :no-sync no-sync))
  (setf (slot-value db 'sec-handle) nil)
  (call-next-method)
  (when-bind assoc (db-assoc db)
    (when (and assoc (not (listp assoc)))
      (db-disassociate assoc db)))
  t)

(defmethod db-get-env ((db db))
  (make-instance 'db-env
		 :handle (%db-get-env (db-handle db))))

;;;db configs

(defmethod+ ("db_get_dbname" db-dbname) :error-int
  (db :pointer :class (db db-handle))
  (filenamemap :out :string)
  (dbnamep :out :string))

;;put and get functions

;;assumes that key and data are cbuffers

(defmethod db-put (db-handle key data
		   &key txn append no-dup-data no-overwrite)
  (%db-put db-handle txn
	  (cbuffer-data key) (cbuffer-size key)
	  (cbuffer-data data) (cbuffer-size data)
	  :append append
	  :no-dup-data no-dup-data
	  :no-overwrite no-overwrite))

(defmethod db-put ((db db) key data &key txn append no-dup-data no-overwrite)
  (db-put (db-handle db) key data
	  :txn txn
	  :append append
	  :no-dup-data no-dup-data
	  :no-overwrite no-overwrite))

(defmethod db-put ((db db-txn) key data &key
		   txn append no-dup-data no-overwrite)
  "callback returns a cbuffer, which will be freed by this put function"
  (let ((env (db-get-env db)))
    (with-txn (txn env :txn txn)
      (db-put (db-handle db) key data
	      :txn txn
	      :append append
	      :no-dup-data no-dup-data
	      :no-overwrite no-overwrite)
      (when-bind assoc (db-assoc db)
	(when (listp assoc)
	  (mapc (rcurry #'update-secondary key data txn) assoc)))
      t)))

(defmethod db-put ((db db-ext) key data &key
		   txn append no-dup-data no-overwrite)
  (with-cbuffer key-buffer
    (with-cbuffer data-buffer
      (funcall (buf-writer db) key key-buffer)
      (funcall (buf-writer db) data data-buffer)
      (with-txn (put-txn (db-get-env db) :txn txn)
	(db-put (db-handle db) key-buffer data-buffer
		:txn put-txn
		:append append
		:no-dup-data no-dup-data
		:no-overwrite no-overwrite)
	(when-bind assoc (db-assoc db)
	  (when (listp assoc)
	    (mapc (rcurry #'ext-update-secondary key data key-buffer put-txn)
		  assoc)))
	t))))

(defun put-into-secondary (db use key data txn)
  (unwind-protect
	 (when use
	   (db-put (db-handle db) key data :txn txn))
      (when key
	(free-cbuffer key))))

(defun update-secondary (db-and-callback key data txn)
  (multiple-value-call (lambda (use buf)
			 (put-into-secondary (car db-and-callback)
					     use buf key txn))
    (funcall (cdr db-and-callback) (car db-and-callback) key data))
  t)

(defun ext-update-secondary (db-and-callback key data key-buffer txn)
  (multiple-value-call
      (lambda (use ret)
	(multiple-value-call (lambda (use buf) (put-into-secondary
					   (car db-and-callback)
					   use buf key-buffer txn))
	  (make-return-buffer (buf-writer (car db-and-callback))
			      use ret)))
    (funcall (cdr db-and-callback) (car db-and-callback) key data)))

(defun build-put-function (write-fn)
  (lambda (db key data &key txn append no-dup-data no-overwrite)
    (with-cbuffer key-buffer
      (with-cbuffer data-buffer
	(funcall write-fn key key-buffer)
	(funcall write-fn data data-buffer)
	(db-put db key-buffer data-buffer
		:txn txn
		:append append
		:no-dup-data no-dup-data
		:no-overwrite no-overwrite)))))

(defun handle-get-code (code
			on-return
			on-not-found
			on-buffer-small
			otherwise)
  (case code
    (0 (funcall on-return))
    (-30989 (funcall on-not-found))
    (-30999 (funcall on-buffer-small))
    (otherwise (funcall otherwise))))

(defun handle-error (type code &optional (error-fun #'bdb-check-error))
    (when type
      (if (functionp type)
	  (funcall type)
	  (funcall error-fun code))))

(defmethod db-get (db key &key data txn
	       consume consume-wait
	       set-recno
	       multiple rmw
	       not-found)
  (let ((tmp-buf (make-get-buffer data)))
    (labels ((get-by-buffer (tmp-buf)
	       (multiple-value-bind (code ptr size)
		   (%db-get db txn
			    (cbuffer-data key) (cbuffer-size key)
			    (cbuffer-data tmp-buf) (cbuffer-size tmp-buf)
			    (cbuffer-length tmp-buf)
			    :consume consume :consume-wait consume-wait
			    :get-both data :set-recno set-recno
			    :multiple multiple :rmw rmw)
		 (handle-get-code
		  code
		  (lambda () (make-cbuffer-from-pointer ptr size)) ;;return
		  (lambda () ;;not-found
		    (free-cbuffer tmp-buf)
		    (handle-error not-found code))
		  (lambda () ;;buffer-small
		    (get-by-buffer (cbuffer-resize tmp-buf size)))
		  (lambda () ;;otherwise
		    (free-cbuffer tmp-buf)
		    (bdb-check-error code))))))
      (get-by-buffer tmp-buf))))

;(defmethod db-get (db key &key data txn
;	       consume consume-wait
;	       set-recno
;	       multiple rmw
;	       not-found)
;  (let ((tmp-buf (make-get-buffer data)))
;    (labels ((get-by-buffer (tmp-buf)
;	       (multiple-value-bind (code ptr size)
;		   (%db-get db txn
;			    (cbuffer-data key) (cbuffer-size key)
;			    (cbuffer-data tmp-buf) (cbuffer-size tmp-buf)
;			    (cbuffer-length tmp-buf)
;			    :consume consume :consume-wait consume-wait
;			    :get-both data :set-recno set-recno
;			    :multiple multiple :rmw rmw)
;		 (case code
;		   (0 (make-cbuffer-from-pointer ptr size))
;		   (-30989 (free-cbuffer tmp-buf) ;;DB_NOTFOUND
;			   (when not-found
;			     (if (functionp not-found)
;				 (funcall not-found)
;				 (bdb-check-error code))))
;		   (-30999 ;;DB_BUFFER_SMALL
;		    (get-by-buffer (cbuffer-resize tmp-buf size)))
;		   (otherwise (free-cbuffer tmp-buf)
;			      (bdb-check-error code))))))
;      (get-by-buffer tmp-buf))))


(defmethod db-get ((db db) key &key data txn
		   consume consume-wait
		   set-recno
		   multiple rmw
		   not-found)
  (db-get (db-handle db) key
	  :data data
	  :txn txn
	  :consume consume
	  :consume-wait consume-wait
	  :set-recno set-recno
	  :multiple multiple
	  :rmw rmw
	  :not-found not-found))

(defmethod db-get ((db db-txn) key &key data txn
		   consume consume-wait
		   set-recno
		   multiple rmw
		   not-found)
  (db-get (if (is-secondary db)
	      (db-sec-handle db)
	      (db-handle db))
	  key
	  :data data
	  :txn txn
	  :consume consume
	  :consume-wait consume-wait
	  :set-recno set-recno
	  :multiple multiple
	  :rmw rmw
	  :not-found not-found))

(defmethod db-get ((db db-ext) key &key data txn
		   consume consume-wait
		   set-recno
		   multiple rmw
		   not-found)
  (with-cbuffer key-buffer
    (funcall (buf-writer db) key key-buffer)
    (let ((data-buffer (when data (alloc-cbuffer))))
      (unwind-protect
	   (let* ((buf (db-get (if (is-secondary db)
				   (db-sec-handle db)
				   (db-handle db))
			       key-buffer
			       :data (when data
				       (funcall (buf-writer db)
						data
						data-buffer)
				       data-buffer)
			       :txn txn
			       :consume consume :consume-wait consume-wait
			       :set-recno set-recno
			       :multiple multiple :rmw rmw
			       :not-found not-found))
		  (ret))
	     (unwind-protect
		  (setf ret (funcall (buf-reader db) buf))
	       (free-cbuffer buf))
	     ret)
	(when data-buffer
	  (free-cbuffer data-buffer))))))

(defun build-get-function (write-fn read-fn)
  (lambda (db key &key data txn
	   consume consume-wait
	   set-recno
	   multiple rmw
	   not-found)
    (with-cbuffer key-buffer
      (funcall write-fn key key-buffer)
      (let ((data-buffer (when data (alloc-cbuffer))))
	(unwind-protect
	     (let* ((buf (db-get db key-buffer
				 :data (when data
					 (funcall write-fn data data-buffer)
					 data-buffer)
				 :txn txn
				 :consume consume :consume-wait consume-wait
				 :set-recno set-recno
				 :multiple multiple :rmw rmw
				 :not-found not-found))
		    (ret))
	       (unwind-protect
		    (setf ret (funcall read-fn buf))
		 (free-cbuffer buf))
	       ret)
	  (when data-buffer
	    (free-cbuffer data-buffer)))))))

(defmethod+ ("db_remove" db-remove) :int
  (db :pointer :class (db db-handle))
  (dbfile :string)
  (dbname :string)
  (flags :uint32 :const 0))

(defmethod+ ("db_rename" db-rename) :int
  (db :pointer :class (db db-handle))
  (dbfile :string)
  (dbname :string)
  (newname :string)
  (flags :uint32 :const 0))

(defmethod db-del ((db db) key &key txn)
  (%db-del db txn (cbuffer-data key) (cbuffer-size key) 0))

(defmethod db-del ((db db-txn) key &key txn)
  (with-txn (txn (db-get-env db) :txn txn)
    (%db-del db txn (cbuffer-data key) (cbuffer-size key) 0)))

(defmethod db-del ((db db-ext) key &key txn)
  (with-cbuffer key-buffer
    (funcall (buf-writer db) key key-buffer)
    (call-next-method db key-buffer :txn txn)))

(defun build-del-function (write-fn)
  (lambda (db key &key txn)
    (with-cbuffer key-buffer
      (funcall write-fn key key-buffer)
      (db-del db key-buffer :txn txn))))

(defmethod+ ("db_truncate" db-truncate) :int
  (db :pointer :class (db db-handle))
  (txn :pointer :key :class txn-handle)
  (countp :out :uint32)
  (flags :uint32 :const 0))

(defmethod+ ("db_sync" db-sync) :int
  (db :pointer :class (db db-handle))
  (flags :uint32 :const 0))