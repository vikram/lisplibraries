;; This software is Copyright (c) 2006, Steffen Siering.
;; <COMPANY> grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :bdb)

;;;; * Secondary Databases
;;;;
;;;; Notes:
;;;; the C library handles secondaries like this:
;;;; db_create: - will create environment, if none specified and
;;;;              add to db pointer
;;;; db_put/__db_put: - create cursor with write-lock
;;;;                  - use __db_c_put on cursor to insert data
;;;; __db_c_put: - uses private flag DB_UPDATE_SECONDARY to do puts on
;;;;               secondary databases
;;;;             - use __db_s_first and __db_s_next for iterating over
;;;;               secondaries, they will take care of thread-locking
;;;;             - in first loop add new keys:
;;;;                     - call callback to get secondary key
;;;;                     - open cursor on secondary with transaction and
;;;;                       lock id of primary database
;;;;                     - use __db_c_put with DB_UPDATE_SECONDARY flag
;;;;                       to insert data into secondary
;;;;                     - close cursor
;;;;             - in second loop remove old secondary indicies:
;;;;                     - get old secondary key
;;;;                     - if secondary key needs to be deleted than:
;;;;                           - get cursor
;;;;                           - use get to move cursor to position
;;;;                           - delete entry
;;;;             - update primary database now
;;;;
;;;;  perl/tcl/python/ruby libs weren't much more helpfull :(
;;;;
;;;;  for the lisp library:
;;;;             - when opening a database it depends on the environment,
;;;;               wether we'll use the C-based
;;;;               implementation or a transaction based. If transactions
;;;;               are supported by environment.
;;;;               Todo so, db-open returns different kind of classes:
;;;;                        - db-std ;; standard db-class, if txn not allowed
;;;;                        - db-txn ;; if environment supports transactions
;;;;                 
;;;;             - db-associate needs different implementations for db-std
;;;;               and db-txn:
;;;;                  - db-std:
;;;;                            - use the old low level method
;;;;                            - use defcallback and go on
;;;;                            - no changes to db-put
;;;;                  - db-txn:
;;;;                            - add callback function and secondary to table
;;;;                              in database instance
;;;;                            - when closing secondary, remove from table
;;;;                            - don't use defcallback, use lambda directly
;;;;                            - overwrite db-put:
;;;;                                       - create an transaction
;;;;                                       - add data to primary
;;;;                                       - in a loop, add keys to secondaries
;;;;
;;;;

;;I consider this... some kind of a hack for being able to use
;;secondary databases (C-style association when using db-std),
;;but callbacks are somewhat ugly when using ffi :(

(defcstruct DBT ;;tried to avoid using DBT directly... :'(
  (data :pointer)
  (size :uint32)
  (ulen :uint32)
  (dlen :uint32)
  (doff :uint32)
  (flags :uint32))

;;dirty, but cffi doesn't support anonymous callbacks :(
(defun create-db-assoc-callback (fun &optional name)
  (let ((callback-fun (if name name (gensym "DB_ASSOC_CALLBACK"))))
      (get-callback
       (eval
	`(defcallback ,callback-fun :int ((secondary :pointer)
					  (key-dbt :pointer)
					  (data-dbt :pointer)
					  (ret-dbt :pointer))
	  (with-foreign-slots ((data size) key-dbt DBT)
	    (let ((key-data data)
		  (key-size size))
	      (with-foreign-slots ((data size) data-dbt DBT)
		(let ((data-data data)
		      (data-size size))
		  (multiple-value-bind (use-data ret-ptr ret-size)
		      (funcall ,fun
			       secondary
			       key-data key-size
			       data-data data-size)
		    (if use-data
			(progn
			  (with-foreign-slots ((data size flags) ret-dbt DBT)
			    (setf data ret-ptr ;;buffer is freed by DB
				  size ret-size
				  flags 1))	;;DB_DBT_APPMALLOC
			  0)
			-30998))))))))))) ;;DB_DONOTINDEX

(defun create-cbuffered-db-assoc-callback (fun &optional name)
  (create-db-assoc-callback
   (lambda (secondary key-ptr key-size data-ptr data-size)
     (multiple-value-bind (use-ret ret-buf)
	 (funcall fun secondary
		  (make-cbuffer-from-pointer key-ptr key-size)
		  (make-cbuffer-from-pointer data-ptr data-size))
       (cond (use-ret (values t (cbuffer-data ret-buf) (cbuffer-size ret-buf)))
	     (ret-buf (free-cbuffer ret-buf)
		      (values nil nil))
	     (t (values nil nil)))))
   name))

(defun make-return-buffer (writer use data)
  (if use
      (let ((ret-buf (alloc-cbuffer)))
	(funcall writer data ret-buf)
	(values t ret-buf))
      (values nil nil)))

(defun build-assoc-callback-maker (buf-writer buf-reader)
  (lambda (fun &optional name)
    (create-cbuffered-db-assoc-callback
     (lambda (secondary key-buf data-buf)
       (multiple-value-call (curry #'make-return-buffer buf-writer)
	 (funcall fun secondary
		    (funcall buf-reader key-buf)
		    (funcall buf-reader data-buf))))
     name)))

(defun build-cbuffered-assoc-lambda (buf-writer buf-reader)
  (lambda (fun)
    (lambda (secondary key-buf data-buf)
      (multiple-value-call (curry #'make-return-buffer buf-writer)
	(funcall fun secondary
		   (funcall buf-reader key-buf)
		   (funcall buf-reader data-buf))))))

(defmethod db-associate ((primary db-std) (secondary db) callback &key
			 txn create (use-cbuffer t) callback-maker callback-name)
  (let ((make-callback (cond ((not (null callback-maker)) callback-maker)
			     (use-cbuffer #'create-cbuffered-db-assoc-callback)
			     (t #'create-db-assoc-callback))))
    (%db-associate primary txn secondary
		   (if (pointerp callback)
		       callback
		       (funcall make-callback callback callback-name))
		   :create create)))

(defmethod db-associate ((primary db-txn) (secondary db-txn) callback
			 &key txn callback-maker create)
  (with-txn (txn (db-get-env primary) :txn txn)
    (%db-dummy-associate primary
			 txn
			 (db-sec-handle (db-open-sec secondary
						     :txn txn))
			 :create create)
    (push (cons secondary
		       (if (functionp callback-maker)
			   (funcall callback-maker callback)
			   callback))
		 (db-assoc primary))
    (setf (db-assoc secondary) primary)
    primary))

(defmethod db-disassociate ((primary db-txn) (secondary db-txn))
  (with-slots (db-assoc) primary
    (setf db-assoc
	  (filter (lambda (arg)
		    (eq (car arg) secondary))
		  db-assoc))))

;;TODO: implement pget and c_pget

(defmethod db-join ((primary db) cursor-seq &key join-item rmw)
  (let ((size (length cursor-seq)))
    (with-foreign-object (cur-list :pointer (1+ size))
      (setf (mem-aref cur-list :pointer size) (null-pointer))
      (loop for i from 0 below size
	    for cursor in cursor-seq
	    do (setf (mem-aref cur-list :pointer i)
		     (cursor-handle cursor)))
      (%db-join primary cur-list
		:join-item join-item
		:rmw rmw))))

;;; pget

(defmethod db-pget (db key &key pkey txn
	       consume consume-wait
	       set-recno
	       multiple rmw
	       not-found)
  (labels ((get-by-buffer (pkey-buf data-buf)
	     (multiple-value-bind (code pkey-ptr pkey-size
					data-ptr data-size)
		 (%db-pget db txn
			   (cbuffer-data key) (cbuffer-size key)
			   (cbuffer-data pkey-buf) (cbuffer-size pkey-buf)
			   (cbuffer-length pkey-buf)
			   (cbuffer-data data-buf) (cbuffer-size data-buf)
			   (cbuffer-length data-buf)
			   :consume consume :consume-wait consume-wait
			   :get-both (not (null pkey)) :set-recno set-recno
			   :multiple multiple :rmw rmw)
	       (handle-get-code
		code
		(lambda () (values (make-cbuffer-from-pointer pkey-ptr pkey-size)
			      (make-cbuffer-from-pointer data-ptr data-size)))
		(lambda ()
		  (free-cbuffer pkey-buf)
		  (free-cbuffer data-buf)
		  (handle-error not-found code))
		(lambda () (get-by-buffer (cbuffer-resize pkey-buf pkey-size)
				     (cbuffer-resize data-buf data-size)))
		(lambda ()
		  (free-cbuffer pkey-buf)
		  (free-cbuffer data-buf)
		  (bdb-check-error code))))))
    (get-by-buffer (make-get-buffer pkey)
		   (make-get-buffer nil))))


(defmethod db-pget ((db db) key &key pkey txn
		   consume consume-wait
		   set-recno
		   multiple rmw
		   not-found)
  (db-pget (db-handle db) key
	  :pkey pkey
	  :txn txn
	  :consume consume
	  :consume-wait consume-wait
	  :set-recno set-recno
	  :multiple multiple
	  :rmw rmw
	  :not-found not-found))

(defmethod db-pget ((db db-txn) key &key pkey txn
		    consume consume-wait
		    set-recno
		    multiple rmw
		    not-found)
  (db-get (if (is-secondary db)
	      (db-sec-handle db)
	      (db-handle db))
	  key
	  :pkey pkey
	  :txn txn
	  :consume consume
	  :consume-wait consume-wait
	  :set-recno set-recno
	  :multiple multiple
	  :rmw rmw
	  :not-found not-found))

(defmethod db-pget ((db db-ext) key &key pkey txn
		   consume consume-wait
		   set-recno
		   multiple rmw
		   not-found)
  (with-cbuffer key-buffer
    (funcall (buf-writer db) key key-buffer)
    (let ((pkey-buffer (when pkey (alloc-cbuffer))))
      (unwind-protect
	   (multiple-value-bind (pkey-buf data-buf)
	       (db-pget (if (is-secondary db)
			   (db-sec-handle db)
			   (db-handle db))
		       key-buffer
		       :pkey (when pkey
			       (funcall (buf-writer db)
					pkey
					pkey-buffer)
			       pkey-buffer)
		       :txn txn
		       :consume consume :consume-wait consume-wait
		       :set-recno set-recno
		       :multiple multiple :rmw rmw
		       :not-found not-found)
	     (let ((pkey-ret nil)
		   (data-ret nil))
	       (unwind-protect
		    (progn
		      (setf pkey-ret (funcall (buf-reader db) pkey-buf))
		      (setf data-ret (funcall (buf-reader db) data-buf)))
		 (free-cbuffer pkey-buf)
		 (free-cbuffer data-buf))
	       (values pkey-ret data-ret)))
	(when pkey-buffer
	  (free-cbuffer pkey-buffer))))))

(defun build-pget-function (write-fn read-fn)
  (lambda (db key &key pkey txn
	   consume consume-wait
	   set-recno
	   multiple rmw
	   not-found)
    (with-cbuffer key-buffer
      (funcall write-fn key key-buffer)
      (let ((pkey-buffer (when pkey (alloc-cbuffer))))
	(unwind-protect
	     (multiple-value-bind (pkey-buf data-buf)
		 (db-get db key-buffer
			 :pkey (when pkey
				 (funcall write-fn pkey pkey-buffer)
				 pkey-buffer)
			 :txn txn
			 :consume consume :consume-wait consume-wait
			 :set-recno set-recno
			 :multiple multiple :rmw rmw
			 :not-found not-found)
	       (let* ((pkey-ret nil)
		      (data-ret nil))
		 (unwind-protect
		      (progn
			(setf pkey-ret (funcall read-fn pkey-buf))
			(setf data-ret (funcall read-fn data-buf)))
		   (free-cbuffer pkey-buf)
		   (free-cbuffer data-buf))
		 (values pkey-ret data-ret)))
	  (when pkey-buffer
	    (free-cbuffer pkey-buffer)))))))
;;----------------------------------------------------------------------
;; db-cursor-pget


(defmethod db-cursor-pget ((cursor cursor) &key key pkey
		      ;;flags
		      current first get-both get-both-range
		      get-recno join-item last
		      next next-dup next-no-dup
		      prev prev-no-dup set set-range set-recno
		      multiple multiple-key rmw not-found)
  (let ((tmp-key-buf (make-get-buffer key))
	(tmp-pkey-buf (make-get-buffer pkey))
	(tmp-data-buf (make-get-buffer nil)))
    (labels ((get-by-buffer (key pkey data)
	       (multiple-value-bind (ret-code key-ptr key-size
					      pkey-ptr pkey-size
					      data-ptr data-size)
		   (%db-cursor-pget cursor
				   (cbuffer-data key) (cbuffer-size key)
				   (cbuffer-length key)
				   (cbuffer-data pkey) (cbuffer-size pkey)
				   (cbuffer-length pkey)
				   (cbuffer-data data) (cbuffer-size data)
				   (cbuffer-length data)
				   :current current :first first
				   :get-both get-both
				   :get-both-range get-both-range
				   :get-recno get-recno :join-item join-item
				   :last last :next next :next-dup next-dup
				   :next-no-dup next-no-dup :prev prev
				   :prev-no-dup prev-no-dup :set set
				   :set-range set-range :set-recno set-recno
				   :multiple multiple
				   :multiple-key multiple-key
				   :rmw rmw)
		 (handle-get-code
		  ret-code
		  (lambda () (values (make-cbuffer-from-pointer key-ptr
							   key-size)
				(make-cbuffer-from-pointer pkey-ptr
							   pkey-size)
				(make-cbuffer-from-pointer data-ptr
							   data-size)))
		  (lambda ()
		    (free-cbuffer key)
		    (free-cbuffer pkey)
		    (free-cbuffer data)
		    (handle-error not-found ret-code))
		  (lambda ()
		    (get-by-buffer (cbuffer-ensure-size key key-size)
				   (cbuffer-ensure-size pkey pkey-size)
				   (cbuffer-ensure-size data data-size)))
		  (lambda ()
		    (free-cbuffer key)
		    (free-cbuffer pkey)
		    (free-cbuffer data)
		    (bdb-check-error ret-code))))))
      (get-by-buffer tmp-key-buf tmp-pkey-buf tmp-data-buf))))

(defmethod db-cursor-pget ((cursor ext-cursor) &key key pkey
			  ;;flags
			  current first get-both get-both-range
			  get-recno join-item last
			  next next-dup next-no-dup
			  prev prev-no-dup set set-range set-recno
			  multiple multiple-key rmw not-found)
  (let ((key-buffer (when key (make-get-buffer nil)))
	(pkey-buffer (when pkey (make-get-buffer nil))))
    (unwind-protect
	 (multiple-value-bind (ret-key ret-pkey ret-data)
	     (call-next-method cursor
			       :key (when key
				      (funcall (buf-writer cursor)
					       key key-buffer)
				      key-buffer)
			       :pkey (when pkey
				       (funcall (buf-writer cursor)
						pkey pkey-buffer)
				       pkey-buffer)
			       :current current :first first
			       :get-both get-both
			       :get-both-range get-both-range
			       :get-recno get-recno :join-item join-item
			       :last last :next next :next-dup next-dup
			       :next-no-dup next-no-dup :prev prev
			       :prev-no-dup prev-no-dup :set set
			       :set-range set-range :set-recno set-recno
			       :multiple multiple
			       :multiple-key multiple-key
			       :rmw rmw :not-found not-found)
	   (unwind-protect
		(values (funcall (buf-reader cursor) ret-key)
			(funcall (buf-reader cursor) ret-pkey)
			(funcall (buf-reader cursor) ret-data))
	     (progn
	       (free-cbuffer ret-key)
	       (free-cbuffer ret-pkey)
	       (free-cbuffer ret-data))))
      (progn
	(when pkey-buffer (free-cbuffer pkey-buffer))
	(when key-buffer (free-cbuffer key-buffer))))))

(defun build-cursor-pget-function (write-fn read-fn)
  (lambda (cursor &key key pkey
	   ;;flags
	   current first get-both get-both-range
	   get-recno join-item last
	   next next-dup next-no-dup
	   prev prev-no-dup set set-range set-recno
	   multiple multiple-key rmw not-found)
    (let ((key-buffer (when key (make-get-buffer nil)))
	  (pkey-buffer (when pkey (make-get-buffer nil))))
      (unwind-protect
	   (multiple-value-bind (ret-key ret-pkey ret-data)
	       (db-cursor-pget cursor
			      :key (when key
				     (funcall write-fn key key-buffer)
				     key-buffer)
			      :pkey (when pkey
				      (funcall write-fn pkey pkey-buffer)
				      pkey-buffer)
			       :current current :first first
			       :get-both get-both
			       :get-both-range get-both-range
			       :get-recno get-recno :join-item join-item
			       :last last :next next :next-dup next-dup
			       :next-no-dup next-no-dup :prev prev
			       :prev-no-dup prev-no-dup :set set
			       :set-range set-range :set-recno set-recno
			       :multiple multiple
			       :multiple-key multiple-key
			       :rmw rmw :not-found not-found)
	     (unwind-protect
		  (values (funcall read-fn ret-key)
			  (funcall read-fn ret-pkey)
			  (funcall read-fn ret-data))
	       (progn
		 (free-cbuffer ret-key)
		 (free-cbuffer ret-pkey)
		 (free-cbuffer ret-data))))
	(progn
	  (when pkey-buffer (free-cbuffer pkey-buffer))
	  (when key-buffer (free-cbuffer key-buffer)))))))
