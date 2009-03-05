;; This software is Copyright (c) 2006, Steffen Siering.
;; <COMPANY> grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :bdb)

;;;; * Auxilliary Bindings
;;;; (to be used by this library only)

(defcfun "strlen" :int
  (str :pointer))

(defcfun "memcpy" :pointer
  (destination :pointer)
  (source :pointer)
  (length :int))

(defcfun+ ("bdb_create" bdb-create) :int
  (dbp :out :pointer)
  (dbenv :pointer)
  (flags :uint32))

(defcfun+ ("db_open" %db-open) :int
  (dbp :pointer)
  (txn :pointer)
  (file :string)
  (db :string)
  (type :uint32)
  (flags :flag (:auto-commit
		:create
		:exclusive
		:no-mmap
		:read-only
		:threaded
		:truncate))
  (mode :int))

(defmethod+ ("db_close" %db-close) :error-int
  (db :pointer)
  (flags :flag (:no-sync)))

(defcfun+ ("db_get_env" %db-get-env) :pointer
  (db :pointer))

(defmethod+ ("db_cursor_close" %db-cursor-close) :int
  (cursor :pointer :class (cursor cursor-handle)))

(defcfun+ ("db_set_flags" %db-set-flags) :int
  (db :pointer)
  (flags :flag (:checksum :encrypt
		:dup :dup-sort
		:recnum
		:revsplitoff
		:inorder
		:renumber :snapshot)))

(defmethod+ ("db_get_type" %db-get-type) :int
  (db :pointer :class (db db-handle))
  (type :out :uint32))

(defmethod+ ("db_get_open_flags" %db-open-flags) :int
  (db :pointer :class (db db-handle))
  (flagsp :out :uint32))

(defmethod+ ("db_put_raw" %db-put) :int
  (db :pointer)
  (txn :pointer :class txn-handle)
  (key :pointer)
  (key-size :uint32)
  (data :pointer)
  (data-size :uint32)
  (flags :flag (:append
		:no-dup-data
		:no-overwrite)))

(defctype :get-error-int :int)

(defmethod+ ("db_get_raw" %db-get) :get-error-int
  (db :pointer)
  (txn :pointer :class txn-handle)
  (key :pointer)
  (key-size :uint32)
  (data :pointer)
  (data-size :uint32)
  (data-ulen :uint32)
  (flags :flag (:consume
		:consume-wait
		:get-both
		:set-recno
		:multiple
		:rmw))
  (result-buffer :out :pointer)
  (result-size :out :uint32))

(defmethod+ ("db_del" %db-del) :int
  (db :pointer :class (db db-handle))
  (txn :pointer :class txn-handle)
  (key :pointer)
  (key-size :uint32)
  (flags :uint32))

;;db-env

(defcfun+ ("bdb_env_create" bdb-env-create) :int
  (env :out :pointer)
  (flags :flag (:rpc-client)))

(defcfun+ ("db_env_open" %db-env-open) :int
  (env :pointer)
  (db_home :string)
  (flags :flag (:init-cdb :init-log :init-lock
		:init-txn :init-mpool
		:init-rep
		:recover
		:recover-fatal
		:use-environ
		:use-environ-root
		:create :lockdown :private))
  (mode :int))

(defmethod+ ("db_env_close" %db-env-close) :int
  (db-env :pointer :class (db-env db-env-handle))
  (flags :uint32 :const 0))

(defmethod+ ("db_env_get_open_flags" %db-env-get-open-flags) :int
  (env :pointer :class (db-env db-env-handle))
  (flagsp :out :uint32))

;;cursor

(defmethod+ ("db_cursor" %db-cursor) :int
  (db :pointer)
  (txn :pointer :class txn-handle)
  (cursorp :out :pointer)
  (flags :flag (:write)))

(defmethod+ ("db_cursor_get_raw" %db-cursor-get) :get-error-int
  (cursor :pointer :class (cursor cursor-handle))
  (key :pointer)
  (key-size :uint32)
  (key-ulen :uint32)
  (data :pointer)
  (data-size :uint32)
  (data-ulen :uint32)
  (flags :flag (:current :first
		:get-both :get-both-range
		:get-recno :join-item
		:last :next :next-dup :next-no-dup
		:prev :prev-no-dup
		:set :set-range :set-recno
		:multiple :multiple-key
		:rmw))
  (key-ret-ptr :out :pointer)
  (key-ret-size :out :uint32)
  (data-ret-ptr :out :pointer)
  (data-ret-size :out :uint32))

(defmethod+ ("db_cursor_put_raw" %db-cursor-put) :int
  (cursor :pointer :class (cursor cursor-handle))
  (key :pointer)
  (key-size :uint32)
  (data :pointer)
  (data-size :uint32)
  (flags :flag (:after
		:before
		:current
		:key-first
		:key-last
		:no-dup-data)))

(defmethod+ ("db_join" %db-join) :int
  (primary :pointer :class (db db-handle)) ;;database
  (curs_list :pointer) ;;null terminated array with cursor pointers
  (dbcpm :out :pointer)
  (flags :flag (:join-item
		:rmw)))


(defmethod+ ("db_cursor_dup" %db-cursor-dup) :int
  (cursor :pointer :class (cursor cursor-handle))
  (dup_cursor :out :pointer)
  (flags :flag (:position)))

;;config
(defmethod+ ("db_set_pagesize" %db-set-pagesize) :int
  (db :pointer :class (db db-handle))
  (size :uint32))

(defmethod+ ("db_get_pagesize" %db-get-pagesize) :int
  (db :pointer :class (db db-handle))
  (size :out :uint32))

(defmethod+ ("db_get_flags" %db-get-flags) :int
  (db :pointer :class (db db-handle))
  (flags :out :uint32))

(defmethod+ ("db_get_encrypt_flags" %db-get-encrypt-flags) :int
  (db :pointer :class (db db-handle))
  (flags :out :uint32))

(defmethod+ ("db_env_set_lk_max_lockers" %db-env-set-lk-max-lockers) :int
  (dbenv :pointer :class (db-env db-env-handle))
  (max :uint32))

(defmethod+ ("db_env_get_lk_max_lockers" %db-env-get-lk-max-lockers) :int
  (dbenv :pointer :class (db-env db-env-handle))
  (max :out :uint32))

(defmethod+ ("db_env_set_lk_max_locks" %db-env-set-lk-max-locks) :int
  (dbenv :pointer :class (db-env db-env-handle)) 
  (max :uint32))

(defmethod+ ("db_env_get_lk_max_locks" %db-env-get-lk-max-locks) :int
  (dbenv :pointer :class (db-env db-env-handle))
  (max :out :uint32))

(defmethod+ ("db_env_set_lk_max_objects" %db-env-set-lk-max-objects) :int
  (dbenv :pointer :class (db-env db-env-handle))
  (max :uint32))

(defmethod+ ("db_env_get_lk_max_objects" %db-env-get-lk-max-objects) :int
  (dbenv :pointer :class (db-env db-env-handle))
  (max :out :uint32))

(defmethod+ ("db_env_get_lk_detect" %db-env-get-lk-detect) :int
  (dbenv :pointer :class (db-env db-env-handle))
  (detect :out :uint32))

;; txn

(defmethod+ ("db_env_txn_begin" %db-env-txn-begin) :int
  (env :pointer :class (db-env db-env-handle))
  (parent :pointer :class txn-handle) ;;parent is optional
  (tid :out :pointer)
  (flags :flag (:txn-no-sync
		:txn-not-durable
		:txn-no-wait
		:txn-sync)))


(defmethod+ ("db_txn_abort" %db-txn-abort) :int
  (tid :pointer :class txn-handle))

(defmethod+ ("db_txn_commit" %db-txn-commit) :int
  (tid :pointer :class txn-handle)
  (flags :flag (:txn-no-sync :txn-sync)))

;;secondary

(defmethod+ ("db_associate" %db-associate) :int
  (primary :pointer :class (db db-handle))
  (txn :pointer :class txn-handle)
  (secondary :pointer :class (db db-handle))
  (callback :pointer)
  (flags :flag (:create)))

(defmethod+ ("db_dummy_associate" %db-dummy-associate) :int
  (primary :pointer :class (db db-handle))
  (txn :pointer :class txn-handle)
  (secondary :pointer)
  (flags :flag (:create)))

(defmethod+ ("db_pget_raw" %db-pget) :get-error-int
  (db :pointer)
  (txn :pointer :class txn-handle)
  (key :pointer)
  (key-size :uint32)
  (pkey :pointer)
  (pkey-size :uint32)
  (pkey-ulen :uint32)
  (data :pointer)
  (data-size :uint32)
  (data-ulen :uint32)
  (flags :flag (:consume
		:consume-wait
		:get-both
		:set-recno
		:multiple
		:rmw))
  (ret-pkey :out :pointer)
  (ret-pkey-size :out :uint32)
  (ret-data :out :pointer)
  (ret-data-size :out :uint32))


(defmethod+ ("db_cursor_pget_raw" %db-cursor-pget) :get-error-int
  (cursor :pointer :class (cursor cursor-handle))
  (key :pointer)
  (key-size :uint32)
  (key-ulen :uint32)
  (pkey :pointer)
  (pkey-size :uint32)
  (pkey-ulen :uint32)
  (data :pointer)
  (data-size :uint32)
  (data-ulen :uint32)
  (flags :flag (:current :first
		:get-both :get-both-range
		:get-recno :join-item
		:last :next :next-dup :next-no-dup
		:prev :prev-no-dup
		:set :set-range :set-recno
		:multiple :multiple-key
		:rmw))
  (key-ret-ptr :out :pointer)
  (key-ret-size :out :uint32)
  (pkey-ret-ptr :out :pointer)
  (pkey-ret-size :out :uint32)
  (data-ret-ptr :out :pointer)
  (data-ret-size :out :uint32))

;;sequences

(defmethod+ ("bdb_sequence_create" %bdb-sequ-create) :int
  (sequ :out :pointer)
  (db :pointer :class (db db-handle))
  (flags :uint32 :const 0))

(defmethod+ ("db_sequence_open" %db-sequ-open) :int
  (sequ :pointer)
  (txn :pointer :class txn-handle)
  (key :pointer)
  (key-size :uint32)
  (flags :flag (:create :exclusive :threaded)))

(defmethod+ ("db_sequence_set_flags" %db-sequ-set-flags) :int
  (sequ :pointer)
  (flags :flag (:seq-dec :seq-inc :seq-wrap)))

(defmethod+ ("db_sequence_get_flags" %db-sequ-get-flags) :int
  (sequ :pointer :class (db-sequence sequ-handle))
  (flags :out :uint32))

#-cffi-features:no-long-long
(defmethod+ ("db_sequence_init_value" %db-sequ-init-value) :int
  (sequ :pointer)
  (value :int64))

#+cffi-features:no-long-long
(progn
  (defmethod+ ("db_sequence_init_value_i" %%db-sequ-init-value) :int
    (sequ :pointer)
    (lower :uint32)
    (upper :int32))

  (defmethod %db-sequ-init-value (sequ value)
    (%%db-sequ-init-value sequ
			  (ldb (byte 32 0) value)
			  (ldb (byte 32 32) value))))



(defmethod+ ("db_sequence_remove" %db-sequ-remove) :int
  (sequ :pointer :class (db-sequence sequ-handle))
  (txn :pointer :class txn-handle)
  (flags :flag (:txn-no-sync)))

(defmethod+ ("db_sequence_close" %db-sequ-close) :int
  (sequ :pointer)
  (flags :uint32 :const 0))

#-cffi-features:no-long-long
(defmethod+ ("db_sequence_get" %db-sequ-get) :int
  (sequ :pointer :class (db-sequence sequ-handle))
  (txn :pointer :class txn-handle)
  (delta :int32)
  (ret :out :int64)
  (flags :flag (:txn-no-sync)))

;;;%db-sequ-get for systems without longlong support

#+cffi-features:no-long-long
(progn
  (defmethod+ ("db_sequence_get_i" %%db-sequ-get) :int
    (sequ :pointer :class (db-sequence sequ-handle))
    (txn :pointer :class txn-handle)
    (delta :int32)
    (lower :out :uint32)
    (upper :out :int32)
    (flags :flag (:txn-no-sync)))

  (defmethod %db-sequ-get (sequ txn delta &key txn-no-sync)
    (multiple-value-bind (lower upper)
	(%%db-sequ-get sequ txn delta
		       :txn-no-sync txn-no-sync)
      (let ((ret 0))
	(setf (ldb (byte 32 0) ret) lower)
	(setf (ldb (byte 32 32) ret) upper)
	ret))))