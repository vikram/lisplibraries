
(in-package :bdb)

;;;; ** Not yet supported/implemented Bindings
 
(defcfun ("db_get_transactional" db_get_transactional) :int
  (db :pointer))

;TODO:
(defcfun ("db_pget" db_pget) :int
  (db :pointer)
  (txn :pointer)
  (key :pointer)
  (pkey :pointer)
  (data :pointer)
  (flags :pointer))

;TODO:
(defcfun ("db_cursor_pget" db_cursor_pget) :int
  (cursor :pointer)
  (key :pointer)
  (pkey :pointer)
  (data :pointer)
  (flags :pointer))


;TODO
(defcfun ("db_set_errcall" db_set_errcall) :void
  (db :pointer)
  (db_errcall_fcn :pointer))

(defcfun "db_set_errpfx" :void
  (db :pointer)
  (errpfx :string))

(defcfun* ("db_get_errpfx" db-get-errpfx) :void
  (db :pointer)
  (errpfx :out :string))

;TODO
(defcfun ("db_env_set_errcall" db_env_set_errcall) :void
  (db :pointer)
  (db_errcall_fcn :pointer))

(defcfun "db_env_set_errpfx" :void
  (db :pointer)
  (errpfx :string))

(defcfun+ ("db_env_get_errpfx" db-env-get-errpfx) :void
  (db :pointer)
  (errpfx :out :string))

;TODO
(defcfun ("db_env_txn_recover" db_env_txn_recover) :int
  (dbenv :pointer)
  (preplist :pointer)
  (count :long)
  (ret :pointer)
  (flags :pointer))


;TODO
(defcfun ("db_env_log_archive" db_env_log_archive) :int
  (dbenv :pointer)
  (listp :pointer)
  (flags :pointer))

;TODO
(defcfun ("db_env_log_flush" db_env_log_flush) :int
  (dbenv :pointer)
  (lsn :pointer))

;;helpers
