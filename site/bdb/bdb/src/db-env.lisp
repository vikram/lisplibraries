;; This software is Copyright (c) 2006, Steffen Siering.
;; <COMPANY> grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :bdb)

;;;; * Database Environments

(defun db-env-open (db-home &key
		    (mode 0)
		    rpc-client
		    init-cdb init-txn init-log init-lock
		    init-mpool init-rep
		    recover recover-fatal
		    use-environ
		    use-environ-root
		    create lockdown private
		    buf-reader buf-writer
		    )
  (let ((env (bdb-env-create :rpc-client rpc-client)))
    (%db-env-open env db-home mode
		 :init-cdb init-cdb :init-txn init-txn
		 :init-log init-log :init-lock init-lock
		 :init-mpool init-mpool :init-rep init-rep
		 :recover recover :recover-fatal recover-fatal
		 :use-environ use-environ :use-environ-root use-environ-root
		 :create create :lockdown lockdown :private private)
    (init-environment (if (and (functionp buf-reader )
			    (functionp buf-writer))
		       'db-ext-env
		       'db-env)
		      env buf-reader buf-writer)))


(defmethod init-environment ((class (eql 'db-env)) handle
			     buf-reader buf-writer)
  (declare (ignore buf-reader buf-writer))
  (make-instance class :handle handle))

(defmethod init-environment ((class (eql 'db-ext-env)) handle
			     buf-reader buf-writer)
  (make-instance class
		 :handle handle
		 :buf-reader buf-reader
		 :buf-writer buf-writer))

(defmethod db-env-close ((db-env db-env))
  (%db-env-close db-env)
  (setf (slot-value db-env 'db-env-handle) nil))

(defmethod+ ("db_env_get_home" db-env-get-home) :int
  (env :pointer :class (db-env db-env-handle))
  (homep :out :string))

(defmethod db-env-open-flags ((env db-env))
  (let ((val (%db-env-get-open-flags env)))
    (with-flag-system (find-flag-system :bdb-flags)
      (flag-list val
		 :flags '(:init-cdb :init-lock :init-log
			  :init-mpool :init-rep :init-txn
			  :recover :recover-fatal
			  :use-environ :use-environ-root
			  :lockdown :private :auto-commit
			  :system-mem :create :threaded)))))

(defmethod+ ("db_env_set_tmp_dir" db-env-set-tmp-dir) :int
  (dbenv :pointer :class (db-env db-env-handle))
  (dir :string))

(defmethod+ ("db_env_get_tmp_dir" db-env-get-tmp-dir) :int
  (dbenv :pointer :class (db-env db-env-handle))
  (dir :out :string))
