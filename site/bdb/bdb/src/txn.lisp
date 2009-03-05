;; This software is Copyright (c) 2006, Steffen Siering.
;; <COMPANY> grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :bdb)

;;;; * Transactions

;;transactions

(defmethod db-env-txn-begin ((env db-env) &key parent
			     no-sync not-durable no-wait sync)
  (make-instance 'txn
		 :handle (%db-env-txn-begin env parent
					    :txn-no-sync no-sync
					    :txn-not-durable not-durable
					    :txn-no-wait no-wait
					    :txn-sync sync)))

(defmethod db-txn-abort ((txn txn))
  (%db-txn-abort txn)
  (setf (slot-value txn 'txn-handle) nil)
  t)

(defmethod db-txn-commit ((txn txn) &key no-sync sync)
  (%db-txn-commit txn :txn-no-sync no-sync :txn-sync sync)
  (setf (slot-value txn 'txn-handle) nil)
  t)

(defmacro in-txn ((txn &key no-sync sync do-commit) &body body)
  (with-gensyms (success return)
    (once-only (txn do-commit)
      `(let ((,return nil)
	     (,success nil))
	(unwind-protect
	     (progn
	       (setq ,return (multiple-value-list
			      (progn
				,@body)))
	       (when ,do-commit
		 (db-txn-commit ,txn
				:no-sync ,no-sync
				:sync ,sync))
	       (setq ,success t)
	       (apply #'values ,return))
	  (if (and ,do-commit
		   (not ,success))
	      (db-txn-abort ,txn)))))))

(defmacro with-txn ((txn-var env &key parent txn
			     no-sync not-durable no-wait sync)
		    &body body)
  (once-only (env txn parent no-sync not-durable no-wait sync)
    `(let ((,txn-var (if ,txn
			 ,txn
			 (db-env-txn-begin ,env
					   :parent ,parent
					   :no-sync ,no-sync
					   :not-durable ,not-durable
					   :no-wait ,no-wait
					   :sync ,sync))))
      (in-txn (,txn-var :no-sync ,no-sync
			:sync ,sync
			:do-commit (null ,txn))
	,@body))))

(defmethod+ ("db_txn_id" db-txn-id) :uint32
  (txn :pointer :class (txn txn-handle)))

(defmethod+ ("db_env_set_tx_max" db-env-set-tx-max) :int
  (dbenv :pointer :class (db-env db-env))
  (max :uint32))

(defmethod+ ("db_env_get_tx_max" db-env-get-tx-max) :int
  (dbenv :pointer :class (db-env db-env))
  (max :out :uint32))

(defmethod+ ("db_env_txn_checkpoint" db-env-txn-checkpoint) :int
  (env :pointer :class (db-env db-env))
  (kbyte :uint32)
  (min :uint32)
  (flags :uint32))

(defmethod+ ("db_txn_set_timeout" db-txn-set-timeout) :int
  (tid :pointer :class (txn txn-handle))
  (timeout :uint32)
  (flags :flag (:lock-timeout :txn-timeout)))


(defmethod+ ("db_env_set_timeout" db-env-set-timeout) :int
  (dbenv :pointer :class (db-env db-env))
  (timeout :uint32)
  (flags :flag (:lock-timeout :txn-timeout)))

(defmethod+ ("db_env_get_timeout" db-env-get-timeout) :int
  (dbenv :pointer :class (db-env db-env))
  (timeout :out :uint32)
  (flags :flag (:lock-timeout :txn-timeout)))
