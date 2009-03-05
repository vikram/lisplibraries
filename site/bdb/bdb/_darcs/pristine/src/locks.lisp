;; This software is Copyright (c) 2006, Steffen Siering.
;; <COMPANY> grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :bdb)

;;;; * BDB Locking system

(defmethod db-env-max-lockers ((db-env db-env))
  (%db-env-get-lk-max-lockers db-env))

(defmethod (setf db-env-max-lockers) (value (db-env db-env))
  (%db-env-set-lk-max-lockers db-env value))

(defmethod db-env-max-locks ((db-env db-env))
  (%db-env-get-lk-max-locks db-env))

(defmethod (setf db-env-max-locks) (value (db-env db-env))
  (%db-env-set-lk-max-locks db-env value))

(defmethod db-env-max-lk-objects ((db-env db-env))
  (%db-env-get-lk-max-objects db-env))

(defmethod (setf db-env-max-lk-objects) (value (db-env db-env))
  (%db-env-set-lk-max-objects db-env value))

(defmethod+ ("db_env_set_lk_detect" db-env-set-lk-detect) :int
  (dbenv :pointer :class (db-env db-env-handle))
  (detect :flag (:default :expire
		 :max-locks :max-write
		 :min-locks :min-write
		 :oldest :random
		 :youngest)))

(defmethod db-env-get-lk-detect ((db-env db-env))
  (first
   (let ((val (%db-env-get-lk-detect db-env)))
     (with-flag-system (find-flag-system :bdb-flags)
       (flag-list val '(:default :expire
		 :max-locks :max-write
		 :min-locks :min-write
		 :oldest :random
		 :youngest))))))

(defmethod+ ("db_env_lock_detect" db-env-lock-detect) :int
  (env :pointer :class (db-env db-env-handle))
  (flags :uint32 :const 0)
  (atype :flag (:default :expire
		 :max-locks :max-write
		 :min-locks :min-write
		 :oldest :random
		 :youngest))
  (aborted :out :int))
