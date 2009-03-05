;; This software is Copyright (c) 2006, Steffen Siering.
;; <COMPANY> grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :bdb)

;;;; * Logging

(defmethod+ ("db_env_set_lg_max" db-env-set-lg-max) :int
  (dbenv :pointer :class (db-env db-env-handle))
  (max :uint32))

(defmethod+ ("db_env_get_lg_max" db-env-get-lg-max) :int
  (dbenv :pointer :class (db-env db-env-handle))
  (max :out :uint32))

(defmethod+ ("db_env_set_lg_regionmax" db-env-set-lg-regionmax) :int
  (dbenv :pointer :class (db-env db-env-handle))
  (max :uint32))

(defmethod+ ("db_env_get_lg_regionmax" db-env-get-lg-regionmax) :int
  (dbenv :pointer :class (db-env db-env-handle))
  (max :out :uint32))

(defmethod+ ("db_env_set_lg_bsize" db-env-set-lg-bsize) :int
  (dbenv :pointer :class (db-env db-env-handle))
  (bsize :uint32))

(defmethod+ ("db_env_get_lg_bsize" db-env-get-lg-bsize) :int
  (dbenv :pointer (db-env db-env-handle))
  (bsize :out :uint32))
