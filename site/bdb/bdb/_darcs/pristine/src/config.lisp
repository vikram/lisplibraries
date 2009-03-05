;; This software is Copyright (c) 2006, Steffen Siering.
;; <COMPANY> grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :bdb)

(defmethod db-pagesize ((db db))
  (%db-get-pagesize db))

(defmethod (setf db-pagesize) (size (db db))
  (%db-set-pagesize db size))

(defmethod+ ("db_set_cachesize" db-set-cachesize) :int
  (db :pointer :class (db db-handle))
  (gbyte :uint32)
  (bytes :uint32)
  (ncache :int))

(defmethod+ ("db_get_cachesize" db-get-cachesize) :int
  (db :pointer :class (db db-handle))
  (gbyte :out :uint32)
  (bytes :out :uint32)
  (ncache :out :int))

(defmethod db-get-flags ((db db))
  (let ((val (%db-get-flags db)))
    (with-flag-system (find-flag-system :bdb-flags)
      (flag-list val
		 :flags '(:checksum :encrypt
			  :dup :dup-sort
			  :recnum
			  :revsplitoff
			  :inorder
			  :renumber :snapshot)))))

(defmethod+ ("db_set_encrypt" db-set-encrypt) :int
  (db :pointer :class (db db-handle))
  (pwd :string)
  (flags :flag (:encrypt-aes)))

(defmethod db-encrypt-flags ((db db))
  (let ((val (%db-get-encrypt-flags db)))
    (with-flag-system (find-flag-system :bdb-flags)
      (flag-list val '(:encrypt-aes)))))

(defmethod+ ("db_env_set_cachesize" db-env-set-cachesize) :int
  (db :pointer :class (db-env db-env-handle))
  (gbyte :uint32)
  (bytes :uint32)
  (ncache :int))

(defmethod+ ("db_env_get_cachesize" db-env-get-cachesize) :int
  (db :pointer :class (db-env db-env-handle))
  (gbyte :out :uint32)
  (bytes :out :uint32)
  (ncache :out :int))
