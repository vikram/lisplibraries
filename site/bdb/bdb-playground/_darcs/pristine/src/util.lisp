;; This software is Copyright (c) 2006, Steffen Siering.
;; <COMPANY> grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :bdb-playground)

;;;; * Database functions

(setf (symbol-function 'buf-writer)
      (make-cbuffer-writer #'cl-store:store))

(setf (symbol-function 'buf-reader)
      (make-cbuffer-reader #'cl-store:restore))

(setf (symbol-function 'db-put)
      (build-put-function #'buf-writer))

(setf (symbol-function 'db-get)
      (build-get-function #'buf-writer #'buf-reader))

(setf (symbol-function 'db-del)
      (build-del-function #'buf-writer))

(setf (symbol-function 'db-cursor-get)
      (build-cursor-get-function #'buf-writer #'buf-reader))

(setf (symbol-function 'db-cursor-put)
      (build-cursor-put-function #'buf-writer))

(setf (symbol-function 'create-assoc-callback)
      (build-assoc-callback-maker #'buf-writer #'buf-reader))

(setf (symbol-function 'create-assoc-lambda)
      (build-cbuffered-assoc-lambda #'buf-writer #'buf-reader))

(defmethod db-associate ((primary db-std) (secondary db-std) callback
			 &key txn create)
  (bdb::db-associate primary secondary
		     (if (cffi::pointerp callback)
			 callback
			 (create-assoc-callback callback))
		     :txn txn :create create))

(defmethod db-associate ((primary db-txn) (secondary db-txn) callback
			 &key txn create)
  (bdb::db-associate primary secondary
		     (create-assoc-lambda callback)
		     :txn txn
		     :create create))