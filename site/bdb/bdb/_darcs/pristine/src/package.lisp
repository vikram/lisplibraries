;; This software is Copyright (c) 2006, Steffen Siering.
;; <COMPANY> grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :cl-user)

;;;; * Package definition

(defpackage bdb
  (:use :cl :cffi :mycl-util :cffi-util)
  (:export
   ;;classes
   #:db-env
   #:db
   #:txn
   #:db-std
   #:db-txn
   #:is-primary
   #:is-secondary

   ;;bdb
   #:db-strerr
   #:bdb-version-major
   #:bdb-version-minor
   #:bdb-version-patch
   #:bdb-cersion-string
   #:bdb-version

   ;;config
   #:db-pagesize
   #:db-set-cachesize
   #:db-get-cachesize
   #:db-set-flags
   #:db-get-flags
   #:db-set-encrypt
   #:db-encrypt-flags
   #:db-env-set-cachesize
   #:db-env-get-cachesize

   ;;consts
   #:create-umask

   ;;cursor
   #:db-cursor
   #:db-cursor-close
   #:db-cursor-get
   #:build-cursor-get-function
   #:db-cursor-put
   #:build-cursor-put-function
   #:db-cursor-del
   #:db-cursor-count
   #:db-cursor-dup

   ;;db-env
   #:db-env-open
   #:db-env-close
   #:db-env-get-home
   #:db-env-open-flags
   #:db-env-set-tmp-dir
   #:db-env-get-tmp-dir

   ;;db
   #:db-open
   #:db-open-flags
   #:db-close
   #:db-get-env
   #:db-dbname
   #:db-put
   #:db-get-type
   #:build-put-function
   #:db-get
   #:build-get-function
   #:db-remove
   #:db-rename
   #:db-del
   #:build-del-function
   #:db-truncate
   #:db-sync

   ;;locks
   #:db-env-max-lockers
   #:db-env-max-locks
   #:db-env-max-lk-objects
   #:db-env-set-lk-detect
   #:db-env-get-lk-detect
   #:db-env-lock-detect

   ;;log
   #:db-env-set-lg-max
   #:db-env-get-lg-max
   #:db-env-set-lg-regionmax
   #:db-env-get-lg-regionmax
   #:db-env-set-lg-bsize
   #:db-env-get-lg-bsize

   ;;secondary
   #:db-associate
   #:db-join
   #:build-assoc-callback-maker
   #:build-cbuffered-assoc-lambda
   #:db-pget
   #:db-cursor-pget

   ;;txn
   #:db-env-txn-begin
   #:db-txn-abort
   #:db-txn-commit
   #:db-txn-id
   #:db-env-set-tx-max
   #:db-env-get-tx-max
   #:db-env-txn-checkpoint
   #:db-txn-set-timeout
   #:db-env-set-timeout
   #:db-env-get-timeout
   #:with-txn

   ;;sequences
   #:db-sequ-open
   #:db-sequ-close
   #:db-sequ-get
   #:db-sequ-get-flags
   #:db-sequ-remove
   #:db-sequ-get-cachesize
   #:db-sequ-set-cachesize
   
   ;;util
   #:make-cbuffer-writer
   #:make-cbuffer-reader))

(in-package :bdb)

(define-foreign-library bdb
  (t "libbdb.so"))

(load-foreign-library 'bdb)

(defun bdb-check-error (code)
      (if (= code 0)
      t
      (error "error-code: ~A~%error-str: ~A"
	     code
	     (foreign-funcall "db_strerr" :int code :string))))

(defmacro defcfun+ ((name fun-name) ret-type &body args)
  `(defcfun* (,name ,fun-name :error-fun bdb-check-error)
      ,(if (eq ret-type :int) :error-int ret-type)
    ,@args))

(defmacro defmethod+ ((name fun-name) ret-type &body args)
  `(defcfun* (,name ,fun-name :error-fun bdb-check-error :method t)
    ,(if (eq ret-type :int) :error-int ret-type)
    ,@args))
