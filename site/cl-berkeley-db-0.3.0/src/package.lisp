;;;Copyright (c) 2006-2007, Arthur Smyles
;;;All rights reserved.
;;;
;;;Redistribution and use in source and binary forms, with or without
;;;modification, are permitted provided that the following conditions are met:
;;;
;;;1. Redistributions of source code must retain the above copyright notice,
;;;   this list of conditions and the following disclaimer.
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;         documentation and/or other materials provided with the distribution.
;;;
;;;         THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;;;         AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;;         IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;;         ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
;;;         LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;;         CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;;;         SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;;         INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;;;         CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;;;         ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;;         POSSIBILITY OF SUCH DAMAGE.
;;;
(defpackage #:cl-berkeley-db.common-lisp.net
  (:use #:cffi #:cl)
  (:shadow #:close #:open #:abort)
  (:nicknames #:bdb)
  ;version
  (:export #:DB_VERSION_MAJOR #:DB_VERSION_MINOR #:DB_VERSION_PATCH)
  ;verbose flags
  (:export #:DB_VERB_DEADLOCK #:DB_VERB_RECOVERY #:DB_VERB_REGISTER #:DB_VERB_REPLICATION #:DB_VERB_WAITSFOR)
  ;flags
  (:export #:DB_CREATE #:DB_RECOVER #:DB_INIT_LOG #:DB_INIT_TXN #:DB_INIT_LOCK #:DB_INIT_MPOOL #:DB_THREAD #:DB_MULTIVERSION #:DB_REGISTER)
  ;dbt.lisp
  (:export #:db-dbt #:data-dbt #:app-dbt #:buffer-dbt #:dbt-data #:dbt-size #:dbt-ulen #:dbt-dlen #:dbt-doff #:dbt-flags)
  ;error.lisp
  (:export #:buffer-small #:do-not-index #:key-empty #:key-exist #:lock-deadlock #:lock-not-granted #:log-buffer-full #:no-server #:no-server-home #:no-server-id #:not-found #:old-version #:page-not-found #:rep-dup-master #:rep-handle-dead #:rep-hold-election #:rep-ignore #:rep-isperm #:rep-join-failure #:rep-lockout #:rep-new-master #:rep-new-site #:rep-not-perm #:rep-unavailable #:run-recovery #:secondary-bad #:verify-bad #:version-mismatch #:db-error)

  ;cursor.lisp
  (:export #:cursor #:db-cursor #:cursor-close #:cursor-count #:cursor-delete #:copy-cursor #:cursor-get #:cursor-pget #:cursor-put)
  ;cursor-get functions
  (:export #:cursor-get-first #:cursor-get-next #:cursor-get-prev #:cursor-get-last #:cursor-get-current #:cursor-get-at #:cursor-get-at-range #:cursor-get-both)
  ;cursor get flags
  (:export #:DB_CURRENT #:DB_FIRST #:DB_GET_BOTH #:DB_GET_BOTH_RANGE #:DB_GET_RECNO #:DB_JOIN_ITEM #:DB_LAST #:DB_NEXT #:DBNEXT_NODUP #:DB_PREV #:DB_PREV_NODUP #:DB_SET #:DB_SET_RANGE #:DB_SET_RECNO #:DB_MULTIPLE #:DB_RMW)
  ;cursor put flags
  (:export #:DB_AFTER #:DB_BEFORE #:DB_KEYFIRST #:DB_KEYLAST #:DB_NODUPDATA)
  ;transaction.lisp
  (:export #:txn #:env-txn-checkpoint #:env-print-txn-stats #:env-timeout #:env-tx-max #:env-tx-timestamp
	   #:env-txn-begin #:txn-abort #:txn-commit #:txn-discard #:txn-id #:txn-prepare #:txn-name #:txn-timeout)
  ;transaction flags
  (:export #:DB_READ_COMMITTED #:DB_READ_UNCOMMITTED #:DB_TXN_NOSYNC #:DB_TXN_NOWAIT #:DB_TXN_SNAPSHOT #:DB_TXN_SYNC #:DB_AUTO_COMMIT)
  ;environment.lisp
  (:export #:env #:env-error-fn #:env-message-fn #:env-event-notify #:env-is-alive #:env-current-thread #:env-thread-name #:env-feedback #:db-event #:panic #:rep-client #:rep-master #:rep-new-master #:rep-startup-done #:write-failed #:env-close #:env-db-remove #:env-db-rename #:env-err #:env-errx #:env-fail-check #:env-fileid-reset #:env-home #:env-open-flags #:env-lsn-reset #:env-open #:env-open* #:env-remove #:env-stat-print #:env-app-dispatch #:env-cache-size #:env-add-data-dir #:env-data-dirs #:env-encrypt #:env-encrypt-flags #:env-error-prefix #:env-flags #:env-rpc-server #:env-shm-key #:env-thread-count #:env-timout #:env-temp-dir #:env-verbose)
  ;database.lisp
  (:export #:db #:btree #:hash #:recno #:queue #:db-associate-fn #:db-data-compare-fn #:db-feedback #:db-append-recno-fn #:btree-key-compare-fn #:btree-key-prefix-fn #:hash-fn #:db-env #:db-associate #:db-close #:db-delete #:db-fd #:db-get #:db-pget #:db-byteswapped #:db-type #:db-join #:db-key-range #:db-open #:db-open* #:db-name #:db-open-flags #:db-transactional #:db-put #:db-remove #:db-stat-print #:db-sync #:db-truncate #:db-upgrade #:db-cache-size #:db-encrypt #:db-encrypt-flags #:db-error-prefix #:db-lorder #:db-page-size #:btree-min-key #:recno-delim #:db-record-length #:db-pad #:recno-source #:hash-ffactor #:hash-size #:queue-extent-size)
  ;utils.lisp
  (:export #:with-transaction #:with-cursor))

(in-package #:cl-berkeley-db.common-lisp.net)
;load the library
(define-foreign-library cl-berkeley-db (t "/usr/lib/libdb-4.6.so"))
(load-foreign-library 'cl-berkeley-db)

;;utility methods used by the other files

;TODO replace with cffi equivalent 
(defun byte-vector (pointer size)
  (unless (null-pointer-p pointer)
  (let ((result (make-array size :element-type '(unsigned-byte 8) :initial-element 0)))
    (sb-kernel:copy-ub8-from-system-area pointer 0 result 0 size)
    result)))

(defmacro define-struct-method (name  (&rest args) (slot type &key cstruct-reader handle-errors) &body body)
  (let ((ff-call 'ff-call)
	(cstruct (if cstruct-reader 
		   (list cstruct-reader (if (atom name)
					  (first args)
					  (second args)))
		   (if (atom name)
		     (first args)
		     (second args))))
	(cargs (if (atom name) args (list (cdr args) (first args)))))

  `(defun ,name ,args
     (declare (ignorable ,@(remove-if (lambda (x) (member x lambda-list-keywords)) 
				      (mapcar (lambda (x) (if (atom x) x (first x))) args))))
       (macrolet ((,ff-call (&rest args)
			    `(,',(if (eq handle-errors t) 'handle-db-error 'progn)
			       (cffi:foreign-funcall-pointer (cffi:foreign-slot-value ,',cstruct ',',type ',',slot)
						   (:cconv :cdecl)
						   :pointer ,',cstruct
						   ,@args 
						   ,',(case handle-errors 
							((t) :int)
							((nil) :void)
							(otherwise handle-errors))))))
	   ,@body))))

(defmacro override (name (&rest args) &body body)
  `(let ((super (function ,name)))
     (defun ,name ,args 
       (flet ((call-next-method (&rest args) (apply super args)))
	 ,@body))))
