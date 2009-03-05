;; This software is Copyright (c) 2006, Steffen Siering.
;; <COMPANY> grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :bdb)

;;;; * Constants/Flags

(def-flag-system :bdb-flags)

(use-flagsystem :bdb-flags)

(defun create-def-flag (expr)
  (eval
   `(def-flag* ,(first expr) ,(second expr) ,(third expr))))

(mapcar #'create-def-flag
	(group
	 '(;;bdb_create flags
	   :xa-create DB_XA_CREATE #x0000002
	  
	   ;;db_open flags
	   :auto-commit DB_AUTO_COMMIT #x01000000
	   :create DB_CREATE #x0000001
	   :exclusive DB_EXCL #x0001000
	   :no-mmap DB_NOMMAP #x0000008
	   :read-only DB_RDONLY #x0000010
	   ;(DB_READ_UNCOMMITED) not found in libdb 4.3 maybe in 4.4
	   :threaded DB_THREAD #x0000040
	   :truncate DB_TRUNCATE #x0000080

	   ;;db-close flags
	   :no-sync DB_NOSYNC 23
	  
	   ;;db-put flags
	   :append DB_APPEND 2
	   :no-dup-data DB_NODUPDATA 21
	   :no-overwrite DB_NOOVERWRITE 22

	   ;;db-get flags
	   :consume DB_CONSUME 5
	   :consume-wait DB_CONSUME_WAIT 6
	   :get-both DB_GET_BOTH 10
	   :set-recno DB_SET_RECNO 31
	   :multiple DB_MULTIPLE #x08000000
	   :rmw DB_RMW #x20000000
	   :not-found DB_NOTFOUND -30989
	   
	   ;;db-cursor
	   :write DB_WRITECURSOR 35

	   ;;open_flags
	   :init-cdb DB_INIT_CDB #x0001000
	   :init-lock DB_INIT_LOCK #x0002000
	   :init-log  DB_INIT_LOG #x0004000
	   :init-mpool  DB_INIT_MPOOL #x0008000
	   :init-rep DB_INIT_REP #x0010000
	   :init-txn DB_INIT_TXN #x0020000
	   :recover DB_RECOVER #x0000020
	   :recover-fatal DB_RECOVER_FATAL #x0200000
	   :use-environ DB_USE_ENVIRON #x0000400
	   :use-environ-root DB_USE_ENVIRON_ROOT #x0000800
	   :lockdown DB_LOCKDOWN #x0080000
	   :private DB_PRIVATE #x0100000
	   :system-mem DB_SYSTEM_MEM #x0400000

	   ;;db-env-create
	   :rpc-client DB_RPCCLIENT #x0000001

	   ;;db-env-txn-begin
	   :txn-no-sync DB_TXN_NOSYNC #x0000100
	   :txn-not-durable DB_TXN_NOT_DURABLE #x0000200
	   :txn-no-wait DB_TXN_NOWAIT #x0001000
	   :txn-sync DB_TXN_SYNC #x0002000

	   ;;db-cursor-get
	   :current DB_CURRENT 7
	   :first DB_FIRST 9
	   :get-both-range DB_GET_BOTH_RANGE 12
	   :get-both-c DB_GET_BOTHC 11
	   :get-recno DB_GET_RECNO 13
	   :join-item DB_JOIN_ITEM 14
	   :last DB_LAST 17
	   :next DB_NEXT 18
	   :next-dup DB_NEXT_DUP 19
	   :next-no-dup DB_NEXT_NODUP 20
	   :prev DB_PREV 25
	   :prev-no-dup DB_PREV_NODUP 26
	   :set DB_SET 28
	   :set-range DB_SET_RANGE 30
	   :multiple-key DB_MULTIPLE_KEY #x10000000

	   ;;db-cursor-put
	   :after DB_AFTER 1
	   :key-first DB_KEYFIRST 15
	   :key-last DB_KEYLAST 16
	   :before DB_BEFORE 3

	   ;;db-cursor-dup
	   :position DB_POSITION 24

	   ;;db-associate
	   ;;DB_IMMUTABLE_KEY

	   ;;db-set-flags/db-get-flags
	   ;DB_TXN_DURABLE
	   :checksum DB_CHKSUM #x0000001
	   :encrypt DB_ENCRYPT #x0000008
	   :dup DB_DUP #x0000002
	   :dup-sort DB_DUPSORT #x0000004
	   :recnum DB_RECNUM #x0000020
	   :revsplitoff DB_REVSPLITOFF #x0000080
	   :inorder DB_INORDER #x0000010
	   :renumber DB_RENUMBER #x0000040
	   :snapshot DB_SNAPSHOT #x0000100

	   ;;db-set-encrypt
	   :encrypt-aes DB_ENCRYPT_AES #x0000001

	   ;;db-env-lk-detect
	   :default DB_LOCK_DEFAULT 1
	   :expire DB_LOCK_EXPIRE 2
	   :max-locks DB_LOCK_MAXLOCKS 3
	   :max-write DB_LOCK_MAXWRITE 4
	   :min-locks DB_LOCK_MINLOCKS 5
	   :min-write DB_LOCK_MINWRITE 6
	   :oldest DB_LOCK_OLDEST 7
	   :random DB_LOCK_RANDOM 8
	   :youngest DB_LOCK_YOUNGEST 9

	   ;;txn-timeout
	   :lock-timeout DB_SET_LOCK_TIMEOUT 29
	   :txn-timeout DB_SET_TXN_TIMEOUT 33

	   ;;db-sequ-set-flags
	   :seq-dec DB_SEQ_DEC 1
	   :seq-inc DB_SEQ_INC 2
	   :seq-wrap DB_SEQ_WRAP 8
	   )
	 3))

;;DBTYPES:
(mapcar #'create-def-flag
	(group '(:db-btree DB_BTREE 1
		 :db-hash DB_HASH 2
		 :db-recno DB_RECNO 3
		 :db-queue DB_QUEUE 4
		 :db-unknown DB_UNKNOWN 5)
	       3))

;;;BDB_FILE_MODE:
(mapcar #'create-def-flag
	(group
	 '(:db-file-owner-read FILE_OWNER_READ #o400
	   :db-file-owner-write FILE_OWNER_WRITE #o200
	   :db-file-owner-exec FILE_OWNER_EXEC #o100
	   :db-file-group-read FILE_GROUP_READ #o40
	   :db-file-group-write FILE_GROUP_WRITE #o20
	   :db-file-group-exec FILE_GROUP_EXEC #o10
	   :db-file-others-read FILE_OTHERS_READ #o4
	   :db-file-others-write FILE_OTHERS_WRITE #o2
	   :db-file-others-exec FILE_OTHERS_EXEC #o1
	   :db-default FILE_DEFAULT 0)
	 3))

(defun create-umask (&key
		     db-file-owner-read
		     db-file-owner-write
		     db-file-owner-exec
		     db-file-group-read
		     db-file-group-write
		     db-file-group-exec
		     db-file-others-read
		     db-file-others-write
		     db-file-others-exec)
  (flags :db-file-owner-read db-file-owner-read
	 :db-file-owner-write db-file-owner-write
	 :db-file-owner-exec db-file-owner-exec
	 :db-file-group-read db-file-group-read
	 :db-file-group-write db-file-group-write
	 :db-file-group-exec db-file-group-exec
	 :db-file-others-read db-file-others-read
	 :db-file-others-write db-file-others-write
	 :db-file-others-exec db-file-others-exec))
