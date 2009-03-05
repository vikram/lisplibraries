
(in-package #:cl-berkeley-db.common-lisp.net)





(cl:defconstant DB_VERSION_MAJOR 4)

(cl:defconstant DB_VERSION_MINOR 6)

(cl:defconstant DB_VERSION_PATCH 21)

;(cl:defconstant DB_VERSION_STRING "Berkeley DB 4.6.21: (September 27, 2007)")

(cffi:defctype db_seq_t :pointer)

(cffi:defctype db_threadid_t :unsigned-long)

(cffi:defctype db_pgno_t :unsigned-int)

(cffi:defctype db_indx_t :unsigned-short)

(cl:defconstant DB_MAX_PAGES #xffffffff)

(cffi:defctype db_recno_t :unsigned-int)

(cl:defconstant DB_MAX_RECORDS #xffffffff)

(cffi:defctype db_timeout_t :unsigned-int)

(cffi:defctype roff_t :unsigned-int)

(cffi:defctype DB :pointer)

(cffi:defctype DB_BTREE_STAT :pointer)

(cffi:defctype DB_CIPHER :pointer)

(cffi:defctype DB_COMPACT :pointer)

(cffi:defctype DBT :pointer)

(cffi:defctype DB_ENV :pointer)

(cffi:defctype DB_HASH_STAT :pointer)

(cffi:defctype DB_LOCK_ILOCK :pointer)

(cffi:defctype DB_LOCK_STAT :pointer)

(cffi:defctype DB_LOCK_HSTAT :pointer)

(cffi:defctype DB_LOCK :pointer)

(cffi:defctype DB_LOCKER :pointer)

(cffi:defctype DB_LOCKREQ :pointer)

(cffi:defctype DB_LOCKTAB :pointer)

(cffi:defctype DB_LOG :pointer)

(cffi:defctype DB_LOGC :pointer)

(cffi:defctype DB_LOG_STAT :pointer)

(cffi:defctype DB_LSN :pointer)

(cffi:defctype DB_MPOOL :pointer)

(cffi:defctype DB_MPOOL_FSTAT :pointer)

(cffi:defctype DB_MPOOL_STAT :pointer)

(cffi:defctype DB_MPOOLFILE :pointer)

(cffi:defctype DB_MUTEX_STAT :pointer)

(cffi:defctype DB_MUTEX :pointer)

(cffi:defctype DB_MUTEXMGR :pointer)

(cffi:defctype DB_PREPLIST :pointer)

(cffi:defctype DB_QUEUE_STAT :pointer)

(cffi:defctype DB_REP :pointer)

(cffi:defctype DB_REP_STAT :pointer)

(cffi:defctype DB_REPMGR_SITE :pointer)

(cffi:defctype DB_REPMGR_STAT :pointer)

(cffi:defctype DB_SEQ_RECORD :pointer)

(cffi:defctype DB_SEQUENCE_STAT :pointer)

(cffi:defctype DB_SEQUENCE :pointer)

(cffi:defctype DB_TXN :pointer)

(cffi:defctype DB_TXN_ACTIVE :pointer)

(cffi:defctype DB_TXN_STAT :pointer)

(cffi:defctype DB_TXNMGR :pointer)

(cffi:defctype DBC :pointer)

(cffi:defctype DBC_INTERNAL :pointer)

(cffi:defctype DB_FH :pointer)

(cffi:defctype FNAME :pointer)

(cffi:defctype DB_KEY_RANGE :pointer)

(cffi:defctype MPOOLFILE :pointer)

(cffi:defcstruct __db_dbt
	(data :pointer)
	(size :unsigned-int)
	(ulen :unsigned-int)
	(dlen :unsigned-int)
	(doff :unsigned-int)
	(app_data :pointer)
	(flags :unsigned-int))

(cl:defconstant DB_CREATE #x0000001)

(cl:defconstant DB_DURABLE_UNKNOWN #x0000002)

(cl:defconstant DB_FORCE #x0000004)

(cl:defconstant DB_MULTIVERSION #x0000008)

(cl:defconstant DB_NOMMAP #x0000010)

(cl:defconstant DB_RDONLY #x0000020)

(cl:defconstant DB_RECOVER #x0000040)

(cl:defconstant DB_THREAD #x0000080)

(cl:defconstant DB_TRUNCATE #x0000100)

(cl:defconstant DB_TXN_NOSYNC #x0000200)

(cl:defconstant DB_TXN_NOWAIT #x0000400)

(cl:defconstant DB_TXN_NOT_DURABLE #x0000800)

(cl:defconstant DB_TXN_WRITE_NOSYNC #x0001000)

(cl:defconstant DB_SPARE_FLAG #x0002000)

(cl:defconstant DB_IGNORE_LEASE #x01000000)

(cl:defconstant DB_AUTO_COMMIT #x02000000)

(cl:defconstant DB_READ_COMMITTED #x04000000)

(cl:defconstant DB_DEGREE_2 #x04000000)

(cl:defconstant DB_READ_UNCOMMITTED #x08000000)

(cl:defconstant DB_DIRTY_READ #x08000000)

(cl:defconstant DB_TXN_SNAPSHOT #x10000000)

(cl:defconstant DB_CXX_NO_EXCEPTIONS #x0000001)

(cl:defconstant DB_RPCCLIENT #x0000002)

(cl:defconstant DB_XA_CREATE #x0000002)

(cl:defconstant DB_USE_ENVIRON #x0004000)

(cl:defconstant DB_USE_ENVIRON_ROOT #x0008000)

(cl:defconstant DB_INIT_CDB #x0010000)

(cl:defconstant DB_INIT_LOCK #x0020000)

(cl:defconstant DB_INIT_LOG #x0040000)

(cl:defconstant DB_INIT_MPOOL #x0080000)

(cl:defconstant DB_INIT_REP #x0100000)

(cl:defconstant DB_INIT_TXN #x0200000)

(cl:defconstant DB_LOCKDOWN #x0400000)

(cl:defconstant DB_PRIVATE #x0800000)

(cl:defconstant DB_RECOVER_FATAL #x1000000)

(cl:defconstant DB_REGISTER #x2000000)

(cl:defconstant DB_SYSTEM_MEM #x4000000)

(cl:defconstant DB_JOINENV #x0)

(cl:defconstant DB_EXCL #x0004000)

(cl:defconstant DB_FCNTL_LOCKING #x0008000)

(cl:defconstant DB_NO_AUTO_COMMIT #x0010000)

(cl:defconstant DB_RDWRMASTER #x0020000)

(cl:defconstant DB_WRITEOPEN #x0040000)

(cl:defconstant DB_IMMUTABLE_KEY #x0004000)

(cl:defconstant DB_TXN_SYNC #x0004000)

(cl:defconstant DB_TXN_WAIT #x0008000)

(cl:defconstant DB_CKP_INTERNAL #x0004000)

(cl:defconstant DB_ENCRYPT_AES #x0000001)

(cl:defconstant DB_CDB_ALLDB #x00004000)

(cl:defconstant DB_DIRECT_DB #x00008000)

(cl:defconstant DB_DIRECT_LOG #x00010000)

(cl:defconstant DB_DSYNC_DB #x00020000)

(cl:defconstant DB_DSYNC_LOG #x00040000)

(cl:defconstant DB_LOG_AUTOREMOVE #x00080000)

(cl:defconstant DB_LOG_INMEMORY #x00100000)

(cl:defconstant DB_NOLOCKING #x00200000)

(cl:defconstant DB_NOPANIC #x00400000)

(cl:defconstant DB_OVERWRITE #x00800000)

(cl:defconstant DB_PANIC_ENVIRONMENT #x01000000)

(cl:defconstant DB_REGION_INIT #x20000000)

(cl:defconstant DB_TIME_NOTGRANTED #x40000000)

(cl:defconstant DB_YIELDCPU #x80000000)

(cl:defconstant DB_UPGRADE #x0000001)

(cl:defconstant DB_VERIFY #x0000002)

(cl:defconstant DB_FREELIST_ONLY #x00004000)

(cl:defconstant DB_FREE_SPACE #x00008000)

(cl:defconstant DB_COMPACT_FLAGS (cl:logior #x00004000 #x00008000))

(cl:defconstant DB_DIRECT #x0004000)

(cl:defconstant DB_EXTENT #x0008000)

(cl:defconstant DB_ODDFILESIZE #x0010000)

(cl:defconstant DB_CHKSUM #x00004000)

(cl:defconstant DB_DUP #x00008000)

(cl:defconstant DB_DUPSORT #x00010000)

(cl:defconstant DB_ENCRYPT #x00020000)

(cl:defconstant DB_INORDER #x00040000)

(cl:defconstant DB_RECNUM #x00080000)

(cl:defconstant DB_RENUMBER #x00100000)

(cl:defconstant DB_REVSPLITOFF #x00200000)

(cl:defconstant DB_SNAPSHOT #x00400000)

(cl:defconstant DB_FAST_STAT #x0000001)

(cl:defconstant DB_STAT_ALL #x0000002)

(cl:defconstant DB_STAT_CLEAR #x0000004)

(cl:defconstant DB_STAT_LOCK_CONF #x0000008)

(cl:defconstant DB_STAT_LOCK_LOCKERS #x0000010)

(cl:defconstant DB_STAT_LOCK_OBJECTS #x0000020)

(cl:defconstant DB_STAT_LOCK_PARAMS #x0000040)

(cl:defconstant DB_STAT_MEMP_HASH #x0000080)

(cl:defconstant DB_STAT_NOERROR #x0000100)

(cl:defconstant DB_STAT_SUBSYSTEM #x0000200)

(cl:defconstant DB_JOIN_NOSORT #x0000001)

(cl:defconstant DB_AGGRESSIVE #x0000001)

(cl:defconstant DB_NOORDERCHK #x0000002)

(cl:defconstant DB_ORDERCHKONLY #x0000004)

(cl:defconstant DB_PR_PAGE #x0000008)

(cl:defconstant DB_PR_RECOVERYTEST #x0000010)

(cl:defconstant DB_PRINTABLE #x0000020)

(cl:defconstant DB_SALVAGE #x0000040)

(cl:defconstant DB_UNREF #x0000080)

(cl:defconstant DB_REP_ANYWHERE #x0000001)

(cl:defconstant DB_REP_NOBUFFER #x0000002)

(cl:defconstant DB_REP_PERMANENT #x0000004)

(cl:defconstant DB_REP_REREQUEST #x0000008)

(cffi:defctype db_mutex_t :unsigned-int)

(cl:defconstant DB_MUTEX_ALLOCATED #x01)

(cl:defconstant DB_MUTEX_LOCKED #x02)

(cl:defconstant DB_MUTEX_LOGICAL_LOCK #x04)

(cl:defconstant DB_MUTEX_PROCESS_ONLY #x08)

(cl:defconstant DB_MUTEX_SELF_BLOCK #x10)

(cffi:defcstruct __db_mutex_stat
	(st_mutex_align :unsigned-int)
	(st_mutex_tas_spins :unsigned-int)
	(st_mutex_cnt :unsigned-int)
	(st_mutex_free :unsigned-int)
	(st_mutex_inuse :unsigned-int)
	(st_mutex_inuse_max :unsigned-int)
	(st_region_wait :unsigned-int)
	(st_region_nowait :unsigned-int)
	(st_regsize :unsigned-int))

(cl:defconstant DB_THREADID_STRLEN 128)

(cl:defconstant DB_LOCKVERSION 1)

(cl:defconstant DB_FILE_ID_LEN 20)

(cl:defconstant DB_LOCK_NORUN 0)

(cl:defconstant DB_LOCK_DEFAULT 1)

(cl:defconstant DB_LOCK_EXPIRE 2)

(cl:defconstant DB_LOCK_MAXLOCKS 3)

(cl:defconstant DB_LOCK_MAXWRITE 4)

(cl:defconstant DB_LOCK_MINLOCKS 5)

(cl:defconstant DB_LOCK_MINWRITE 6)

(cl:defconstant DB_LOCK_OLDEST 7)

(cl:defconstant DB_LOCK_RANDOM 8)

(cl:defconstant DB_LOCK_YOUNGEST 9)

(cl:defconstant DB_LOCK_ABORT #x001)

(cl:defconstant DB_LOCK_NOWAIT #x002)

(cl:defconstant DB_LOCK_RECORD #x004)

(cl:defconstant DB_LOCK_SET_TIMEOUT #x008)

(cl:defconstant DB_LOCK_SWITCH #x010)

(cl:defconstant DB_LOCK_UPGRADE #x020)

(cl:defconstant DB_SET_LOCK_TIMEOUT 1)

(cl:defconstant DB_SET_TXN_NOW 2)

(cl:defconstant DB_SET_TXN_TIMEOUT 3)

(cffi:defcenum db_lockmode_t
	(:DB_LOCK_NG 0)
	(:DB_LOCK_READ 1)
	(:DB_LOCK_WRITE 2)
	(:DB_LOCK_WAIT 3)
	(:DB_LOCK_IWRITE 4)
	(:DB_LOCK_IREAD 5)
	(:DB_LOCK_IWR 6)
	(:DB_LOCK_READ_UNCOMMITTED 7)
	(:DB_LOCK_WWRITE 8))

(cffi:defcenum db_lockop_t
	(:DB_LOCK_DUMP 0)
	(:DB_LOCK_GET 1)
	(:DB_LOCK_GET_TIMEOUT 2)
	(:DB_LOCK_INHERIT 3)
	(:DB_LOCK_PUT 4)
	(:DB_LOCK_PUT_ALL 5)
	(:DB_LOCK_PUT_OBJ 6)
	(:DB_LOCK_PUT_READ 7)
	(:DB_LOCK_TIMEOUT 8)
	(:DB_LOCK_TRADE 9)
	(:DB_LOCK_UPGRADE_WRITE 10))

(cffi:defcenum db_status_t
	(:DB_LSTAT_ABORTED 1)
	(:DB_LSTAT_EXPIRED 2)
	(:DB_LSTAT_FREE 3)
	(:DB_LSTAT_HELD 4)
	(:DB_LSTAT_PENDING 5)
	(:DB_LSTAT_WAITING 6))

(cffi:defcstruct __db_lock_stat
	(st_id :unsigned-int)
	(st_cur_maxid :unsigned-int)
	(st_maxlocks :unsigned-int)
	(st_maxlockers :unsigned-int)
	(st_maxobjects :unsigned-int)
	(st_nmodes :int)
	(st_nlockers :unsigned-int)
	(st_nlocks :unsigned-int)
	(st_maxnlocks :unsigned-int)
	(st_maxnlockers :unsigned-int)
	(st_nobjects :unsigned-int)
	(st_maxnobjects :unsigned-int)
	(st_nrequests :unsigned-int)
	(st_nreleases :unsigned-int)
	(st_nupgrade :unsigned-int)
	(st_ndowngrade :unsigned-int)
	(st_lock_wait :unsigned-int)
	(st_lock_nowait :unsigned-int)
	(st_ndeadlocks :unsigned-int)
	(st_locktimeout :unsigned-int)
	(st_nlocktimeouts :unsigned-int)
	(st_txntimeout :unsigned-int)
	(st_ntxntimeouts :unsigned-int)
	(st_objs_wait :unsigned-int)
	(st_objs_nowait :unsigned-int)
	(st_lockers_wait :unsigned-int)
	(st_lockers_nowait :unsigned-int)
	(st_locks_wait :unsigned-int)
	(st_locks_nowait :unsigned-int)
	(st_region_wait :unsigned-int)
	(st_region_nowait :unsigned-int)
	(st_hash_len :unsigned-int)
	(st_regsize :unsigned-int))

(cffi:defcstruct __db_lock_hstat
	(st_nrequests :unsigned-int)
	(st_nreleases :unsigned-int)
	(st_nupgrade :unsigned-int)
	(st_ndowngrade :unsigned-int)
	(st_lock_wait :unsigned-int)
	(st_lock_nowait :unsigned-int)
	(st_nlocktimeouts :unsigned-int)
	(st_ntxntimeouts :unsigned-int)
	(st_hash_len :unsigned-int))

(cffi:defcstruct __db_ilock
	(pgno :unsigned-int)
        (fileid :uint8 :count 20)
	(type :unsigned-int))

(cffi:defcstruct __db_lock_u
	(off :unsigned-int)
	(ndx :unsigned-int)
	(gen :unsigned-int)
	(mode db_lockmode_t))

(cffi:defcstruct __db_lockreq
	(op db_lockop_t)
	(mode db_lockmode_t)
	(timeout :unsigned-int)
	(obj :pointer)
	(lock __db_lock_u))

(cl:defconstant DB_LOGVERSION 13)

(cl:defconstant DB_LOGOLDVER 8)

(cl:defconstant DB_LOGMAGIC #x040988)

(cl:defconstant DB_ARCH_ABS #x001)

(cl:defconstant DB_ARCH_DATA #x002)

(cl:defconstant DB_ARCH_LOG #x004)

(cl:defconstant DB_ARCH_REMOVE #x008)

(cl:defconstant DB_FLUSH #x001)

(cl:defconstant DB_LOG_CHKPNT #x002)

(cl:defconstant DB_LOG_COMMIT #x004)

(cl:defconstant DB_LOG_NOCOPY #x008)

(cl:defconstant DB_LOG_NOT_DURABLE #x010)

(cl:defconstant DB_LOG_WRNOSYNC #x020)

(cffi:defcstruct __db_lsn
	(file :unsigned-int)
	(offset :unsigned-int))

(cl:defconstant DB_user_BEGIN 10000)

(cl:defconstant DB_debug_FLAG #x80000000)

(cffi:defcstruct __db_log_cursor
	(dbenv :pointer)
	(fhp :pointer)
	(lsn __db_lsn)
	(len :unsigned-int)
	(prev :unsigned-int)
	(dbt __db_dbt)
	(p_lsn __db_lsn)
	(p_version :unsigned-int)
	(bp :pointer)
	(bp_size :unsigned-int)
	(bp_rlen :unsigned-int)
	(bp_lsn __db_lsn)
	(bp_maxrec :unsigned-int)
	(close :pointer)
	(get :pointer)
	(version :pointer)
	(flags :unsigned-int))

(cffi:defcstruct __db_log_stat
	(st_magic :unsigned-int)
	(st_version :unsigned-int)
	(st_mode :int)
	(st_lg_bsize :unsigned-int)
	(st_lg_size :unsigned-int)
	(st_wc_bytes :unsigned-int)
	(st_wc_mbytes :unsigned-int)
	(st_record :unsigned-int)
	(st_w_bytes :unsigned-int)
	(st_w_mbytes :unsigned-int)
	(st_wcount :unsigned-int)
	(st_wcount_fill :unsigned-int)
	(st_rcount :unsigned-int)
	(st_scount :unsigned-int)
	(st_region_wait :unsigned-int)
	(st_region_nowait :unsigned-int)
	(st_cur_file :unsigned-int)
	(st_cur_offset :unsigned-int)
	(st_disk_file :unsigned-int)
	(st_disk_offset :unsigned-int)
	(st_maxcommitperflush :unsigned-int)
	(st_mincommitperflush :unsigned-int)
	(st_regsize :unsigned-int))

(cl:defconstant DB_MPOOL_CREATE #x001)

(cl:defconstant DB_MPOOL_DIRTY #x002)

(cl:defconstant DB_MPOOL_EDIT #x004)

(cl:defconstant DB_MPOOL_FREE #x008)

(cl:defconstant DB_MPOOL_LAST #x010)

(cl:defconstant DB_MPOOL_NEW #x020)

(cl:defconstant DB_MPOOL_DISCARD #x001)

(cl:defconstant DB_MPOOL_NOFILE #x001)

(cl:defconstant DB_MPOOL_UNLINK #x002)

(cffi:defcenum DB_CACHE_PRIORITY
	(:DB_PRIORITY_UNCHANGED 0)
	(:DB_PRIORITY_VERY_LOW 1)
	(:DB_PRIORITY_LOW 2)
	(:DB_PRIORITY_DEFAULT 3)
	(:DB_PRIORITY_HIGH 4)
	(:DB_PRIORITY_VERY_HIGH 5))

(cffi:defcstruct __db_mpoolfile
	(fhp :pointer)
	(ref :unsigned-int)
	(pinref :unsigned-int)
	(dbenv :pointer)
	(mfp :pointer)
	(clear_len :unsigned-int)
	(fileid :pointer)
	(ftype :int)
	(lsn_offset :int)
	(gbytes :unsigned-int)
	(bytes :unsigned-int)
	(pgcookie :pointer)
	(priority :int)
	(addr :pointer)
	(len :pointer)
	(config_flags :unsigned-int)
	(close :pointer)
	(get :pointer)
	(get_clear_len :pointer)
	(get_fileid :pointer)
	(get_flags :pointer)
	(get_ftype :pointer)
	(get_last_pgno :pointer)
	(get_lsn_offset :pointer)
	(get_maxsize :pointer)
	(get_pgcookie :pointer)
	(get_priority :pointer)
	(open :pointer)
	(put :pointer)
	(set_clear_len :pointer)
	(set_fileid :pointer)
	(set_flags :pointer)
	(set_ftype :pointer)
	(set_lsn_offset :pointer)
	(set_maxsize :pointer)
	(set_pgcookie :pointer)
	(set_priority :pointer)
	(sync :pointer)
	(flags :unsigned-int)
	(q :pointer))

(cffi:defcstruct __db_mpoolfile_q
	(tqe_next :pointer)
	(tqe_prev :pointer))

(cffi:defcstruct __db_mpool_stat
	(st_gbytes :unsigned-int)
	(st_bytes :unsigned-int)
	(st_ncache :unsigned-int)
	(st_max_ncache :unsigned-int)
	(st_mmapsize :pointer)
	(st_maxopenfd :int)
	(st_maxwrite :int)
	(st_maxwrite_sleep :unsigned-int)
	(st_pages :unsigned-int)
	(st_map :unsigned-int)
	(st_cache_hit :unsigned-int)
	(st_cache_miss :unsigned-int)
	(st_page_create :unsigned-int)
	(st_page_in :unsigned-int)
	(st_page_out :unsigned-int)
	(st_ro_evict :unsigned-int)
	(st_rw_evict :unsigned-int)
	(st_page_trickle :unsigned-int)
	(st_page_clean :unsigned-int)
	(st_page_dirty :unsigned-int)
	(st_hash_buckets :unsigned-int)
	(st_hash_searches :unsigned-int)
	(st_hash_longest :unsigned-int)
	(st_hash_examined :unsigned-int)
	(st_hash_nowait :unsigned-int)
	(st_hash_wait :unsigned-int)
	(st_hash_max_nowait :unsigned-int)
	(st_hash_max_wait :unsigned-int)
	(st_region_nowait :unsigned-int)
	(st_region_wait :unsigned-int)
	(st_mvcc_frozen :unsigned-int)
	(st_mvcc_thawed :unsigned-int)
	(st_mvcc_freed :unsigned-int)
	(st_alloc :unsigned-int)
	(st_alloc_buckets :unsigned-int)
	(st_alloc_max_buckets :unsigned-int)
	(st_alloc_pages :unsigned-int)
	(st_alloc_max_pages :unsigned-int)
	(st_io_wait :unsigned-int)
	(st_regsize :unsigned-int))

(cffi:defcstruct __db_mpool_fstat
	(file_name :string)
	(st_pagesize :unsigned-int)
	(st_map :unsigned-int)
	(st_cache_hit :unsigned-int)
	(st_cache_miss :unsigned-int)
	(st_page_create :unsigned-int)
	(st_page_in :unsigned-int)
	(st_page_out :unsigned-int))

(cl:defconstant DB_TXNVERSION 1)

(cffi:defcenum db_recops
	(:DB_TXN_ABORT 0)
	(:DB_TXN_APPLY 1)
	(:DB_TXN_BACKWARD_ALLOC 2)
	(:DB_TXN_BACKWARD_ROLL 3)
	(:DB_TXN_FORWARD_ROLL 4)
	(:DB_TXN_OPENFILES 5)
	(:DB_TXN_POPENFILES 6)
	(:DB_TXN_PRINT 7))

(cffi:defcstruct __db_txn_klinks
	(tqe_next :pointer)
	(tqe_prev :pointer))

(cffi:defcstruct __db_txn_logs
	(stqh_first :pointer)
	(stqh_last :pointer))

(cffi:defcstruct __db_txn_events
	(tqh_first :pointer)
	(tqh_last :pointer))

(cffi:defcstruct __db_txn_kids
	(tqh_first :pointer)
	(tqh_last :pointer))

(cffi:defcstruct __db_txn_xalinks
	(tqe_next :pointer)
	(tqe_prev :pointer))

(cffi:defcstruct __db_txn_links
	(tqe_next :pointer)
	(tqe_prev :pointer))

(cffi:defcstruct __db_txn
	(mgrp :pointer)
	(parent :pointer)
	(txnid :unsigned-int)
	(name :string)
	(locker :pointer)
	(tid :unsigned-long)
	(td :pointer)
	(lock_timeout :unsigned-int)
	(expire :unsigned-int)
	(txn_list :pointer)
	(links __db_txn_links)
	(xalinks __db_txn_xalinks)
	(kids __db_txn_kids)
	(events __db_txn_events)
	(logs __db_txn_logs)
	(klinks __db_txn_klinks)
	
	(api_internal :pointer)
	(xml_internal :pointer)
	(cursors :unsigned-int)
	(abort :pointer)
	(commit :pointer)
	(discard :pointer)
	(get_name :pointer)
	(id :pointer)
	(prepare :pointer)
	(set_name :pointer)
	(set_timeout :pointer)
	(set_txn_lsnp :pointer)
	(flags :unsigned-int))




(cl:defconstant TXN_SYNC_FLAGS (cl:logior #x2000 #x0040 #x4000))

(cl:defconstant DB_XIDDATASIZE 128)

(cffi:defcstruct __db_preplist
	(txn :pointer)
	(gid :pointer))

(cffi:defcstruct __db_txn_active
	(txnid :unsigned-int)
	(parentid :unsigned-int)
	(pid :int)
	(tid :unsigned-long)
	(lsn __db_lsn)
	(read_lsn __db_lsn)
	(mvcc_ref :unsigned-int)
	(status :unsigned-int)
	(xa_status :unsigned-int)
	(xid :pointer)
	(name :pointer))

(cffi:defcstruct __db_txn_stat
	(st_nrestores :unsigned-int)
	(st_last_ckp __db_lsn)
	(st_time_ckp :long)
	(st_last_txnid :unsigned-int)
	(st_maxtxns :unsigned-int)
	(st_naborts :unsigned-int)
	(st_nbegins :unsigned-int)
	(st_ncommits :unsigned-int)
	(st_nactive :unsigned-int)
	(st_nsnapshot :unsigned-int)
	(st_maxnactive :unsigned-int)
	(st_maxnsnapshot :unsigned-int)
	(st_txnarray :pointer)
	(st_region_wait :unsigned-int)
	(st_region_nowait :unsigned-int)
	(st_regsize :unsigned-int))

(cl:defconstant DB_EID_BROADCAST -1)

(cl:defconstant DB_EID_INVALID -2)

(cl:defconstant DB_REP_CONF_BULK #x0001)

(cl:defconstant DB_REP_CONF_DELAYCLIENT #x0002)

(cl:defconstant DB_REP_CONF_NOAUTOINIT #x0004)

(cl:defconstant DB_REP_CONF_NOWAIT #x0008)

(cl:defconstant DB_REP_CLIENT 1)

(cl:defconstant DB_REP_ELECTION 2)

(cl:defconstant DB_REP_MASTER 3)

(cl:defconstant DB_REPFLAGS_MASK #x000000ff)

(cl:defconstant DB_REP_DEFAULT_PRIORITY 100)

(cl:defconstant DB_REPMGR_ACKS_ALL 1)

(cl:defconstant DB_REPMGR_ACKS_ALL_PEERS 2)

(cl:defconstant DB_REPMGR_ACKS_NONE 3)

(cl:defconstant DB_REPMGR_ACKS_ONE 4)

(cl:defconstant DB_REPMGR_ACKS_ONE_PEER 5)

(cl:defconstant DB_REPMGR_ACKS_QUORUM 6)

(cl:defconstant DB_REP_ACK_TIMEOUT 1)

(cl:defconstant DB_REP_CHECKPOINT_DELAY 2)

(cl:defconstant DB_REP_CONNECTION_RETRY 3)

(cl:defconstant DB_REP_ELECTION_RETRY 4)

(cl:defconstant DB_REP_ELECTION_TIMEOUT 5)

(cl:defconstant DB_REP_FULL_ELECTION_TIMEOUT 6)

(cl:defconstant DB_REP_LEASE_TIMEOUT 7)

(cl:defconstant DB_EVENT_NO_SUCH_EVENT 0)

(cl:defconstant DB_EVENT_PANIC 1)

(cl:defconstant DB_EVENT_REP_CLIENT 2)

(cl:defconstant DB_EVENT_REP_ELECTED 3)

(cl:defconstant DB_EVENT_REP_MASTER 4)

(cl:defconstant DB_EVENT_REP_NEWMASTER 5)

(cl:defconstant DB_EVENT_REP_PERM_FAILED 6)

(cl:defconstant DB_EVENT_REP_STARTUPDONE 7)

(cl:defconstant DB_EVENT_WRITE_FAILED 8)

(cl:defconstant DB_REPMGR_PEER #x01)

(cffi:defcstruct __db_repmgr_site
	(eid :int)
	(host :string)
	(port :unsigned-int)
	(status :unsigned-int))

(cffi:defcstruct __db_rep_stat
	(st_log_queued :unsigned-int)
	(st_startup_complete :unsigned-int)
	(st_status :unsigned-int)
	(st_next_lsn __db_lsn)
	(st_waiting_lsn __db_lsn)
	(st_next_pg :unsigned-int)
	(st_waiting_pg :unsigned-int)
	(st_dupmasters :unsigned-int)
	(st_env_id :int)
	(st_env_priority :int)
	(st_bulk_fills :unsigned-int)
	(st_bulk_overflows :unsigned-int)
	(st_bulk_records :unsigned-int)
	(st_bulk_transfers :unsigned-int)
	(st_client_rerequests :unsigned-int)
	(st_client_svc_req :unsigned-int)
	(st_client_svc_miss :unsigned-int)
	(st_gen :unsigned-int)
	(st_egen :unsigned-int)
	(st_log_duplicated :unsigned-int)
	(st_log_queued_max :unsigned-int)
	(st_log_queued_total :unsigned-int)
	(st_log_records :unsigned-int)
	(st_log_requested :unsigned-int)
	(st_master :int)
	(st_master_changes :unsigned-int)
	(st_msgs_badgen :unsigned-int)
	(st_msgs_processed :unsigned-int)
	(st_msgs_recover :unsigned-int)
	(st_msgs_send_failures :unsigned-int)
	(st_msgs_sent :unsigned-int)
	(st_newsites :unsigned-int)
	(st_nsites :int)
	(st_nthrottles :unsigned-int)
	(st_outdated :unsigned-int)
	(st_pg_duplicated :unsigned-int)
	(st_pg_records :unsigned-int)
	(st_pg_requested :unsigned-int)
	(st_txns_applied :unsigned-int)
	(st_startsync_delayed :unsigned-int)
	(st_elections :unsigned-int)
	(st_elections_won :unsigned-int)
	(st_election_cur_winner :int)
	(st_election_gen :unsigned-int)
	(st_election_lsn __db_lsn)
	(st_election_nsites :int)
	(st_election_nvotes :int)
	(st_election_priority :int)
	(st_election_status :int)
	(st_election_tiebreaker :unsigned-int)
	(st_election_votes :int)
	(st_election_sec :unsigned-int)
	(st_election_usec :unsigned-int))

(cffi:defcstruct __db_repmgr_stat
	(st_perm_failed :unsigned-int)
	(st_msgs_queued :unsigned-int)
	(st_msgs_dropped :unsigned-int)
	(st_connection_drop :unsigned-int)
	(st_connect_fail :unsigned-int))

(cffi:defcstruct __db_seq_record
	(seq_version :unsigned-int)
	(flags :unsigned-int)
	(seq_value :pointer)
	(seq_max :pointer)
	(seq_min :pointer))

(cffi:defcstruct __db_sequence
	(seq_dbp :pointer)
	(mtx_seq :unsigned-int)
	(seq_rp :pointer)
	(seq_record __db_seq_record)
	(seq_cache_size :int)
	(seq_last_value :pointer)
	(seq_key __db_dbt)
	(seq_data __db_dbt)
	(api_internal :pointer)
	(close :pointer)
	(get :pointer)
	(get_cachesize :pointer)
	(get_db :pointer)
	(get_flags :pointer)
	(get_key :pointer)
	(get_range :pointer)
	(initial_value :pointer)
	(open :pointer)
	(remove :pointer)
	(set_cachesize :pointer)
	(set_flags :pointer)
	(set_range :pointer)
	(stat :pointer)
	(stat_print :pointer))

(cffi:defcstruct __db_seq_stat
	(st_wait :unsigned-int)
	(st_nowait :unsigned-int)
	(st_current :pointer)
	(st_value :pointer)
	(st_last_value :pointer)
	(st_min :pointer)
	(st_max :pointer)
	(st_cache_size :int)
	(st_flags :unsigned-int))

(cffi:defcenum DBTYPE
	(:DB_BTREE 1)
	(:DB_HASH 2)
	(:DB_RECNO 3)
	(:DB_QUEUE 4)
	(:DB_UNKNOWN 5))

(cl:defconstant DB_RENAMEMAGIC #x030800)

(cl:defconstant DB_BTREEVERSION 9)

(cl:defconstant DB_BTREEOLDVER 8)

(cl:defconstant DB_BTREEMAGIC #x053162)

(cl:defconstant DB_HASHVERSION 9)

(cl:defconstant DB_HASHOLDVER 7)

(cl:defconstant DB_HASHMAGIC #x061561)

(cl:defconstant DB_QAMVERSION 4)

(cl:defconstant DB_QAMOLDVER 3)

(cl:defconstant DB_QAMMAGIC #x042253)

(cl:defconstant DB_SEQUENCE_VERSION 2)

(cl:defconstant DB_SEQUENCE_OLDVER 1)

(cl:defconstant DB_AFTER 1)

(cl:defconstant DB_APPEND 2)

(cl:defconstant DB_BEFORE 3)

(cl:defconstant DB_CONSUME 4)

(cl:defconstant DB_CONSUME_WAIT 5)

(cl:defconstant DB_CURRENT 6)

(cl:defconstant DB_FIRST 7)

(cl:defconstant DB_GET_BOTH 8)

(cl:defconstant DB_GET_BOTHC 9)

(cl:defconstant DB_GET_BOTH_RANGE 10)

(cl:defconstant DB_GET_RECNO 11)

(cl:defconstant DB_JOIN_ITEM 12)

(cl:defconstant DB_KEYFIRST 13)

(cl:defconstant DB_KEYLAST 14)

(cl:defconstant DB_LAST 15)

(cl:defconstant DB_NEXT 16)

(cl:defconstant DB_NEXT_DUP 17)

(cl:defconstant DB_NEXT_NODUP 18)

(cl:defconstant DB_NODUPDATA 19)

(cl:defconstant DB_NOOVERWRITE 20)

(cl:defconstant DB_NOSYNC 21)

(cl:defconstant DB_POSITION 22)

(cl:defconstant DB_PREV 23)

(cl:defconstant DB_PREV_DUP 24)

(cl:defconstant DB_PREV_NODUP 25)

(cl:defconstant DB_SET 26)

(cl:defconstant DB_SET_RANGE 27)

(cl:defconstant DB_SET_RECNO 28)

(cl:defconstant DB_UPDATE_SECONDARY 29)

(cl:defconstant DB_WRITECURSOR 30)

(cl:defconstant DB_WRITELOCK 31)

(cl:defconstant DB_OPFLAGS_MASK #x000000ff)

(cl:defconstant DB_MULTIPLE #x10000000)

(cl:defconstant DB_MULTIPLE_KEY #x20000000)

(cl:defconstant DB_RMW #x40000000)

(cl:defconstant DB_BUFFER_SMALL -30999)

(cl:defconstant DB_DONOTINDEX -30998)

(cl:defconstant DB_KEYEMPTY -30997)

(cl:defconstant DB_KEYEXIST -30996)

(cl:defconstant DB_LOCK_DEADLOCK -30995)

(cl:defconstant DB_LOCK_NOTGRANTED -30994)

(cl:defconstant DB_LOG_BUFFER_FULL -30993)

(cl:defconstant DB_NOSERVER -30992)

(cl:defconstant DB_NOSERVER_HOME -30991)

(cl:defconstant DB_NOSERVER_ID -30990)

(cl:defconstant DB_NOTFOUND -30989)

(cl:defconstant DB_OLD_VERSION -30988)

(cl:defconstant DB_PAGE_NOTFOUND -30987)

(cl:defconstant DB_REP_DUPMASTER -30986)

(cl:defconstant DB_REP_HANDLE_DEAD -30985)

(cl:defconstant DB_REP_HOLDELECTION -30984)

(cl:defconstant DB_REP_IGNORE -30983)

(cl:defconstant DB_REP_ISPERM -30982)

(cl:defconstant DB_REP_JOIN_FAILURE -30981)

(cl:defconstant DB_REP_LEASE_EXPIRED -30980)

(cl:defconstant DB_REP_LOCKOUT -30979)

(cl:defconstant DB_REP_NEWSITE -30978)

(cl:defconstant DB_REP_NOTPERM -30977)

(cl:defconstant DB_REP_UNAVAIL -30976)

(cl:defconstant DB_RUNRECOVERY -30975)

(cl:defconstant DB_SECONDARY_BAD -30974)

(cl:defconstant DB_VERIFY_BAD -30973)

(cl:defconstant DB_VERSION_MISMATCH -30972)

(cl:defconstant DB_ALREADY_ABORTED -30899)

(cl:defconstant DB_DELETED -30898)

(cl:defconstant DB_EVENT_NOT_HANDLED -30897)

(cl:defconstant DB_NEEDSPLIT -30896)

(cl:defconstant DB_REP_BULKOVF -30895)

(cl:defconstant DB_REP_EGENCHG -30894)

(cl:defconstant DB_REP_LOGREADY -30893)

(cl:defconstant DB_REP_NEWMASTER -30892)

(cl:defconstant DB_REP_PAGEDONE -30891)

(cl:defconstant DB_SURPRISE_KID -30890)

(cl:defconstant DB_SWAPBYTES -30889)

(cl:defconstant DB_TIMEOUT -30888)

(cl:defconstant DB_TXN_CKP -30887)

(cl:defconstant DB_VERIFY_FATAL -30886)

(cffi:defcstruct __db_s_links
	(le_next :pointer)
	(le_prev :pointer))

(cffi:defcstruct __db_s_secondaries
	(lh_first :pointer))

(cffi:defcstruct __db_join_queue
	(tqh_first :pointer)
	(tqh_last :pointer))

(cffi:defcstruct __db_active_queue
	(tqh_first :pointer)
	(tqh_last :pointer))

(cffi:defcstruct __db_free_queue
	(tqh_first :pointer)
	(tqh_last :pointer))

(cffi:defcstruct __db_dblistlinks
	(tqe_next :pointer)
	(tqe_prev :pointer))

(cffi:defcstruct __dbc_links
	(tqe_next :pointer)
	(tqe_prev :pointer))

(cffi:defcstruct __db
	(pgsize :unsigned-int)
	(priority DB_CACHE_PRIORITY)
	(db_append_recno :pointer)
	(db_feedback :pointer)
	(dup_compare :pointer)
	(app_private :pointer)
	(dbenv :pointer)
	(type DBTYPE)
	(mpf :pointer)
	(mutex :unsigned-int)
	(fname :string)
	(dname :string)
	(open_flags :unsigned-int)
	(fileid :uint8 :count #.DB_FILE_ID_LEN)
	(adj_fileid :unsigned-int)
	(log_filename :pointer)
	(meta_pgno :unsigned-int)
	(locker :pointer)
	(cur_locker :pointer)
	(cur_txn :pointer)
	(associate_locker :pointer)
	(handle_lock __db_lock_u)
	(cl_id :unsigned-int)
	(timestamp :long)
	(fid_gen :unsigned-int)
	(my_rskey __db_dbt)
	(my_rkey __db_dbt)
	(my_rdata __db_dbt)
	(saved_open_fhp :pointer)
	(dblistlinks __db_dblistlinks)
	(free_queue __db_free_queue)
	(active_queue __db_active_queue)
	(join_queue __db_join_queue)
	(s_secondaries __db_s_secondaries)
	(s_links __db_s_links)
	(s_refcnt :unsigned-int)
	(s_callback :pointer)
	(s_primary :pointer)
	(s_assoc_flags :unsigned-int)
	(api_internal :pointer)
	(bt_internal :pointer)
	(h_internal :pointer)
	(q_internal :pointer)
	(xa_internal :pointer)
	(associate :pointer)
	(close :pointer)
	(compact :pointer)
	(cursor :pointer)
	(del :pointer)
	(err :pointer)
	(errx :pointer)
	(exists :pointer)
	(fd :pointer)
	(get :pointer)
	(get_bt_minkey :pointer)
	(get_byteswapped :pointer)
	(get_cachesize :pointer)
	(get_dbname :pointer)
	(get_encrypt_flags :pointer)
	(get_env :pointer)
	(get_errfile :pointer)
	(get_errpfx :pointer)
	(get_flags :pointer)
	(get_h_ffactor :pointer)
	(get_h_nelem :pointer)
	(get_lorder :pointer)
	(get_mpf :pointer)
	(get_msgfile :pointer)
	(get_multiple :pointer)
	(get_open_flags :pointer)
	(get_pagesize :pointer)
	(get_priority :pointer)
	(get_q_extentsize :pointer)
	(get_re_delim :pointer)
	(get_re_len :pointer)
	(get_re_pad :pointer)
	(get_re_source :pointer)
	(get_transactional :pointer)
	(get_type :pointer)
	(join :pointer)
	(key_range :pointer)
	(open :pointer)
	(pget :pointer)
	(put :pointer)
	(remove :pointer)
	(rename :pointer)
	(set_alloc :pointer)
	(set_append_recno :pointer)
	(set_bt_compare :pointer)
	(set_bt_minkey :pointer)
	(set_bt_prefix :pointer)
	(set_cachesize :pointer)
	(set_dup_compare :pointer)
	(set_encrypt :pointer)
	(set_errcall :pointer)
	(set_errfile :pointer)
	(set_errpfx :pointer)
	(set_feedback :pointer)
	(set_flags :pointer)
	(set_h_compare :pointer)
	(set_h_ffactor :pointer)
	(set_h_hash :pointer)
	(set_h_nelem :pointer)
	(set_lorder :pointer)
	(set_msgcall :pointer)
	(set_msgfile :pointer)
	(set_pagesize :pointer)
	(set_paniccall :pointer)
	(set_priority :pointer)
	(set_q_extentsize :pointer)
	(set_re_delim :pointer)
	(set_re_len :pointer)
	(set_re_pad :pointer)
	(set_re_source :pointer)
	(stat :pointer)
	(stat_print :pointer)
	(sync :pointer)
	(truncate :pointer)
	(upgrade :pointer)
	(verify :pointer)
	(dump :pointer)
	(db_am_remove :pointer)
	(db_am_rename :pointer)
	(stored_get :pointer)
	(stored_close :pointer)
	(am_ok :unsigned-int)
	(preserve_fid :int)
	(orig_flags :unsigned-int)
	(flags :unsigned-int))



(cffi:defcstruct __dbc
	(dbp :pointer)
	(txn :pointer)
	(priority DB_CACHE_PRIORITY)
	(links __dbc_links)
	(rskey :pointer)
	(rkey :pointer)
	(rdata :pointer)
	(my_rskey __db_dbt)
	(my_rkey __db_dbt)
	(my_rdata __db_dbt)
	(lref :pointer)
	(locker :pointer)
	(lock_dbt __db_dbt)
	(lock __db_ilock)
	(mylock __db_lock_u)
	(cl_id :unsigned-int)
	(dbtype DBTYPE)
	(internal :pointer)
	(close :pointer)
	(count :pointer)
	(del :pointer)
	(dup :pointer)
	(get :pointer)
	(get_priority :pointer)
	(pget :pointer)
	(put :pointer)
	(set_priority :pointer)
	(c_close :pointer)
	(c_count :pointer)
	(c_del :pointer)
	(c_dup :pointer)
	(c_get :pointer)
	(c_pget :pointer)
	(c_put :pointer)
	(am_bulk :pointer)
	(am_close :pointer)
	(am_del :pointer)
	(am_destroy :pointer)
	(am_get :pointer)
	(am_put :pointer)
	(am_writelock :pointer)
	(flags :unsigned-int)
	(links :pointer))



(cffi:defcstruct __key_range
	(less :double)
	(equal :double)
	(greater :double))

(cffi:defcstruct __db_bt_stat
	(bt_magic :unsigned-int)
	(bt_version :unsigned-int)
	(bt_metaflags :unsigned-int)
	(bt_nkeys :unsigned-int)
	(bt_ndata :unsigned-int)
	(bt_pagecnt :unsigned-int)
	(bt_pagesize :unsigned-int)
	(bt_minkey :unsigned-int)
	(bt_re_len :unsigned-int)
	(bt_re_pad :unsigned-int)
	(bt_levels :unsigned-int)
	(bt_int_pg :unsigned-int)
	(bt_leaf_pg :unsigned-int)
	(bt_dup_pg :unsigned-int)
	(bt_over_pg :unsigned-int)
	(bt_empty_pg :unsigned-int)
	(bt_free :unsigned-int)
	(bt_int_pgfree :unsigned-int)
	(bt_leaf_pgfree :unsigned-int)
	(bt_dup_pgfree :unsigned-int)
	(bt_over_pgfree :unsigned-int))

(cffi:defcstruct __db_compact
	(compact_fillpercent :unsigned-int)
	(compact_timeout :unsigned-int)
	(compact_pages :unsigned-int)
	(compact_pages_free :unsigned-int)
	(compact_pages_examine :unsigned-int)
	(compact_levels :unsigned-int)
	(compact_deadlock :unsigned-int)
	(compact_pages_truncated :unsigned-int)
	(compact_truncate :unsigned-int))

(cffi:defcstruct __db_h_stat
	(hash_magic :unsigned-int)
	(hash_version :unsigned-int)
	(hash_metaflags :unsigned-int)
	(hash_nkeys :unsigned-int)
	(hash_ndata :unsigned-int)
	(hash_pagecnt :unsigned-int)
	(hash_pagesize :unsigned-int)
	(hash_ffactor :unsigned-int)
	(hash_buckets :unsigned-int)
	(hash_free :unsigned-int)
	(hash_bfree :unsigned-int)
	(hash_bigpages :unsigned-int)
	(hash_big_bfree :unsigned-int)
	(hash_overflows :unsigned-int)
	(hash_ovfl_free :unsigned-int)
	(hash_dup :unsigned-int)
	(hash_dup_free :unsigned-int))

(cffi:defcstruct __db_qam_stat
	(qs_magic :unsigned-int)
	(qs_version :unsigned-int)
	(qs_metaflags :unsigned-int)
	(qs_nkeys :unsigned-int)
	(qs_ndata :unsigned-int)
	(qs_pagesize :unsigned-int)
	(qs_extentsize :unsigned-int)
	(qs_pages :unsigned-int)
	(qs_re_len :unsigned-int)
	(qs_re_pad :unsigned-int)
	(qs_pgfree :unsigned-int)
	(qs_first_recno :unsigned-int)
	(qs_cur_recno :unsigned-int))

(cl:defconstant DB_REGION_MAGIC #x120897)

(cffi:defcstruct __db_env_xa_txn
	(tqh_first :pointer)
	(tqh_last :pointer))

(cffi:defcstruct __db_env_links
	(tqe_next :pointer)
	(tqe_prev :pointer))

(cffi:defcstruct __db_env_dblist
	(tqh_first :pointer)
	(tqh_last :pointer))

(cffi:defcstruct __db_env_fdlist
	(tqh_first :pointer)
	(tqh_last :pointer))

(cffi:defcstruct __db_env_mutex_iq
	(alloc_id :int)
	(flags :unsigned-int))

(cffi:defcstruct __db_env
	(db_errcall :pointer)
	(db_errfile :pointer)
	(db_errpfx :string)
	(db_msgfile :pointer)
	(db_msgcall :pointer)
	(db_feedback :pointer)
	(db_paniccall :pointer)
	(db_event_func :pointer)
	(db_malloc :pointer)
	(db_realloc :pointer)
	(db_free :pointer)
	(dbt_usercopy :pointer)
	(verbose :unsigned-int) 
	(app_private :pointer)
	(app_dispatch :pointer)
	(mutex_align :unsigned-int)
	(mutex_cnt :unsigned-int)
	(mutex_inc :unsigned-int)
	(mutex_tas_spins :unsigned-int)
	(mutex_iq :pointer)
	(mutex_iq_next :unsigned-int)
	(mutex_iq_max :unsigned-int)
	(lk_conflicts :pointer)
	(lk_modes :int)
	(lk_max :unsigned-int)
	(lk_max_lockers :unsigned-int)
	(lk_max_objects :unsigned-int)
	(lk_detect :unsigned-int)
	(lk_timeout :unsigned-int)
	(lg_bsize :unsigned-int)
	(lg_size :unsigned-int)
	(lg_regionmax :unsigned-int)
	(lg_filemode :int)
	(mp_ncache :unsigned-int)
	(mp_gbytes :unsigned-int)
	(mp_bytes :unsigned-int)
	(mp_max_gbytes :unsigned-int)
	(mp_max_bytes :unsigned-int)
	(mp_mmapsize :pointer)
	(mp_maxopenfd :int)
	(mp_maxwrite :int)
	(mp_maxwrite_sleep :unsigned-int)
	(tx_max :unsigned-int)
	(tx_timestamp :long)
	(tx_timeout :unsigned-int)
	(thr_nbucket :unsigned-int)
	(thr_max :unsigned-int)
	(thr_hashtab :pointer)
	(mtx_env :unsigned-int)
	(pid_cache :int)
	(db_home :string)
	(db_log_dir :string)
	(db_tmp_dir :string)
	(db_data_dir :pointer)
	(data_cnt :int)
	(data_next :int)
	(db_mode :int)
	(dir_mode :int)
	(env_lref :pointer)
	(open_flags :unsigned-int)
	(reginfo :pointer)
	(lockfhp :pointer)
	(registry :pointer)
	(registry_off :unsigned-int)
	(thread_id :pointer)
	(is_alive :pointer)
	(thread_id_string :pointer)
	(recover_dtab :pointer)
	(recover_dtab_size :pointer)
	(cl_handle :pointer)
	(cl_id :unsigned-int)
	(db_ref :int)
	(shm_key :long)
	(fdlist __db_env_fdlist)
	(mtx_dblist :unsigned-int)
	(dblist __db_env_dblist)
	(links __db_env_links)
	(xa_txn __db_env_xa_txn)

	(xa_rmid :int)
	(passwd :string)
	(passwd_len :pointer)
	(crypto_handle :pointer)
	(mtx_mt :unsigned-int)
	(mti :int)
	(mt :pointer)
	(api1_internal :pointer)
	(api2_internal :pointer)
	(lk_handle :pointer)
	(lg_handle :pointer)
	(mp_handle :pointer)
	(mutex_handle :pointer)
	(rep_handle :pointer)
	(tx_handle :pointer)
	(cdsgroup_begin :pointer)
	(close :pointer)
	(dbremove :pointer)
	(dbrename :pointer)
	(err :pointer)
	(errx :pointer)
	(failchk :pointer)
	(fileid_reset :pointer)
	(get_cachesize :pointer)
	(get_cache_max :pointer)
	(get_data_dirs :pointer)
	(get_encrypt_flags :pointer)
	(get_errfile :pointer)
	(get_errpfx :pointer)
	(get_flags :pointer)
	(get_home :pointer)
	(get_lg_bsize :pointer)
	(get_lg_dir :pointer)
	(get_lg_filemode :pointer)
	(get_lg_max :pointer)
	(get_lg_regionmax :pointer)
	(get_lk_conflicts :pointer)
	(get_lk_detect :pointer)
	(get_lk_max_lockers :pointer)
	(get_lk_max_locks :pointer)
	(get_lk_max_objects :pointer)
	(get_mp_max_openfd :pointer)
	(get_mp_max_write :pointer)
	(get_mp_mmapsize :pointer)
	(get_msgfile :pointer)
	(get_open_flags :pointer)
	(get_shm_key :pointer)
	(get_thread_count :pointer)
	(get_timeout :pointer)
	(get_tmp_dir :pointer)
	(get_tx_max :pointer)
	(get_tx_timestamp :pointer)
	(get_verbose :pointer)
	(is_bigendian :pointer)
	(lock_detect :pointer)
	(lock_get :pointer)
	(lock_id :pointer)
	(lock_id_free :pointer)
	(lock_put :pointer)
	(lock_stat :pointer)
	(lock_stat_print :pointer)
	(lock_vec :pointer)
	(log_archive :pointer)
	(log_cursor :pointer)
	(log_file :pointer)
	(log_flush :pointer)
	(log_printf :pointer)
	(log_put :pointer)
	(log_stat :pointer)
	(log_stat_print :pointer)
	(lsn_reset :pointer)
	(memp_fcreate :pointer)
	(memp_register :pointer)
	(memp_stat :pointer)
	(memp_stat_print :pointer)
	(memp_sync :pointer)
	(memp_trickle :pointer)
	(mutex_alloc :pointer)
	(mutex_free :pointer)
	(mutex_get_align :pointer)
	(mutex_get_increment :pointer)
	(mutex_get_max :pointer)
	(mutex_get_tas_spins :pointer)
	(mutex_lock :pointer)
	(mutex_set_align :pointer)
	(mutex_set_increment :pointer)
	(mutex_set_max :pointer)
	(mutex_set_tas_spins :pointer)
	(mutex_stat :pointer)
	(mutex_stat_print :pointer)
	(mutex_unlock :pointer)
	(open :pointer)
	(remove :pointer)
	(rep_elect :pointer)
	(rep_flush :pointer)
	(rep_get_config :pointer)
	(rep_get_limit :pointer)
	(rep_get_nsites :pointer)
	(rep_get_priority :pointer)
	(rep_get_timeout :pointer)
	(rep_process_message :pointer)
	(rep_set_config :pointer)
	(rep_set_lease :pointer)
	(rep_set_limit :pointer)
	(rep_set_nsites :pointer)
	(rep_set_priority :pointer)
	(rep_set_timeout :pointer)
	(rep_set_transport :pointer)
	(rep_start :pointer)
	(rep_stat :pointer)
	(rep_stat_print :pointer)
	(rep_sync :pointer)
	(repmgr_add_remote_site :pointer)
	(repmgr_get_ack_policy :pointer)
	(repmgr_set_ack_policy :pointer)
	(repmgr_set_local_site :pointer)
	(repmgr_site_list :pointer)
	(repmgr_start :pointer)
	(repmgr_stat :pointer)
	(repmgr_stat_print :pointer)
	(set_alloc :pointer)
	(set_app_dispatch :pointer)
	(set_cachesize :pointer)
	(set_cache_max :pointer)
	(set_data_dir :pointer)
	(set_encrypt :pointer)
	(set_errcall :pointer)
	(set_errfile :pointer)
	(set_errpfx :pointer)
	(set_event_notify :pointer)
	(set_feedback :pointer)
	(set_flags :pointer)
	(set_intermediate_dir :pointer)
	(set_isalive :pointer)
	(set_lg_bsize :pointer)
	(set_lg_dir :pointer)
	(set_lg_filemode :pointer)
	(set_lg_max :pointer)
	(set_lg_regionmax :pointer)
	(set_lk_conflicts :pointer)
	(set_lk_detect :pointer)
	(set_lk_max_lockers :pointer)
	(set_lk_max_locks :pointer)
	(set_lk_max_objects :pointer)
	(set_mp_max_openfd :pointer)
	(set_mp_max_write :pointer)
	(set_mp_mmapsize :pointer)
	(set_msgcall :pointer)
	(set_msgfile :pointer)
	(set_paniccall :pointer)
	(set_rep_request :pointer)
	(set_rpc_server :pointer)
	(set_shm_key :pointer)
	(set_thread_count :pointer)
	(set_thread_id :pointer)
	(set_thread_id_string :pointer)
	(set_timeout :pointer)
	(set_tmp_dir :pointer)
	(set_tx_max :pointer)
	(set_tx_timestamp :pointer)
	(set_verbose :pointer)
	(stat_print :pointer)
	(txn_begin :pointer)
	(txn_checkpoint :pointer)
	(txn_recover :pointer)
	(txn_stat :pointer)
	(txn_stat_print :pointer)
	(prdbt :pointer)
	(test_abort :int)
	(test_check :int)
	(test_copy :int)
	(flags :unsigned-int)
	(xa_txn :pointer)
	(links :pointer)
	(dblist :pointer)
	(fdlist :pointer)
	(mutex_iq :pointer))



(cl:defconstant DB_DBM_HSEARCH 0)

(cffi:defcfun ("db_create" db_create) :int
  (arg0 :pointer)
  (arg1 :pointer)
  (arg2 :unsigned-int))

(cffi:defcfun ("db_strerror" db_strerror) :string
  (arg0 :int))

(cffi:defcfun ("db_env_create" db_env_create) :int
  (arg0 :pointer)
  (arg1 :unsigned-int))

(cffi:defcfun ("db_version" db_version) :string
  (arg0 :pointer)
  (arg1 :pointer)
  (arg2 :pointer))

(cffi:defcfun ("log_compare" log_compare) :int
  (arg0 :pointer)
  (arg1 :pointer))

(cffi:defcfun ("db_env_set_func_close" db_env_set_func_close) :int
  (arg0 :pointer))

(cffi:defcfun ("db_env_set_func_dirfree" db_env_set_func_dirfree) :int
  (arg0 :pointer))

(cffi:defcfun ("db_env_set_func_dirlist" db_env_set_func_dirlist) :int
  (arg0 :pointer))

(cffi:defcfun ("db_env_set_func_exists" db_env_set_func_exists) :int
  (arg0 :pointer))

(cffi:defcfun ("db_env_set_func_free" db_env_set_func_free) :int
  (arg0 :pointer))

(cffi:defcfun ("db_env_set_func_fsync" db_env_set_func_fsync) :int
  (arg0 :pointer))

(cffi:defcfun ("db_env_set_func_ftruncate" db_env_set_func_ftruncate) :int
  (arg0 :pointer))

(cffi:defcfun ("db_env_set_func_ioinfo" db_env_set_func_ioinfo) :int
  (arg0 :pointer))

(cffi:defcfun ("db_env_set_func_malloc" db_env_set_func_malloc) :int
  (arg0 :pointer))

(cffi:defcfun ("db_env_set_func_map" db_env_set_func_map) :int
  (arg0 :pointer))

(cffi:defcfun ("db_env_set_func_pread" db_env_set_func_pread) :int
  (arg0 :pointer))

(cffi:defcfun ("db_env_set_func_pwrite" db_env_set_func_pwrite) :int
  (arg0 :pointer))

(cffi:defcfun ("db_env_set_func_open" db_env_set_func_open) :int
  (arg0 :pointer))

(cffi:defcfun ("db_env_set_func_read" db_env_set_func_read) :int
  (arg0 :pointer))

(cffi:defcfun ("db_env_set_func_realloc" db_env_set_func_realloc) :int
  (arg0 :pointer))

(cffi:defcfun ("db_env_set_func_rename" db_env_set_func_rename) :int
  (arg0 :pointer))

(cffi:defcfun ("db_env_set_func_seek" db_env_set_func_seek) :int
  (arg0 :pointer))

(cffi:defcfun ("db_env_set_func_sleep" db_env_set_func_sleep) :int
  (arg0 :pointer))

(cffi:defcfun ("db_env_set_func_unlink" db_env_set_func_unlink) :int
  (arg0 :pointer))

(cffi:defcfun ("db_env_set_func_unmap" db_env_set_func_unmap) :int
  (arg0 :pointer))

(cffi:defcfun ("db_env_set_func_write" db_env_set_func_write) :int
  (arg0 :pointer))

(cffi:defcfun ("db_env_set_func_yield" db_env_set_func_yield) :int
  (arg0 :pointer))

(cffi:defcfun ("db_sequence_create" db_sequence_create) :int
  (arg0 :pointer)
  (arg1 :pointer)
  (arg2 :unsigned-int))

;dbt constants
(cl:defconstant DB_DBT_APPMALLOC        #x001   "Callback allocated memory.")
(cl:defconstant DB_DBT_DUPOK            #x002   "Insert if duplicate.")
(cl:defconstant DB_DBT_ISSET            #x004   "Lower level calls set value.")
(cl:defconstant DB_DBT_MALLOC           #x008   "Return in malloc'd memory.")
(cl:defconstant	DB_DBT_MULTIPLE		#x010	"References multiple records.")
(cl:defconstant DB_DBT_PARTIAL          #x020   "Partial put/get.")
(cl:defconstant DB_DBT_REALLOC          #x040   "Return in realloc'd memory.")
(cl:defconstant DB_DBT_USERCOPY         #x080   "Use the user-supplied callback.")
(cl:defconstant DB_DBT_USERMEM          #x100   "Return in user's memory.")


;;;; Footer functions and declarations
(defctype lisp-object :pointer)

(defmethod translate-to-foreign (value (name (eql 'lisp-object)))
  (foreign-alloc :int :initial-element (sb-kernel:get-lisp-obj-address value)))

  (defmethod translate-from-foreign (value (name (eql 'lisp-object)))
    (sb-kernel:make-lisp-obj (mem-aref value :int)))





