
(in-package :bdb)

;;;; ** Not yet supported/implemented Constants/Flags

(defconstant DB_MAX_PAGES #xffffffff)

(defconstant DB_MAX_RECORDS #xffffffff)

(defconstant DB_CXX_NO_EXCEPTIONS #x0000002)

(defconstant DB_FORCE #x0000004)

(defconstant DB_DEGREE_2 #x02000000)

(defconstant DB_DIRTY_READ #x04000000)

(defconstant DB_NO_AUTO_COMMIT #x08000000)

(defconstant DB_REP_CREATE #x0000001)

(defconstant DB_JOINENV #x0040000)

(defconstant DB_FCNTL_LOCKING #x0002000)

(defconstant DB_RDWRMASTER #x0004000)

(defconstant DB_WRITEOPEN #x0008000)



(defconstant DB_CDB_ALLDB #x00001000)

(defconstant DB_DIRECT_DB #x00002000)

(defconstant DB_DIRECT_LOG #x00004000)

(defconstant DB_DSYNC_LOG #x00008000)

(defconstant DB_LOG_AUTOREMOVE #x00010000)

(defconstant DB_LOG_INMEMORY #x00020000)

(defconstant DB_NOLOCKING #x00040000)

(defconstant DB_NOPANIC #x00080000)

(defconstant DB_OVERWRITE #x00100000)

(defconstant DB_PANIC_ENVIRONMENT #x00200000)

(defconstant DB_REGION_INIT #x00400000)

(defconstant DB_TIME_NOTGRANTED #x00800000)

(defconstant DB_TXN_WRITE_NOSYNC #x10000000)

(defconstant DB_YIELDCPU #x20000000)

(defconstant DB_UPGRADE #x0000001)

(defconstant DB_VERIFY #x0000002)

(defconstant DB_DIRECT #x0001000)

(defconstant DB_DURABLE_UNKNOWN #x0002000)

(defconstant DB_EXTENT #x0004000)

(defconstant DB_ODDFILESIZE #x0008000)

(defconstant DB_STAT_ALL #x0000001)

(defconstant DB_STAT_CLEAR #x0000002)

(defconstant DB_STAT_LOCK_CONF #x0000004)

(defconstant DB_STAT_LOCK_LOCKERS #x0000008)

(defconstant DB_STAT_LOCK_OBJECTS #x0000010)

(defconstant DB_STAT_LOCK_PARAMS #x0000020)

(defconstant DB_STAT_MEMP_HASH #x0000040)

(defconstant DB_STAT_SUBSYSTEM #x0000080)

(defconstant DB_JOIN_NOSORT #x0000001)

(defconstant DB_AGGRESSIVE #x0000001)

(defconstant DB_NOORDERCHK #x0000002)

(defconstant DB_ORDERCHKONLY #x0000004)

(defconstant DB_PR_PAGE #x0000008)

(defconstant DB_PR_RECOVERYTEST #x0000010)

(defconstant DB_PRINTABLE #x0000020)

(defconstant DB_SALVAGE #x0000040)

(defconstant DB_UNREF #x0000080)

(defconstant DB_REP_NOBUFFER #x0000001)

(defconstant DB_REP_PERMANENT #x0000002)

(defconstant DB_LOCKVERSION 1)

(defconstant DB_FILE_ID_LEN 20)

(defconstant DB_LOCK_NORUN 0)


(defconstant DB_LOCK_ABORT #x001)

(defconstant DB_LOCK_NOWAIT #x002)

(defconstant DB_LOCK_RECORD #x004)

(defconstant DB_LOCK_REMOVE #x008)

(defconstant DB_LOCK_SET_TIMEOUT #x010)

(defconstant DB_LOCK_SWITCH #x020)

(defconstant DB_LOCK_UPGRADE #x040)

(defcenum db_lockmode_t
	(:DB_LOCK_NG 0)
	(:DB_LOCK_READ 1)
	(:DB_LOCK_WRITE 2)
	(:DB_LOCK_WAIT 3)
	(:DB_LOCK_IWRITE 4)
	(:DB_LOCK_IREAD 5)
	(:DB_LOCK_IWR 6)
	(:DB_LOCK_DIRTY 7)
	(:DB_LOCK_WWRITE 8))

(defcenum db_lockop_t
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

(defcenum db_status_t
	(:DB_LSTAT_ABORTED 1)
	(:DB_LSTAT_EXPIRED 2)
	(:DB_LSTAT_FREE 3)
	(:DB_LSTAT_HELD 4)
	(:DB_LSTAT_NOTEXIST 5)
	(:DB_LSTAT_PENDING 6)
	(:DB_LSTAT_WAITING 7))

(defconstant DB_LOGVERSION 10)

(defconstant DB_LOGOLDVER 10)

(defconstant DB_LOGMAGIC #x040988)

(defconstant DB_ARCH_ABS #x001)

(defconstant DB_ARCH_DATA #x002)

(defconstant DB_ARCH_LOG #x004)

(defconstant DB_ARCH_REMOVE #x008)

(defconstant DB_FLUSH #x001)

(defconstant DB_LOG_CHKPNT #x002)

(defconstant DB_LOG_COMMIT #x004)

(defconstant DB_LOG_NOCOPY #x008)

(defconstant DB_LOG_NOT_DURABLE #x010)

(defconstant DB_LOG_PERM #x020)

(defconstant DB_LOG_RESEND #x040)

(defconstant DB_LOG_WRNOSYNC #x080)

(defconstant DB_user_BEGIN 10000)

(defconstant DB_debug_FLAG #x80000000)

(defconstant TXN_CHILDCOMMIT #x001)

(defconstant TXN_COMPENSATE #x002)

(defconstant TXN_DEADLOCK #x004)

(defconstant TXN_DEGREE_2 #x008)

(defconstant TXN_DIRTY_READ #x010)

(defconstant TXN_LOCKTIMEOUT #x020)

(defconstant TXN_MALLOC #x040)

(defconstant TXN_NOSYNC #x080)

(defconstant TXN_NOWAIT #x100)

(defconstant TXN_RESTORED #x200)

(defconstant TXN_SYNC #x400)


(defconstant DB_CACHED_COUNTS 4)

(defconstant DB_FAST_STAT 8)


(defconstant DB_RECORDCOUNT 27)

(defconstant DB_SET_TXN_NOW 32)

(defconstant DB_UPDATE_SECONDARY 34)

(defconstant DB_WRITELOCK 36)

(defconstant DB_OPFLAGS_MASK #x000000ff)

(defconstant DBC_ACTIVE #x0001)

(defconstant DBC_COMPENSATE #x0002)

(defconstant DBC_DEGREE_2 #x0004)

(defconstant DBC_DIRTY_READ #x0008)

(defconstant DBC_OPD #x0010)

(defconstant DBC_RECOVER #x0020)

(defconstant DBC_RMW #x0040)

(defconstant DBC_TRANSIENT #x0080)

(defconstant DBC_WRITECURSOR #x0100)

(defconstant DBC_WRITER #x0200)

(defconstant DBC_MULTIPLE #x0400)

(defconstant DBC_MULTIPLE_KEY #x0800)

(defconstant DBC_OWN_LID #x1000)

(defconstant DB_ENV_AUTO_COMMIT #x0000001)

(defconstant DB_ENV_CDB #x0000002)

(defconstant DB_ENV_CDB_ALLDB #x0000004)

(defconstant DB_ENV_CREATE #x0000008)

(defconstant DB_ENV_DBLOCAL #x0000010)

(defconstant DB_ENV_DIRECT_DB #x0000020)

(defconstant DB_ENV_DIRECT_LOG #x0000040)

(defconstant DB_ENV_DSYNC_LOG #x0000080)

(defconstant DB_ENV_FATAL #x0000100)

(defconstant DB_ENV_LOCKDOWN #x0000200)

(defconstant DB_ENV_LOG_AUTOREMOVE #x0000400)

(defconstant DB_ENV_LOG_INMEMORY #x0000800)

(defconstant DB_ENV_NOLOCKING #x0001000)

(defconstant DB_ENV_NOMMAP #x0002000)

(defconstant DB_ENV_NOPANIC #x0004000)

(defconstant DB_ENV_OPEN_CALLED #x0008000)

(defconstant DB_ENV_OVERWRITE #x0010000)

(defconstant DB_ENV_PRIVATE #x0020000)

(defconstant DB_ENV_REGION_INIT #x0040000)

(defconstant DB_ENV_RPCCLIENT #x0080000)

(defconstant DB_ENV_RPCCLIENT_GIVEN #x0100000)

(defconstant DB_ENV_SYSTEM_MEM #x0200000)

(defconstant DB_ENV_THREAD #x0400000)

(defconstant DB_ENV_TIME_NOTGRANTED #x0800000)

(defconstant DB_ENV_TXN_NOSYNC #x1000000)

(defconstant DB_ENV_TXN_WRITE_NOSYNC #x2000000)

(defconstant DB_ENV_YIELDCPU #x4000000)