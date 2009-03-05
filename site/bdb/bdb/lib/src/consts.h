
//using copy&paste :)

#define	DB_MAX_PAGES	0xffffffff	/* >= # of pages in a file */

#define	DB_MAX_RECORDS	0xffffffff	/* >= # of records in a tree */

/*
 * Common flags --
 *	Interfaces which use any of these common flags should never have
 *	interface specific flags in this range.
 */
#define	DB_CREATE	      0x0000001	/* Create file as necessary. */
#define	DB_CXX_NO_EXCEPTIONS  0x0000002	/* C++: return error values. */
#define	DB_FORCE	      0x0000004	/* Force (anything). */
#define	DB_NOMMAP	      0x0000008	/* Don't mmap underlying file. */
#define	DB_RDONLY	      0x0000010	/* Read-only (O_RDONLY). */
#define	DB_RECOVER	      0x0000020	/* Run normal recovery. */
#define	DB_THREAD	      0x0000040	/* Applications are threaded. */
#define	DB_TRUNCATE	      0x0000080	/* Discard existing DB (O_TRUNC). */
#define	DB_TXN_NOSYNC	      0x0000100	/* Do not sync log on commit. */
#define	DB_TXN_NOT_DURABLE    0x0000200	/* Do not log changes. */
#define	DB_USE_ENVIRON	      0x0000400	/* Use the environment. */
#define	DB_USE_ENVIRON_ROOT   0x0000800	/* Use the environment if root. */

/*
 * Common flags --
 *	Interfaces which use any of these common flags should never have
 *	interface specific flags in this range.
 *
 * DB_AUTO_COMMIT:
 *	DB_ENV->set_flags, DB->associate, DB->del, DB->put, DB->open,
 *	DB->remove, DB->rename, DB->truncate
 * DB_DEGREE_2:
 *	DB->cursor, DB->get, DB->join, DBcursor->c_get, DB_ENV->txn_begin
 * DB_DIRTY_READ:
 *	DB->cursor, DB->get, DB->join, DB->open, DBcursor->c_get,
 *	DB_ENV->txn_begin
 * DB_NOAUTO_COMMIT
 *	DB->associate, DB->del, DB->put, DB->open,
 *	DB->remove, DB->rename, DB->truncate
 *
 * !!!
 * The DB_DIRTY_READ and DB_DEGREE_2 bit masks can't be changed without
 * also changing the masks for the flags that can be OR'd into DB
 * access method and cursor operation values.
 */
#define	DB_AUTO_COMMIT	      0x01000000/* Implied transaction. */
#define	DB_DEGREE_2	      0x02000000/* Degree 2. */
#define	DB_DIRTY_READ	      0x04000000/* Dirty Read. */
#define	DB_NO_AUTO_COMMIT     0x08000000/* Override env-wide AUTOCOMMIT. */

/*
 * Flags private to db_env_create.
 */
#define	DB_RPCCLIENT	      0x0000001	/* An RPC client environment. */

/*
 * Flags private to db_create.
 */
#define	DB_REP_CREATE	      0x0000001	/* Open of an internal rep database. */
#define	DB_XA_CREATE	      0x0000002	/* Open in an XA environment. */

/*
 * Flags private to DB_ENV->open.
 *	   Shared flags up to 0x0000800 */
#define	DB_INIT_CDB	      0x0001000	/* Concurrent Access Methods. */
#define	DB_INIT_LOCK	      0x0002000	/* Initialize locking. */
#define	DB_INIT_LOG	      0x0004000	/* Initialize logging. */
#define	DB_INIT_MPOOL	      0x0008000	/* Initialize mpool. */
#define	DB_INIT_REP	      0x0010000	/* Initialize replication. */
#define	DB_INIT_TXN	      0x0020000	/* Initialize transactions. */
#define	DB_JOINENV	      0x0040000	/* Initialize all subsystems present. */
#define	DB_LOCKDOWN	      0x0080000	/* Lock memory into physical core. */
#define	DB_PRIVATE	      0x0100000	/* DB_ENV is process local. */
#define	DB_RECOVER_FATAL      0x0200000	/* Run catastrophic recovery. */
#define	DB_SYSTEM_MEM	      0x0400000	/* Use system-backed memory. */

/*
 * Flags private to DB->open.
 *	   Shared flags up to 0x0000800 */
#define	DB_EXCL		      0x0001000	/* Exclusive open (O_EXCL). */
#define	DB_FCNTL_LOCKING      0x0002000	/* UNDOC: fcntl(2) locking. */
#define	DB_RDWRMASTER	      0x0004000	/* UNDOC: allow subdb master open R/W */
#define	DB_WRITEOPEN	      0x0008000	/* UNDOC: open with write lock. */

/*
 * Flags private to DB_ENV->txn_begin.
 *	   Shared flags up to 0x0000800 */
#define	DB_TXN_NOWAIT	      0x0001000	/* Do not wait for locks in this TXN. */
#define	DB_TXN_SYNC	      0x0002000	/* Always sync log on commit. */

/*
 * Flags private to DB_ENV->set_encrypt.
 */
#define	DB_ENCRYPT_AES	      0x0000001	/* AES, assumes SHA1 checksum */

/*
 * Flags private to DB_ENV->set_flags.
 *	   Shared flags up to 0x00000800 */
#define	DB_CDB_ALLDB	      0x00001000/* Set CDB locking per environment. */
#define	DB_DIRECT_DB	      0x00002000/* Don't buffer databases in the OS. */
#define	DB_DIRECT_LOG	      0x00004000/* Don't buffer log files in the OS. */
#define	DB_DSYNC_LOG	      0x00008000/* Set O_DSYNC on the log. */
#define	DB_LOG_AUTOREMOVE     0x00010000/* Automatically remove log files. */
#define	DB_LOG_INMEMORY       0x00020000/* Store logs in buffers in memory. */
#define	DB_NOLOCKING	      0x00040000/* Set locking/mutex behavior. */
#define	DB_NOPANIC	      0x00080000/* Set panic state per DB_ENV. */
#define	DB_OVERWRITE	      0x00100000/* Overwrite unlinked region files. */
#define	DB_PANIC_ENVIRONMENT  0x00200000/* Set panic state per environment. */
#define	DB_REGION_INIT	      0x00400000/* Page-fault regions on open. */
#define	DB_TIME_NOTGRANTED    0x00800000/* Return NOTGRANTED on timeout. */
/*	      Shared flags at 0x01000000 */
/*	      Shared flags at 0x02000000 */
/*	      Shared flags at 0x04000000 */
/*	      Shared flags at 0x08000000 */
#define	DB_TXN_WRITE_NOSYNC   0x10000000/* Write, don't sync, on txn commit. */
#define	DB_YIELDCPU	      0x20000000/* Yield the CPU (a lot). */

/*
 * Flags private to DB->set_feedback's callback.
 */
#define	DB_UPGRADE	      0x0000001	/* Upgrading. */
#define	DB_VERIFY	      0x0000002	/* Verifying. */

/*
 * Flags private to DB_MPOOLFILE->open.
 *	   Shared flags up to 0x0000800 */
#define	DB_DIRECT	      0x0001000	/* Don't buffer the file in the OS. */
#define	DB_DURABLE_UNKNOWN    0x0002000 /* internal: durability on open. */
#define	DB_EXTENT	      0x0004000	/* internal: dealing with an extent. */
#define	DB_ODDFILESIZE	      0x0008000	/* Truncate file to N * pgsize. */

/*
 * Flags private to DB->set_flags.
 */
#define	DB_CHKSUM	      0x0000001	/* Do checksumming */
#define	DB_DUP		      0x0000002	/* Btree, Hash: duplicate keys. */
#define	DB_DUPSORT	      0x0000004	/* Btree, Hash: duplicate keys. */
#define	DB_ENCRYPT	      0x0000008	/* Btree, Hash: duplicate keys. */
#define	DB_INORDER	      0x0000010	/* Queue: strict ordering on consume. */
#define	DB_RECNUM	      0x0000020	/* Btree: record numbers. */
#define	DB_RENUMBER	      0x0000040	/* Recno: renumber on insert/delete. */
#define	DB_REVSPLITOFF	      0x0000080	/* Btree: turn off reverse splits. */
#define	DB_SNAPSHOT	      0x0000100	/* Recno: snapshot the input. */

/*
 * Flags private to the DB_ENV->stat_print, DB->stat and DB->stat_print methods.
 */
#define	DB_STAT_ALL	      0x0000001	/* Print: Everything. */
#define	DB_STAT_CLEAR	      0x0000002	/* Clear stat after returning values. */
#define	DB_STAT_LOCK_CONF     0x0000004	/* Print: Lock conflict matrix. */
#define	DB_STAT_LOCK_LOCKERS  0x0000008	/* Print: Lockers. */
#define	DB_STAT_LOCK_OBJECTS  0x0000010	/* Print: Lock objects. */
#define	DB_STAT_LOCK_PARAMS   0x0000020	/* Print: Lock parameters. */
#define	DB_STAT_MEMP_HASH     0x0000040	/* Print: Mpool hash buckets. */
#define	DB_STAT_SUBSYSTEM     0x0000080	/* Print: Subsystems too. */

/*
 * Flags private to DB->join.
 */
#define	DB_JOIN_NOSORT	      0x0000001	/* Don't try to optimize join. */

/*
 * Flags private to DB->verify.
 */
#define	DB_AGGRESSIVE	      0x0000001	/* Salvage whatever could be data.*/
#define	DB_NOORDERCHK	      0x0000002	/* Skip sort order/hashing check. */
#define	DB_ORDERCHKONLY	      0x0000004	/* Only perform the order check. */
#define	DB_PR_PAGE	      0x0000008	/* Show page contents (-da). */
#define	DB_PR_RECOVERYTEST    0x0000010	/* Recovery test (-dr). */
#define	DB_PRINTABLE	      0x0000020	/* Use printable format for salvage. */
#define	DB_SALVAGE	      0x0000040	/* Salvage what looks like data. */
#define	DB_UNREF	      0x0000080	/* Report unreferenced pages. */
/*
 * !!!
 * These must not go over 0x8000, or they will collide with the flags
 * used by __bam_vrfy_subtree.
 */

/*
 * Flags private to DB->set_rep_transport's send callback.
 */
#define	DB_REP_NOBUFFER	      0x0000001	/* Do not buffer this message. */
#define	DB_REP_PERMANENT      0x0000002	/* Important--app. may want to flush. */

/*******************************************************
 * Locking.
 *******************************************************/
#define	DB_LOCKVERSION	1

#define	DB_FILE_ID_LEN		20	/* Unique file ID length. */

/*
 * Deadlock detector modes; used in the DB_ENV structure to configure the
 * locking subsystem.
 */
#define	DB_LOCK_NORUN		0
#define	DB_LOCK_DEFAULT		1	/* Default policy. */
#define	DB_LOCK_EXPIRE		2	/* Only expire locks, no detection. */
#define	DB_LOCK_MAXLOCKS	3	/* Select locker with max locks. */
#define	DB_LOCK_MAXWRITE	4	/* Select locker with max writelocks. */
#define	DB_LOCK_MINLOCKS	5	/* Select locker with min locks. */
#define	DB_LOCK_MINWRITE	6	/* Select locker with min writelocks. */
#define	DB_LOCK_OLDEST		7	/* Select oldest locker. */
#define	DB_LOCK_RANDOM		8	/* Select random locker. */
#define	DB_LOCK_YOUNGEST	9	/* Select youngest locker. */

/* Flag values for lock_vec(), lock_get(). */
#define	DB_LOCK_ABORT		0x001	/* Internal: Lock during abort. */
#define	DB_LOCK_NOWAIT		0x002	/* Don't wait on unavailable lock. */
#define	DB_LOCK_RECORD		0x004	/* Internal: record lock. */
#define	DB_LOCK_REMOVE		0x008	/* Internal: flag object removed. */
#define	DB_LOCK_SET_TIMEOUT	0x010	/* Internal: set lock timeout. */
#define	DB_LOCK_SWITCH		0x020	/* Internal: switch existing lock. */
#define	DB_LOCK_UPGRADE		0x040	/* Internal: upgrade existing lock. */

/*
 * Simple R/W lock modes and for multi-granularity intention locking.
 *
 * !!!
 * These values are NOT random, as they are used as an index into the lock
 * conflicts arrays, i.e., DB_LOCK_IWRITE must be == 3, and DB_LOCK_IREAD
 * must be == 4.
 */
typedef enum {
	DB_LOCK_NG=0,			/* Not granted. */
	DB_LOCK_READ=1,			/* Shared/read. */
	DB_LOCK_WRITE=2,		/* Exclusive/write. */
	DB_LOCK_WAIT=3,			/* Wait for event */
	DB_LOCK_IWRITE=4,		/* Intent exclusive/write. */
	DB_LOCK_IREAD=5,		/* Intent to share/read. */
	DB_LOCK_IWR=6,			/* Intent to read and write. */
	DB_LOCK_DIRTY=7,		/* Dirty Read. */
	DB_LOCK_WWRITE=8		/* Was Written. */
} db_lockmode_t;

/*
 * Request types.
 */
typedef enum {
	DB_LOCK_DUMP=0,			/* Display held locks. */
	DB_LOCK_GET=1,			/* Get the lock. */
	DB_LOCK_GET_TIMEOUT=2,		/* Get lock with a timeout. */
	DB_LOCK_INHERIT=3,		/* Pass locks to parent. */
	DB_LOCK_PUT=4,			/* Release the lock. */
	DB_LOCK_PUT_ALL=5,		/* Release locker's locks. */
	DB_LOCK_PUT_OBJ=6,		/* Release locker's locks on obj. */
	DB_LOCK_PUT_READ=7,		/* Release locker's read locks. */
	DB_LOCK_TIMEOUT=8,		/* Force a txn to timeout. */
	DB_LOCK_TRADE=9,		/* Trade locker ids on a lock. */
	DB_LOCK_UPGRADE_WRITE=10	/* Upgrade writes for dirty reads. */
} db_lockop_t;

/*
 * Status of a lock.
 */
typedef enum  {
	DB_LSTAT_ABORTED=1,		/* Lock belongs to an aborted txn. */
	DB_LSTAT_EXPIRED=2,		/* Lock has expired. */
	DB_LSTAT_FREE=3,		/* Lock is unallocated. */
	DB_LSTAT_HELD=4,		/* Lock is currently held. */
	DB_LSTAT_NOTEXIST=5,		/* Object on which lock was waiting
					 * was removed */
	DB_LSTAT_PENDING=6,		/* Lock was waiting and has been
					 * promoted; waiting for the owner
					 * to run and upgrade it to held. */
	DB_LSTAT_WAITING=7		/* Lock is on the wait queue. */
}db_status_t;

/*******************************************************
 * Logging.
 *******************************************************/
#define	DB_LOGVERSION	10		/* Current log version. */
#define	DB_LOGOLDVER	10		/* Oldest log version supported. */
#define	DB_LOGMAGIC	0x040988

/* Flag values for DB_ENV->log_archive(). */
#define	DB_ARCH_ABS	0x001		/* Absolute pathnames. */
#define	DB_ARCH_DATA	0x002		/* Data files. */
#define	DB_ARCH_LOG	0x004		/* Log files. */
#define	DB_ARCH_REMOVE	0x008	/* Remove log files. */

/* Flag values for DB_ENV->log_put(). */
#define	DB_FLUSH		0x001	/* Flush data to disk (public). */
#define	DB_LOG_CHKPNT		0x002	/* Flush supports a checkpoint */
#define	DB_LOG_COMMIT		0x004	/* Flush supports a commit */
#define	DB_LOG_NOCOPY		0x008	/* Don't copy data */
#define	DB_LOG_NOT_DURABLE	0x010	/* Do not log; keep in memory */
#define	DB_LOG_PERM		0x020	/* Flag record with REP_PERMANENT */
#define	DB_LOG_RESEND		0x040	/* Resent log record */
#define	DB_LOG_WRNOSYNC		0x080	/* Write, don't sync log_put */

/*
 * Application-specified log record types start at DB_user_BEGIN, and must not
 * equal or exceed DB_debug_FLAG.
 *
 * DB_debug_FLAG is the high-bit of the u_int32_t that specifies a log record
 * type.  If the flag is set, it's a log record that was logged for debugging
 * purposes only, even if it reflects a database change -- the change was part
 * of a non-durable transaction.
 */
#define	DB_user_BEGIN		10000
#define	DB_debug_FLAG		0x80000000

/* DB_TXN flags
 */
#define	TXN_CHILDCOMMIT	0x001		/* Transaction that has committed. */
#define	TXN_COMPENSATE	0x002		/* Compensating transaction. */
#define	TXN_DEADLOCK	0x004		/* Transaction has deadlocked. */
#define	TXN_DEGREE_2	0x008		/* Has degree 2 isolation. */
#define	TXN_DIRTY_READ	0x010		/* Transaction does dirty reads. */
#define	TXN_LOCKTIMEOUT	0x020		/* Transaction has a lock timeout. */
#define	TXN_MALLOC	0x040		/* Structure allocated by TXN system. */
#define	TXN_NOSYNC	0x080		/* Do not sync on prepare and commit. */
#define	TXN_NOWAIT	0x100		/* Do not wait on locks. */
#define	TXN_RESTORED	0x200		/* Transaction has been restored. */
#define	TXN_SYNC	0x400		/* Sync on prepare and commit. */

/*
 * DB access method and cursor operation values.  Each value is an operation
 * code to which additional bit flags are added.
 */
#define	DB_AFTER		 1	/* c_put() */
#define	DB_APPEND		 2	/* put() */
#define	DB_BEFORE		 3	/* c_put() */
#define	DB_CACHED_COUNTS	 4	/* stat() */
#define	DB_CONSUME		 5	/* get() */
#define	DB_CONSUME_WAIT		 6	/* get() */
#define	DB_CURRENT		 7	/* c_get(), c_put(), DB_LOGC->get() */
#define	DB_FAST_STAT		 8	/* stat() */
#define	DB_FIRST		 9	/* c_get(), DB_LOGC->get() */
#define	DB_GET_BOTH		10	/* get(), c_get() */
#define	DB_GET_BOTHC		11	/* c_get() (internal) */
#define	DB_GET_BOTH_RANGE	12	/* get(), c_get() */
#define	DB_GET_RECNO		13	/* c_get() */
#define	DB_JOIN_ITEM		14	/* c_get(); do not do primary lookup */
#define	DB_KEYFIRST		15	/* c_put() */
#define	DB_KEYLAST		16	/* c_put() */
#define	DB_LAST			17	/* c_get(), DB_LOGC->get() */
#define	DB_NEXT			18	/* c_get(), DB_LOGC->get() */
#define	DB_NEXT_DUP		19	/* c_get() */
#define	DB_NEXT_NODUP		20	/* c_get() */
#define	DB_NODUPDATA		21	/* put(), c_put() */
#define	DB_NOOVERWRITE		22	/* put() */
#define	DB_NOSYNC		23	/* close() */
#define	DB_POSITION		24	/* c_dup() */
#define	DB_PREV			25	/* c_get(), DB_LOGC->get() */
#define	DB_PREV_NODUP		26	/* c_get(), DB_LOGC->get() */
#define	DB_RECORDCOUNT		27	/* stat() */
#define	DB_SET			28	/* c_get(), DB_LOGC->get() */
#define	DB_SET_LOCK_TIMEOUT	29	/* set_timout() */
#define	DB_SET_RANGE		30	/* c_get() */
#define	DB_SET_RECNO		31	/* get(), c_get() */
#define	DB_SET_TXN_NOW		32	/* set_timout() (internal) */
#define	DB_SET_TXN_TIMEOUT	33	/* set_timout() */
#define	DB_UPDATE_SECONDARY	34	/* c_get(), c_del() (internal) */
#define	DB_WRITECURSOR		35	/* cursor() */
#define	DB_WRITELOCK		36	/* cursor() (internal) */

/* This has to change when the max opcode hits 255. */
#define	DB_OPFLAGS_MASK	0x000000ff	/* Mask for operations flags. */

/*
 * Masks for flags that can be OR'd into DB access method and cursor
 * operation values.
 *
 *	DB_DIRTY_READ	0x04000000	   Dirty Read. */
#define	DB_MULTIPLE	0x08000000	/* Return multiple data values. */
#define	DB_MULTIPLE_KEY	0x10000000	/* Return multiple data/key pairs. */
#define	DB_RMW		0x20000000	/* Acquire write flag immediately. */

#define	DBC_ACTIVE	 0x0001		/* Cursor in use. */
#define	DBC_COMPENSATE	 0x0002		/* Cursor compensating, don't lock. */
#define	DBC_DEGREE_2	 0x0004		/* Cursor has degree 2 isolation. */
#define	DBC_DIRTY_READ	 0x0008		/* Cursor supports dirty reads. */
#define	DBC_OPD		 0x0010		/* Cursor references off-page dups. */
#define	DBC_RECOVER	 0x0020		/* Recovery cursor; don't log/lock. */
#define	DBC_RMW		 0x0040		/* Acquire write flag in read op. */
#define	DBC_TRANSIENT	 0x0080		/* Cursor is transient. */
#define	DBC_WRITECURSOR	 0x0100		/* Cursor may be used to write (CDB). */
#define	DBC_WRITER	 0x0200		/* Cursor immediately writing (CDB). */
#define	DBC_MULTIPLE	 0x0400		/* Return Multiple data. */
#define	DBC_MULTIPLE_KEY 0x0800		/* Return Multiple keys and data. */
#define	DBC_OWN_LID	 0x1000		/* Free lock id on destroy. */

#define	DB_ENV_AUTO_COMMIT	0x0000001 /* DB_AUTO_COMMIT. */
#define	DB_ENV_CDB		0x0000002 /* DB_INIT_CDB. */
#define	DB_ENV_CDB_ALLDB	0x0000004 /* CDB environment wide locking. */
#define	DB_ENV_CREATE		0x0000008 /* DB_CREATE set. */
#define	DB_ENV_DBLOCAL		0x0000010 /* DB_ENV allocated for private DB. */
#define	DB_ENV_DIRECT_DB	0x0000020 /* DB_DIRECT_DB set. */
#define	DB_ENV_DIRECT_LOG	0x0000040 /* DB_DIRECT_LOG set. */
#define	DB_ENV_DSYNC_LOG	0x0000080 /* DB_DSYNC_LOG set. */
#define	DB_ENV_FATAL		0x0000100 /* Doing fatal recovery in env. */
#define	DB_ENV_LOCKDOWN		0x0000200 /* DB_LOCKDOWN set. */
#define	DB_ENV_LOG_AUTOREMOVE   0x0000400 /* DB_LOG_AUTOREMOVE set. */
#define	DB_ENV_LOG_INMEMORY     0x0000800 /* DB_LOG_INMEMORY set. */
#define	DB_ENV_NOLOCKING	0x0001000 /* DB_NOLOCKING set. */
#define	DB_ENV_NOMMAP		0x0002000 /* DB_NOMMAP set. */
#define	DB_ENV_NOPANIC		0x0004000 /* Okay if panic set. */
#define	DB_ENV_OPEN_CALLED	0x0008000 /* DB_ENV->open called. */
#define	DB_ENV_OVERWRITE	0x0010000 /* DB_OVERWRITE set. */
#define	DB_ENV_PRIVATE		0x0020000 /* DB_PRIVATE set. */
#define	DB_ENV_REGION_INIT	0x0040000 /* DB_REGION_INIT set. */
#define	DB_ENV_RPCCLIENT	0x0080000 /* DB_RPCCLIENT set. */
#define	DB_ENV_RPCCLIENT_GIVEN	0x0100000 /* User-supplied RPC client struct */
#define	DB_ENV_SYSTEM_MEM	0x0200000 /* DB_SYSTEM_MEM set. */
#define	DB_ENV_THREAD		0x0400000 /* DB_THREAD set. */
#define	DB_ENV_TIME_NOTGRANTED	0x0800000 /* DB_TIME_NOTGRANTED set. */
#define	DB_ENV_TXN_NOSYNC	0x1000000 /* DB_TXN_NOSYNC set. */
#define	DB_ENV_TXN_WRITE_NOSYNC	0x2000000 /* DB_TXN_WRITE_NOSYNC set. */
#define	DB_ENV_YIELDCPU		0x4000000 /* DB_YIELDCPU set. */
