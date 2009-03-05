/*This software is Copyright (c) 2006, Steffen Siering.
<COMPANY> grants you the rights to distribute
and use this software as governed by the terms
of the Lisp Lesser GNU Public License
(http://opensource.franz.com/preamble.html),
known as the LLGPL.*/

#ifndef _bdb_h_
#define _bdb_h_

#include <db.h>

//general

int bdb_version_major();
int bdb_verions_minor();
int bdb_version_patch();
const char* bdb_version_string();

char *bdb_version(int *major, int *minor, int *patch);

//db


int bdb_create(DB **dbp, DB_ENV *dbenv, u_int32_t flags);

int db_open(DB *dbp, DB_TXN *txn, const char *file,
	const char *db, DBTYPE type, u_int32_t flags, int mode);

int db_close(DB *db, u_int32_t flags);

DB_ENV *db_get_env(DB *db);

int db_get_dbname(DB *db,const char **filenamemap, const char **dbnamep);

int db_get_open_flags(DB *db, u_int32_t *flagsp);

int db_get_transactional(DB *db);

int db_remove(DB *db, const char *dbfile, const char* dbname, u_int32_t flags);

int db_rename(DB *db, const char *dbfile, const char *dbname,
	const char *newname, u_int32_t flags);

int db_put(DB *db, DB_TXN *txn, DBT *key, DBT *data, u_int32_t flags);

int db_put_raw(DB *db, DB_TXN *txn, 
	void *key_data, u_int32_t key_size,
	void *data, u_int32_t data_size,
	u_int32_t flags);

int db_get(DB *db, DB_TXN *txn, DBT *key, DBT *data, u_int32_t flags);

int db_get_raw(DB *db, DB_TXN *txn,
	void *key, u_int32_t key_size,
	void *buffer, u_int32_t buffer_size,
	u_int32_t buffer_ulen,
	u_int32_t flags, 
	void **result_buffer,
	u_int32_t *result_size);

int db_del(DB *db, DB_TXN *txn, void *key, u_int32_t key_size,  u_int32_t flags);

int db_truncate(DB *db, DB_TXN *txn, u_int32_t *countp, u_int32_t flags);

int db_sync(DB *db, u_int32_t flags);

int db_get_type(DB *db, DBTYPE *type);

//cursors
int db_cursor(DB *db, DB_TXN *txn, DBC **cursorp, u_int32_t flags);

int db_cursor_count(DBC *cursor, db_recno_t *count, u_int32_t flags);

int db_cursor_del(DBC *cursor, u_int32_t flags);

int db_cursor_close(DBC *cursor);

int db_cursor_dup(DBC *cursor, DBC **dup_cursor, u_int32_t flags);

int db_cursor_get(DBC *cursor, DBT *key, DBT *data, u_int32_t flags);

int db_cursor_get_raw(DBC *cursor, 
	void *key, u_int32_t key_size,
	u_int32_t key_length,
	void *data, u_int32_t data_size,
	u_int32_t data_length,
	u_int32_t flags,
	void **kret_ptr,
	u_int32_t *kret_size,
	void **dret_ptr,
	u_int32_t *dret_size);

int db_cursor_put(DBC *cursor, DBT *key, DBT *data, u_int32_t flags);

int db_cursor_put_raw(DBC *cursor, 
	void *key, u_int32_t key_size,
	void *data, u_int32_t data_size,
	u_int32_t flags);

//secondary db

int db_associate(DB *primary, DB_TXN *txn, DB *secondary, 
	int (*callback) (DB *secondary, const DBT *key, const DBT *data, DBT *result),
	u_int32_t flags);

int db_dummy_associate(DB *primary, DB_TXN *txn, DB *secondary, u_int32_t flags);

int db_join(DB *primary, DBC **curs_list, DBC **dbcpm, u_int32_t flags);

//int db_pget(DB *db, DB_TXN *txn, DBT *key, DBT *pkey, DBT *data, u_int32_t flags);

int db_pget_raw(DB *db, DB_TXN *txn, 
	void *key, u_int32_t key_size,
	void *pkey, u_int32_t pkey_size, u_int32_t pkey_ulen,
	void *buffer, u_int32_t buffer_size, u_int32_t buffer_ulen,
	u_int32_t flags,
	void **ret_pkey, u_int32_t *ret_pkey_size,
	void **ret_buffer, u_int32_t *ret_buf_size);

int db_cursor_pget(DBC *cursor, 
	DBT *key, 
	DBT *pkey, 
	DBT *data, 
	u_int32_t flags);

int db_cursor_pget_raw(DBC *cursor, 
	void *key, u_int32_t key_size,
	u_int32_t key_length,

	void *pkey, u_int32_t pkey_size,
	u_int32_t pkey_length,

	void *data, u_int32_t data_size,
	u_int32_t data_length,

	u_int32_t flags,

	void **kret_ptr,
	u_int32_t *kret_size,
	void **pkey_ret_ptr,
	u_int32_t *pkey_ret_size,
	void **dret_ptr,
	u_int32_t *dret_size);

//db config

int db_set_pagesize(DB *db, u_int32_t size);

int db_get_pagesize(DB *db, u_int32_t *size);

int db_set_flags(DB *db, u_int32_t flags);

int db_get_flags(DB *db, u_int32_t *flags);

int db_set_encrypt(DB *db, const char *pwd, u_int32_t flags);

int db_get_encrypt_flags(DB *db, u_int32_t *flags);

int db_set_cachesize(DB *db, u_int32_t gbyte, u_int32_t bytes, int ncache);

int db_get_cachesize(DB *db, u_int32_t *gbyte, u_int32_t *bytes, int *ncache);

int db_env_set_cachesize(DB_ENV *db, u_int32_t gbyte, u_int32_t bytes, int ncache);

int db_env_get_cachesize(DB_ENV *db, u_int32_t *gbyte, u_int32_t *bytes, int *ncache);

//environments

int bdb_env_create(DB_ENV **env, u_int32_t flags);

int db_env_open(DB_ENV *env, char *db_home, u_int32_t flags, int mode);

int db_env_close(DB_ENV *env, u_int32_t flags);

int db_env_get_home(DB_ENV *env, const char **homep);

int db_env_get_open_flags(DB_ENV *env, u_int32_t *flagsp);

int db_env_set_tmp_dir(DB_ENV *dbenv, const char *dir);

int db_env_get_tmp_dir(DB_ENV *dbenv, const char **dir);

//error handling
void db_set_errcall(DB *db, void (*db_errcall_fcn)
	(const DB_ENV *dbenv, const char *errpfx, const char *msg));

void db_set_errpfx(DB *db, const char *errpfx);

void db_get_errpfx(DB *db, const char **errpfx);

void db_env_set_errcall(DB_ENV *db, void (*db_errcall_fcn)
	(const DB_ENV *dbenv, const char *errpfx, const char *msg));

void db_env_set_errpfx(DB_ENV *db, const char *errpfx);

void db_env_get_errpfx(DB_ENV *db, const char **errpfx);

char *db_strerr(int error);

//transactions

int db_env_txn_begin(DB_ENV *env, DB_TXN *parent, DB_TXN **tid, u_int32_t flags);

int db_txn_abort(DB_TXN *tid);

int db_txn_commit(DB_TXN *tid, u_int32_t flags);

u_int32_t db_txn_id(DB_TXN *txn);

u_int32_t db_txn_set_timeout(DB_TXN *tid, db_timeout_t timeout, u_int32_t flags);

int db_env_set_tx_max(DB_ENV *dbenv, u_int32_t max);

int db_env_get_tx_max(DB_ENV *dbenv, u_int32_t *max);

int db_env_set_timeout(DB_ENV *dbenv, db_timeout_t timeout, u_int32_t flags);

int db_env_get_timeout(DB_ENV *dbenv, db_timeout_t *timeout, u_int32_t flags);

int db_env_txn_checkpoint(DB_ENV *env, u_int32_t kbyte, u_int32_t min,
	u_int32_t flags);

int db_env_txn_recover(DB_ENV *dbenv, DB_PREPLIST preplist[],
	long count, long *ret, u_int32_t flags);

//locking

int db_env_set_lk_max_lockers(DB_ENV *dbenv, u_int32_t max);

int db_env_get_lk_max_lockers(DB_ENV *dbenv, u_int32_t *max);

int db_env_set_lk_max_locks(DB_ENV *dbenv, u_int32_t max);

int db_env_get_lk_max_locks(DB_ENV *dbenv, u_int32_t *max);

int db_env_set_lk_max_objects(DB_ENV *dbenv, u_int32_t max);

int db_env_get_lk_max_objects(DB_ENV *dbenv, u_int32_t *max);

int db_env_set_lk_detect(DB_ENV *dbenv, u_int32_t detect);

int db_env_get_lk_detect(DB_ENV *dbenv, u_int32_t *detect);

int db_env_lock_detect(DB_ENV *env,
	u_int32_t flags, u_int32_t atype, int *aborted);

//logging
int db_env_log_archive(DB_ENV *dbenv, char **listp[], u_int32_t flags);

int db_env_log_flush(DB_ENV *dbenv, const DB_LSN *lsn);

int db_env_set_lg_max(DB_ENV *dbenv, u_int32_t max);

int db_env_get_lg_max(DB_ENV *dbenv, u_int32_t *max);

int db_env_set_lg_regionmax(DB_ENV *dbenv, u_int32_t max);

int db_env_get_lg_regionmax(DB_ENV *dbenv, u_int32_t *max);

int db_env_set_lg_bsize(DB_ENV *dbenv, u_int32_t bsize);

int db_env_get_lg_bsize(DB_ENV *dbenv, u_int32_t *bsize);

//sequences
int bdb_sequence_create(DB_SEQUENCE **seq, DB *db, u_int32_t flags);

int db_sequence_close(DB_SEQUENCE *seq, u_int32_t flags);

int db_sequence_get(DB_SEQUENCE *seq, DB_TXN *txn, int32_t delta, 
	db_seq_t *retp, u_int32_t flags);

int db_sequence_get_i(DB_SEQUENCE *seq, DB_TXN *txn, int32_t delta, 
	uint32_t *retlower, int32_t *retupper, u_int32_t flags);

int db_sequence_init_value(DB_SEQUENCE *seq, db_seq_t value);

int db_sequence_init_value_i(DB_SEQUENCE *seq, uint32_t lower, int32_t upper);

int db_sequence_open(DB_SEQUENCE *seq, DB_TXN *txn, void *key,
	u_int32_t key_size, u_int32_t flags);

int db_sequence_remove(DB_SEQUENCE *seq, DB_TXN *txn, u_int32_t flags);

int db_sequence_set_cachesize(DB_SEQUENCE *seq, int32_t size);

int db_sequence_get_cachesize(DB_SEQUENCE *seq, int32_t *size);

int db_sequence_get_flags(DB_SEQUENCE *seq, u_int32_t *flags);

int db_sequence_set_flags(DB_SEQUENCE *seq, u_int32_t flags);

#endif
