/*This software is Copyright (c) 2006, Steffen Siering.
<COMPANY> grants you the rights to distribute
and use this software as governed by the terms
of the Lisp Lesser GNU Public License
(http://opensource.franz.com/preamble.html),
known as the LLGPL.*/
 
#include "bdb.h"
#include <string.h>

int bdb_version_major(){ return DB_VERSION_MAJOR; }
int bdb_verions_minor(){ return DB_VERSION_MINOR; }
int bdb_version_patch(){ return DB_VERSION_PATCH; }
const char* bdb_version_string(){ return DB_VERSION_STRING; }

//db


char *bdb_version(int *major, int *minor, int *patch){
    return db_version(major, minor, patch); }

int bdb_create(DB **dbp, DB_ENV *dbenv, u_int32_t flags){
    return db_create(dbp, dbenv, flags); }

int db_open(DB *dbp, DB_TXN *txn, const char *file,
	const char *db, DBTYPE type, u_int32_t flags, int mode){
    return dbp->open(dbp, txn, file, db, type, flags, mode); }

int db_close(DB *db, u_int32_t flags){
    return db->close(db, flags); }

DB_ENV *db_get_env(DB *db){
    return db->get_env(db); }

int db_get_dbname(DB *db, const char **filenamemap, const char **dbnamep){
    return db->get_dbname(db, filenamemap, dbnamep); }

int db_get_open_flags(DB *db, u_int32_t *flagsp){
    return db->get_open_flags(db, flagsp); }

int db_get_transactional(DB *db){
    return db->get_transactional(db); }

int db_remove(DB *db, const char *dbfile, const char* dbname, u_int32_t flags){
    return db->remove(db, dbfile, dbname, flags); }

int db_rename(DB *db, const char *dbfile, const char *dbname,
	const char *newname, u_int32_t flags){
    return db->rename(db, dbfile, dbname, newname, flags); }

int db_put(DB *db, DB_TXN *txn, DBT *key, DBT *data, u_int32_t flags){
    return db->put(db, txn, key, data, flags); }

int db_get_type(DB *db, DBTYPE *type){
    return db->get_type(db, type); }

int db_put_raw(DB *db, DB_TXN *txn, 
	void *key, u_int32_t key_size,
	void *data, u_int32_t data_size,
	u_int32_t flags){
    DBT dbt_key, dbt_value;

    memset(&dbt_key, 0 , sizeof(DBT));
    memset(&dbt_value, 0, sizeof(DBT));
    dbt_key.data = key;
    dbt_key.size = key_size;
    dbt_value.data = data;
    dbt_value.size = data_size;

    return db_put(db, txn, &dbt_key, &dbt_value, flags);
}

int db_get(DB *db, DB_TXN *txn, DBT *key, DBT *data, u_int32_t flags){
    return db->get(db, txn, key, data, flags); }

int db_get_raw(DB *db, DB_TXN *txn,
	void *key, u_int32_t key_size,
	void *buffer, u_int32_t buffer_size,
	u_int32_t buffer_ulen,
	u_int32_t flags, 
	void **result_buffer,
	u_int32_t *result_size){
    DBT dbt_key, dbt_value;
    int ret;

    memset(&dbt_key, 0 , sizeof(DBT));
    memset(&dbt_value, 0, sizeof(DBT));
    dbt_key.data = key;
    dbt_key.size = key_size;
    dbt_value.data = buffer;
    dbt_value.size = buffer_size;
    dbt_value.ulen = buffer_ulen;
    dbt_value.flags |= DB_DBT_USERMEM;

    ret = db_get(db, txn, &dbt_key, &dbt_value, flags);
    *result_buffer = dbt_value.data;
    *result_size = dbt_value.size;
    return ret;
}

int db_cursor_put_raw(DBC *cursor, 
	void *key, u_int32_t key_size,
	void *data, u_int32_t data_size,
	u_int32_t flags){
    DBT dbt_key, dbt_value;

    memset(&dbt_key, 0 , sizeof(DBT));
    memset(&dbt_value, 0, sizeof(DBT));
    dbt_key.data = key;
    dbt_key.size = key_size;
    dbt_value.data = data;
    dbt_value.size = data_size;

    return db_cursor_put(cursor, &dbt_key, &dbt_value, flags);
}

int db_del(DB *db, DB_TXN *txn, void *key, u_int32_t key_size,  u_int32_t flags){
    DBT dbt_key;

    memset(&dbt_key, 0 , sizeof(DBT));
    dbt_key.data = key;
    dbt_key.size = key_size;
    return db->del(db, txn, &dbt_key, flags);
}


int db_truncate(DB *db, DB_TXN *txn, u_int32_t *countp, u_int32_t flags){
    return db->truncate(db, txn, countp, flags); }

int db_sync(DB *db, u_int32_t flags){
    return db->sync(db, flags); }

int db_cursor(DB *db, DB_TXN *txn, DBC **cursorp, u_int32_t flags){
    return db->cursor(db, txn, cursorp, flags); }

int db_cursor_close(DBC *cursor){
    return cursor->c_close(cursor); }

int db_cursor_count(DBC *cursor, db_recno_t *count, u_int32_t flags){
    return cursor->c_count(cursor, count, flags); }

int db_cursor_del(DBC *cursor, u_int32_t flags){
    return cursor->c_del(cursor, flags); }

int db_cursor_dup(DBC *cursor, DBC **dup_cursor, u_int32_t flags){
    return cursor->c_dup(cursor, dup_cursor, flags); }

int db_cursor_get(DBC *cursor, DBT *key, DBT *data, u_int32_t flags){
    return cursor->c_get(cursor, key, data, flags); }

int db_cursor_get_raw(DBC *cursor, 
	void *key, u_int32_t key_size,
	u_int32_t key_length,
	void *data, u_int32_t data_size,
	u_int32_t data_length,
	u_int32_t flags,
	void **kret_ptr,
	u_int32_t *kret_size,
	void **dret_ptr,
	u_int32_t *dret_size){
    DBT dbt_key, dbt_value;
    int ret;

    memset(&dbt_key, 0 , sizeof(DBT));
    memset(&dbt_value, 0, sizeof(DBT));
    dbt_key.data = key;
    dbt_key.size = key_size;
    dbt_key.ulen = key_length;
    dbt_key.flags |= DB_DBT_USERMEM;
    dbt_value.data = data;
    dbt_value.size = data_size;
    dbt_value.ulen = data_length;
    dbt_value.flags |= DB_DBT_USERMEM;

    ret = db_cursor_get(cursor, &dbt_key, &dbt_value, flags);
    *kret_ptr = dbt_key.data;
    *kret_size = dbt_key.size;
    *dret_ptr = dbt_value.data;
    *dret_size = dbt_value.size;
    return ret;
}

int db_cursor_put(DBC *cursor, DBT *key, DBT *data, u_int32_t flags){
    return cursor->c_put(cursor, key, data, flags); }

//secondary db
int db_associate(DB *primary, DB_TXN *txn, DB *secondary, 
	int (*callback) (DB *secondary, const DBT *key, const DBT *data, DBT *result),
	u_int32_t flags){
    return primary->associate(primary, txn, secondary, callback, flags); }

int dummy_callback(DB *secondary, const DBT *key, const DBT *data, DBT *result){
    return DB_DONOTINDEX; }

int db_dummy_associate(DB *primary, DB_TXN *txn, DB *secondary, u_int32_t flags){
    return primary->associate(primary, txn, secondary, dummy_callback, flags); }

int db_join(DB *primary, DBC **curs_list, DBC **dbcpm, u_int32_t flags){
    return primary->join(primary, curs_list, dbcpm, flags); }

int db_pget(DB *db, DB_TXN *txn, DBT *key, DBT *pkey, DBT *data, u_int32_t flags){
    return db->pget(db, txn, key, pkey, data, flags); }

int db_cursor_pget(DBC *cursor, DBT *key, DBT *pkey, DBT *data, u_int32_t flags){
    return cursor->c_pget(cursor, key, pkey, data, flags); }

int db_pget_raw(DB *db, DB_TXN *txn, 
	void *key, u_int32_t key_size,
	void *pkey, u_int32_t pkey_size, u_int32_t pkey_ulen,
	void *buffer, u_int32_t buffer_size, u_int32_t buffer_ulen,
	u_int32_t flags,
	void **ret_pkey, u_int32_t *ret_pkey_size,
	void **ret_buffer, u_int32_t *ret_buf_size){
    DBT dbt_key, dbt_pkey, dbt_value;
    int ret;

    memset(&dbt_key, 0 , sizeof(DBT));
    memset(&dbt_pkey, 0, sizeof(DBT));
    memset(&dbt_value, 0, sizeof(DBT));
    dbt_key.data = key;
    dbt_key.size = key_size;

    dbt_pkey.data = pkey;
    dbt_pkey.size = pkey_size;
    dbt_pkey.ulen = pkey_ulen;
    dbt_pkey.flags |= DB_DBT_USERMEM;

    dbt_value.data = buffer;
    dbt_value.size = buffer_size;
    dbt_value.ulen = buffer_ulen;
    dbt_value.flags |= DB_DBT_USERMEM;

    ret = db_pget(db, txn, &dbt_key, &dbt_pkey, &dbt_value, flags);
    *ret_pkey = dbt_pkey.data;
    *ret_pkey_size = dbt_pkey.size;
    *ret_buffer = dbt_value.data;
    *ret_buf_size = dbt_value.size;

    return ret;
}

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
	u_int32_t *dret_size){
    DBT dbt_key, dbt_pkey, dbt_value;
    int ret;

    memset(&dbt_key, 0 , sizeof(DBT));
    memset(&dbt_pkey, 0 , sizeof(DBT));
    memset(&dbt_value, 0, sizeof(DBT));
    dbt_key.data = key;
    dbt_key.size = key_size;
    dbt_key.ulen = key_length;
    dbt_key.flags |= DB_DBT_USERMEM;
    dbt_pkey.data = pkey;
    dbt_pkey.size = pkey_size;
    dbt_pkey.ulen = key_length;
    dbt_pkey.flags |= DB_DBT_USERMEM;
    dbt_value.data = data;
    dbt_value.size = data_size;
    dbt_value.ulen = data_length;
    dbt_value.flags |= DB_DBT_USERMEM;

    ret = db_cursor_pget(cursor, &dbt_key, &dbt_pkey, &dbt_value, flags);
    *kret_ptr = dbt_key.data;
    *kret_size = dbt_key.size;
    *pkey_ret_ptr = dbt_pkey.data;
    *pkey_ret_size = dbt_pkey.size;
    *dret_ptr = dbt_value.data;
    *dret_size = dbt_value.size;
    return ret;
}

//db config

int db_set_pagesize(DB *db, u_int32_t size){
    return db->set_pagesize(db, size); }

int db_get_pagesize(DB *db, u_int32_t *size){
    return db->get_pagesize(db, size); }

int db_set_flags(DB *db, u_int32_t flags){
    return db->set_flags(db, flags); }

int db_get_flags(DB *db, u_int32_t *flags){
    return db->get_flags(db, flags); }

int db_set_encrypt(DB *db, const char *pwd, u_int32_t flags){
    return db->set_encrypt(db, pwd, flags); }

int db_get_encrypt_flags(DB *db, u_int32_t *flags){
    return db->get_encrypt_flags(db, flags); }

int db_set_cachesize(DB *db, u_int32_t gbyte, u_int32_t bytes, int ncache){
    return db->set_cachesize(db, gbyte, bytes, ncache); }

int db_get_cachesize(DB *db, u_int32_t *gbyte, u_int32_t *bytes, int *ncache){
    return db->get_cachesize(db, gbyte, bytes, ncache); }

int db_env_set_cachesize(DB_ENV *db, u_int32_t gbyte, u_int32_t bytes, int ncache){
    return db->set_cachesize(db, gbyte, bytes, ncache); }

int db_env_get_cachesize(DB_ENV *dbenv, u_int32_t *gbyte, u_int32_t *bytes, int *ncache){
    return dbenv->get_cachesize(dbenv, gbyte, bytes, ncache); }

//db_env

int bdb_env_create(DB_ENV **env, u_int32_t flags){
    return db_env_create(env, flags); }

int db_env_open(DB_ENV *env, char *db_home, u_int32_t flags, int mode){
    return env->open(env, db_home, flags, mode); }

int db_env_close(DB_ENV *env, u_int32_t flags){
    return env->close(env, flags); }

int db_env_get_home(DB_ENV *env, const char **homep){
    return env->get_home(env, homep); }

int db_env_get_open_flags(DB_ENV *env, u_int32_t *flagsp){
    return env->get_open_flags(env, flagsp); }

int db_env_set_tmp_dir(DB_ENV *dbenv, const char *dir){
    return dbenv->set_tmp_dir(dbenv, dir); }

int db_env_get_tmp_dir(DB_ENV *dbenv, const char **dir){
    return dbenv->get_tmp_dir(dbenv, dir); }

//error handling
void db_set_errcall(DB *db, void (*db_errcall_fcn)
	(const DB_ENV *dbenv, const char *errpfx, const char *msg)){
    db->set_errcall(db, db_errcall_fcn); }

void db_set_errpfx(DB *db, const char *errpfx){
    db->set_errpfx(db, errpfx); }

void db_get_errpfx(DB *db, const char **errpfx){
    db->get_errpfx(db, errpfx); }

void db_env_set_errcall(DB_ENV *db, void (*db_errcall_fcn)
	(const DB_ENV *dbenv, const char *errpfx, const char *msg)){
    db->set_errcall(db, db_errcall_fcn); }

void db_env_set_errpfx(DB_ENV *db, const char *errpfx){
    db->set_errpfx(db, errpfx); }

void db_env_get_errpfx(DB_ENV *db, const char **errpfx){
    db->get_errpfx(db, errpfx); }

char *db_strerr(int error){
    return db_strerror(error);
}

//transactions

int db_env_txn_begin(DB_ENV *env, DB_TXN *parent, DB_TXN **tid, u_int32_t flags){
    return env->txn_begin(env, parent, tid, flags); }

int db_txn_abort(DB_TXN *tid){
    return tid->abort(tid); }

int db_txn_commit(DB_TXN *tid, u_int32_t flags){
    return tid->commit(tid, flags); }

u_int32_t db_txn_id(DB_TXN *txn){
    return txn->id(txn); }

u_int32_t db_txn_set_timeout(DB_TXN *tid, db_timeout_t timeout, u_int32_t flags){
    return tid->set_timeout(tid, timeout, flags); }

int db_env_set_tx_max(DB_ENV *dbenv, u_int32_t max){
    return dbenv->set_tx_max(dbenv, max); }

int db_env_get_tx_max(DB_ENV *dbenv, u_int32_t *max){
    return dbenv->get_tx_max(dbenv, max); }

int db_env_set_timeout(DB_ENV *dbenv, db_timeout_t timeout, u_int32_t flags){
    return dbenv->set_timeout(dbenv, timeout, flags); }

int db_env_get_timeout(DB_ENV *dbenv, db_timeout_t *timeout, u_int32_t flags){
    return dbenv->get_timeout(dbenv, timeout, flags); }

int db_env_txn_checkpoint(DB_ENV *env, u_int32_t kbyte, u_int32_t min,
	u_int32_t flags){
    return env->txn_checkpoint(env, kbyte, min, flags); }

int db_env_txn_recover(DB_ENV *dbenv, DB_PREPLIST preplist[],
	long count, long *ret, u_int32_t flags){
    return dbenv->txn_recover(dbenv, preplist, count, ret, flags); }

//locking

int db_env_set_lk_max_lockers(DB_ENV *dbenv, u_int32_t max){
    return dbenv->set_lk_max_lockers(dbenv, max); }

int db_env_get_lk_max_lockers(DB_ENV *dbenv, u_int32_t *max){
    return dbenv->get_lk_max_lockers(dbenv, max); }

int db_env_set_lk_max_locks(DB_ENV *dbenv, u_int32_t max){
    return dbenv->set_lk_max_locks(dbenv, max); }

int db_env_get_lk_max_locks(DB_ENV *dbenv, u_int32_t *max){
    return dbenv->get_lk_max_locks(dbenv, max); }

int db_env_set_lk_max_objects(DB_ENV *dbenv, u_int32_t max){
    return dbenv->set_lk_max_objects(dbenv, max); }

int db_env_get_lk_max_objects(DB_ENV *dbenv, u_int32_t *max){
    return dbenv->get_lk_max_objects(dbenv, max); }

int db_env_set_lk_detect(DB_ENV *dbenv, u_int32_t detect){
    return dbenv->set_lk_detect(dbenv, detect); }

int db_env_get_lk_detect(DB_ENV *dbenv, u_int32_t *detect){
    return dbenv->get_lk_detect(dbenv, detect); }

int db_env_lock_detect(DB_ENV *dbenv,
	u_int32_t flags, u_int32_t atype, int *aborted){
    return dbenv->lock_detect(dbenv, flags, atype, aborted); }

//logging
int db_env_log_archive(DB_ENV *dbenv, char **listp[], u_int32_t flags){
    return dbenv->log_archive(dbenv, listp, flags); }

int db_env_log_flush(DB_ENV *dbenv, const DB_LSN *lsn){
    return dbenv->log_flush(dbenv, lsn); }

int db_env_set_lg_max(DB_ENV *dbenv, u_int32_t max){
    return dbenv->set_lg_max(dbenv, max); }

int db_env_get_lg_max(DB_ENV *dbenv, u_int32_t *max){
    return dbenv->get_lg_max(dbenv, max); }

int db_env_set_lg_regionmax(DB_ENV *dbenv, u_int32_t max){
    return dbenv->set_lg_regionmax(dbenv, max); }

int db_env_get_lg_regionmax(DB_ENV *dbenv, u_int32_t *max){
    return dbenv->get_lg_regionmax(dbenv, max); }

int db_env_set_lg_bsize(DB_ENV *dbenv, u_int32_t bsize){
    return dbenv->set_lg_bsize(dbenv, bsize); }

int db_env_get_lg_bsize(DB_ENV *dbenv, u_int32_t *bsize){
    return dbenv->get_lg_bsize(dbenv, bsize); }


//sequences

int bdb_sequence_create(DB_SEQUENCE **seq, DB *db, u_int32_t flags){
    return db_sequence_create( seq, db, flags ); }

int db_sequence_close(DB_SEQUENCE *seq, u_int32_t flags){
    return seq->close( seq, flags ); }

int db_sequence_get(DB_SEQUENCE *seq, DB_TXN *txn, int32_t delta, 
	db_seq_t *retp, u_int32_t flags){
    return seq->get( seq, txn, delta, retp, flags); }

int db_sequence_get_i(DB_SEQUENCE *seq, DB_TXN *txn, int32_t delta, 
	uint32_t *retlower, int32_t *retupper, u_int32_t flags){
    int ret;
    int64_t sequ;

    ret = db_sequence_get( seq, txn, delta, &sequ, flags);
    if( ret ) return ret;

    *retlower = (uint32_t)(sequ & 0xffffffff);
    *retupper = (int32_t)(sequ >> 32);
    return ret;
}

int db_sequence_init_value(DB_SEQUENCE *seq, db_seq_t value){
    return seq->initial_value( seq, value ); }

db_seq_t make_db_seq_t(uint32_t lower, int32_t upper){
    return ((int64_t)upper << 32) & (int64_t)lower;  //not sure if this works with big-endian systems
}

int db_sequence_init_value_i(DB_SEQUENCE *seq, uint32_t lower, int32_t upper){
    return db_sequence_init_value(seq, make_db_seq_t(lower, upper)); }

int db_sequence_open(DB_SEQUENCE *seq, DB_TXN *txn, void *key,
	u_int32_t key_size, u_int32_t flags){

    DBT dbt_key;
    memset(&dbt_key, 0 , sizeof(DBT));
    dbt_key.data = key;
    dbt_key.size = key_size;

    return seq->open( seq, txn, &dbt_key, flags);
}

int db_sequence_remove(DB_SEQUENCE *seq, DB_TXN *txn, u_int32_t flags){
    return seq->remove( seq, txn, flags ); }

int db_sequence_set_cachesize(DB_SEQUENCE *seq, int32_t size){
    return seq->set_cachesize( seq, size ); }

int db_sequence_get_cachesize(DB_SEQUENCE *seq, int32_t *size){
    return seq->get_cachesize( seq, size ); }

int db_sequence_get_flags(DB_SEQUENCE *seq, u_int32_t *flags){
    return seq->get_flags( seq, flags ); }

int db_sequence_set_flags(DB_SEQUENCE *seq, u_int32_t flags){
    return seq->set_flags( seq, flags ); }

