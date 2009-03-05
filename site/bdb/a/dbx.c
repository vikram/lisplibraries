/*
 * $Revision: 1.7 $
 * Copyright © 2001 Paul Foley (mycroft@actrix.gen.nz)
 * All rights reserved.  Use and verbatim redistribution permitted.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS
 * OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
 * OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
 * BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
 * USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
 */

#include <db.h>

/*************************************************************************/

int dbx_open(DB *db, DB_TXN *txnid, char *file, char *database, DBTYPE type,
	     u_int32_t flags, int mode)
{
    return db->open(db, txnid, file, database, type, flags, mode);
}

int dbx_set_flags(DB *db, u_int32_t flags)
{
    return db->set_flags(db, flags);
}

int dbx_set_encrypt(DB *db, char *pass, u_int32_t flags)
{
    return db->set_encrypt(db, pass, flags);
}

int dbx_close(DB *db, u_int32_t flags)
{
    return db->close(db, flags);
}

int dbx_put(DB *db, char *key, u_int32_t key_start, u_int32_t key_end,
	    char *datum, u_int32_t datum_start, u_int32_t datum_end,
	    DB_TXN *txnid, int partial, u_int32_t start, u_int32_t end,
	    db_recno_t *recno, u_int32_t flags)
{
    DBT xkey, xdatum;

    memset(&xkey, 0, sizeof(xkey));
    memset(&xdatum, 0, sizeof(xdatum));
    if (flags & DB_APPEND) {
	xkey.data = (void *)recno;
	xkey.ulen = sizeof(db_recno_t);
	xkey.flags = DB_DBT_USERMEM;
    } else {
	xkey.data = key + key_start;
	xkey.size = key_end - key_start;
	*recno = 0;
    }
    xdatum.data = datum + datum_start;
    xdatum.size = datum_end - datum_start;
    if (partial) {
	xdatum.doff = start;
	xdatum.dlen = end - start;
	xdatum.flags |= DB_DBT_PARTIAL;
    }

    return db->put(db, txnid, &xkey, &xdatum, flags);
}

int dbx_get(DB *db, char *key, u_int32_t key_start, u_int32_t key_end,
	    char *datum, u_int32_t datum_start, u_int32_t datum_end,
	    DB_TXN *txnid, int partial, u_int32_t start, u_int32_t end,
	    u_int32_t flags, void **result, u_int32_t *result_length)
{
    DBT xkey, xdatum;
    int ret;

    memset(&xkey, 0, sizeof(xkey));
    memset(&xdatum, 0, sizeof(xdatum));
    xkey.data = key + key_start;
    xkey.size = key_end - key_start;
    if (datum) {
	xdatum.data = datum + datum_start;
	xdatum.ulen = datum_end - datum_start;
	xdatum.flags |= DB_DBT_USERMEM;
    }
    if (partial) {
	xdatum.doff = start;
	xdatum.dlen = end - start;
	xdatum.flags |= DB_DBT_PARTIAL;
    }

    if ((ret = db->get(db, txnid, &xkey, &xdatum, flags)) != 0)
	return ret;

    *result = xdatum.data;
    *result_length = xdatum.size;
    return 0;
}

int dbx_del(DB *db, char *key, u_int32_t key_start, u_int32_t key_end,
	    DB_TXN *txnid, u_int32_t flags)
{
    DBT xkey;

    memset(&xkey, 0, sizeof(xkey));
    xkey.data = key + key_start;
    xkey.size = key_end - key_start;
    return db->del(db, txnid, &xkey, flags);
}

int dbx_truncate(DB *db, DB_TXN *txnid, u_int32_t *countp, u_int32_t flags)
{
    return db->truncate(db, txnid, countp, flags);
}

int dbx_remove(DB *db, char *file, char *database, u_int32_t flags)
{
    return db->remove(db, file, database, flags);
}

int dbx_rename(DB *db, char *file, char *database, char *newname,
	       u_int32_t flags)
{
    return db->rename(db, file, database, newname, flags);
}

int dbx_sync(DB *db, u_int32_t flags)
{
    return db->sync(db, flags);
}

int dbx_associate(DB *db, DB_TXN *txnid, DB *sec,
		  int (*cb)(DB *, const DBT *, const DBT *, DBT *),
		  u_int32_t flags)
{
    return db->associate(db, txnid, sec, cb, flags);
}

int dbx_set_feedback(DB *db, void (*cb)(DB *dbp, int opcode, int pct))
{
    return db->set_feedback(db, cb);
}

/*************************************************************************/

int dbx_cursor(DB *db, DB_TXN *txnid, DBC **cursor, u_int32_t flags)
{
    return db->cursor(db, txnid, cursor, flags);
}

int dbx_cursor_close(DBC *cursor)
{
    return cursor->c_close(cursor);
}

int dbx_cursor_count(DBC *cursor, db_recno_t *count, u_int32_t flags)
{
    return cursor->c_count(cursor, count, flags);
}

int dbx_cursor_del(DBC *cursor, u_int32_t flags)
{
    return cursor->c_del(cursor, flags);
}

int dbx_cursor_dup(DBC *cursor, DBC **duplicate, u_int32_t flags)
{
    return cursor->c_dup(cursor, duplicate, flags);
}

int dbx_cursor_get(DBC *cursor,
		   char *key, u_int32_t key_start, u_int32_t key_end,
		   char *datum, u_int32_t datum_start, u_int32_t datum_end,
		   int partial, u_int32_t start, u_int32_t end,
		   u_int32_t flags, void **datum_out, u_int32_t *datum_length,
		   void **key_out, u_int32_t *key_length)
{
    DBT xkey, xdatum;
    int ret;

    memset(&xkey, 0, sizeof(xkey));
    memset(&xdatum, 0, sizeof(xdatum));
    xkey.data = key + key_start;
    xkey.size = key_end - key_start;
    if (datum) {
	xdatum.data = datum + datum_start;
	xdatum.ulen = datum_end - datum_start;
	xdatum.flags |= DB_DBT_USERMEM;
    }
    if (partial) {
	xdatum.doff = start;
	xdatum.dlen = end - start;
	xdatum.flags |= DB_DBT_PARTIAL;
    }

    if ((ret = cursor->c_get(cursor, &xkey, &xdatum, flags)) != 0)
	return ret;

    *datum_out = xdatum.data;
    *datum_length = xdatum.size;
    *key_out = xkey.data;
    *key_length = xkey.size;
    return 0;
}

int dbx_cursor_put(DBC *cursor,
		   char *key, u_int32_t key_start, u_int32_t key_end,
		   char *datum, u_int32_t datum_start, u_int32_t datum_end,
		   int partial, u_int32_t start, u_int32_t end,
		   u_int32_t flags)
{
    DBT xkey, xdatum;

    memset(&xkey, 0, sizeof(xkey));
    memset(&xdatum, 0, sizeof(xdatum));
    xkey.data = key + key_start;
    xkey.size = key_end - key_start;
    xdatum.data = datum + datum_start;
    xdatum.size = datum_end - datum_start;
    if (partial) {
	xdatum.doff = start;
	xdatum.dlen = end - start;
	xdatum.flags |= DB_DBT_PARTIAL;
    }

    return cursor->c_put(cursor, &xkey, &xdatum, flags);
}

/*************************************************************************/

int dbx_txn_begin(DB_ENV *env, DB_TXN *parent, DB_TXN **txnid, u_int32_t flags)
{
    return env->txn_begin(env, parent, txnid, flags);
}

int dbx_txn_commit(DB_TXN *txnid, u_int32_t flags)
{
    return txnid->commit(txnid, flags);
}

int dbx_txn_abort(DB_TXN *txnid)
{
    return txnid->abort(txnid);
}

int dbx_checkpoint(DB_ENV *env, u_int32_t kbyte, u_int32_t min,
		   u_int32_t flags)
{
    return env->txn_checkpoint(env, kbyte, min, flags);
}

/*************************************************************************/

int dbx_env_set_encrypt(DB_ENV *env, char *pass, u_int32_t flags)
{
    return env->set_encrypt(env, pass, flags);
}

int dbx_env_open(DB_ENV *env, char *home, u_int32_t flags, int mode)
{
    return env->open(env, home, flags, mode);
}

int dbx_env_close(DB_ENV *env, u_int32_t flags)
{
    return env->close(env, flags);
}

int dbx_set_data_dir(DB_ENV *env, char *dir)
{
    return env->set_data_dir(env, dir);
}

int dbx_set_log_dir(DB_ENV *env, char *dir)
{
    return env->set_lg_dir(env, dir);
}

int dbx_set_tmp_dir(DB_ENV *env, char *dir)
{
    return env->set_tmp_dir(env, dir);
}

int dbx_env_remove(DB_ENV *env, char *home, u_int32_t flags)
{
    return env->remove(env, home, flags);
}

int dbx_env_dbremove(DB_ENV *env, DB_TXN *txnid, char *file, char *database,
		     u_int32_t flags)
{
    return env->dbremove(env, txnid, file, database, flags);
}

int dbx_env_dbrename(DB_ENV *env, DB_TXN *txnid, char *file, char *database,
		     char *newname, u_int32_t flags)
{
    return env->dbrename(env, txnid, file, database, newname, flags);
}

int dbx_lock_detect(DB_ENV *env, u_int32_t flags, u_int32_t atype,
		    int *aborted)
{
    return env->lock_detect(env, flags, atype, aborted);
}

int dbx_set_lk_detect(DB_ENV *env, u_int32_t detect)
{
    return env->set_lk_detect(env, detect);
}

int dbx_env_set_feedback(DB_ENV *env,
			 void (*cb)(DB_ENV *envp, int opcode, int pct))
{
    return env->set_feedback(env, cb);
}
