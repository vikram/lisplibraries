;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: BERKELEY-DB -*-

;;; $Revision: 1.2 $
;;; Copyright © 2002 Paul Foley (mycroft@actrix.gen.nz)
;;; All rights reserved.  Use and verbatim redistribution permitted.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE
;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
;;; OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
;;; USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
;;; DAMAGE.
#+CMU (ext:file-comment "$Header: /cvsroot/homepage/db/db-ffi.lisp,v 1.2 2002/12/17 08:13:01 anoncvs Exp $")

(in-package #:berkeley-db)

(def-alien-routine ("db_create" %db-create) c-call:int
  (handle sys:system-area-pointer :out)
  (env sys:system-area-pointer)
  (flags c-call:unsigned-int))

(def-alien-routine ("dbx_open" %db-open) c-call:int
  (handle sys:system-area-pointer)
  (txnid sys:system-area-pointer)
  (file c-call:c-string)
  (database c-call:c-string)
  (type db-type)
  (flags c-call:unsigned-int)
  (mode c-call:int))

(def-alien-routine ("dbx_set_flags" %db-set-flags) c-call:int
  (handle sys:system-area-pointer)
  (flags c-call:unsigned-int))

(def-alien-routine ("dbx_set_encrypt" %db-set-encrypt) c-call:int
  (handle sys:system-area-pointer)
  (password c-call:c-string)
  (flags c-call:unsigned-int))

(def-alien-routine ("dbx_close" %db-close) c-call:int
  (handle sys:system-area-pointer)
  (flags c-call:unsigned-int))

(def-alien-routine ("dbx_put" %db-put) c-call:int
  (handle sys:system-area-pointer)
  (key c-call:c-string)
  (kstart c-call:unsigned-int)
  (kend c-call:unsigned-int)
  (datum c-call:c-string)
  (dstart c-call:unsigned-int)
  (dend c-call:unsigned-int)
  (txnid sys:system-area-pointer)
  (partial c-call:int)
  (start c-call:unsigned-int)
  (end c-call:unsigned-int)
  (recno c-call:unsigned-int :out)
  (flags c-call:unsigned-int))

(def-alien-routine ("dbx_get" %db-get) c-call:int
  (handle sys:system-area-pointer)
  (key c-call:c-string)
  (kstart c-call:unsigned-int)
  (kend c-call:unsigned-int)
  (datum c-call:c-string)
  (dstart c-call:unsigned-int)
  (dend c-call:unsigned-int)
  (txnid sys:system-area-pointer)
  (partial c-call:int)
  (start c-call:unsigned-int)
  (end c-call:unsigned-int)
  (flags c-call:unsigned-int)
  (result sys:system-area-pointer :out)
  (rlength c-call:unsigned-int :out))

(def-alien-routine ("dbx_del" %db-del) c-call:int
  (handle sys:system-area-pointer)
  (key c-call:c-string)
  (kstart c-call:unsigned-int)
  (kend c-call:unsigned-int)
  (txnid sys:system-area-pointer)
  (flags c-call:unsigned-int))

(def-alien-routine ("dbx_truncate" %db-truncate) c-call:int
  (handle sys:system-area-pointer)
  (txnid sys:system-area-pointer)
  (countp c-call:unsigned-int :out)
  (flags c-call:unsigned-int))

(def-alien-routine ("dbx_remove" %db-remove) c-call:int
  (handle sys:system-area-pointer)
  (file c-call:c-string)
  (database c-call:c-string)
  (flags c-call:unsigned-int))

(def-alien-routine ("dbx_rename" %db-rename) c-call:int
  (handle sys:system-area-pointer)
  (file c-call:c-string)
  (database c-call:c-string)
  (new-name c-call:c-string)
  (flags c-call:unsigned-int))

(def-alien-routine ("dbx_sync" %db-sync) c-call:int
  (handle sys:system-area-pointer)
  (flags c-call:unsigned-int))

(def-alien-routine ("dbx_associate" %db-associate) c-call:int
  (handle sys:system-area-pointer)
  (txnid sys:system-area-pointer)
  (secondary sys:system-area-pointer)
  (callback sys:system-area-pointer)
  (flags c-call:unsigned-int))

(def-alien-routine ("dbx_set_feedback" %db-set-feedback) c-call:int
  (handle sys:system-area-pointer)
  (callback sys:system-area-pointer))

(def-alien-routine ("dbx_cursor" %db-cursor) c-call:int
  (db sys:system-area-pointer)
  (txnid sys:system-area-pointer)
  (cursor sys:system-area-pointer :out)
  (flags c-call:unsigned-int))

(def-alien-routine ("dbx_cursor_close" %db-cursor-close) c-call:int
  (cursor sys:system-area-pointer))

(def-alien-routine ("dbx_cursor_count" %db-cursor-count) c-call:int
  (cursor sys:system-area-pointer)
  (count c-call:unsigned-int :out)
  (flags c-call:unsigned-int))

(def-alien-routine ("dbx_cursor_del" %db-cursor-del) c-call:int
  (cursor sys:system-area-pointer)
  (flags c-call:unsigned-int))

(def-alien-routine ("dbx_cursor_dup" %db-cursor-dup) c-call:int
  (cursor sys:system-area-pointer)
  (result sys:system-area-pointer :out)
  (flags c-call:unsigned-int))

(def-alien-routine ("dbx_cursor_get" %db-cursor-get) c-call:int
  (cursor sys:system-area-pointer)
  (key c-call:c-string)
  (kstart c-call:unsigned-int)
  (kend c-call:unsigned-int)
  (datum c-call:c-string)
  (dstart c-call:unsigned-int)
  (dend c-call:unsigned-int)
  (partial c-call:int)
  (start c-call:unsigned-int)
  (end c-call:unsigned-int)
  (flags c-call:unsigned-int)
  (dout sys:system-area-pointer :out)
  (doutlen c-call:unsigned-int :out)
  (kout sys:system-area-pointer :out)
  (koutlen c-call:unsigned-int :out))

(def-alien-routine ("dbx_cursor_put" %db-cursor-put) c-call:int
  (cursor sys:system-area-pointer)
  (key c-call:c-string)
  (kstart c-call:unsigned-int)
  (kend c-call:unsigned-int)
  (datum c-call:c-string)
  (dstart c-call:unsigned-int)
  (dend c-call:unsigned-int)
  (partial c-call:int)
  (start c-call:unsigned-int)
  (end c-call:unsigned-int)
  (flags c-call:unsigned-int))


(def-alien-routine ("db_env_create" %db-env-create) c-call:int
  (dbenv sys:system-area-pointer :out)
  (flags c-call:unsigned-int))

(def-alien-routine ("dbx_env_set_encrypt" %db-env-set-encrypt) c-call:int
  (env sys:system-area-pointer)
  (password c-call:c-string)
  (flags c-call:unsigned-int))

(def-alien-routine ("dbx_env_set_feedback" %db-env-set-feedback) c-call:int
  (env sys:system-area-pointer)
  (callback sys:system-area-pointer))

(def-alien-routine ("dbx_env_open" %db-env-open) c-call:int
  (env sys:system-area-pointer)
  (home c-call:c-string)
  (flags c-call:unsigned-int)
  (mode c-call:int))

(def-alien-routine ("dbx_env_close" %db-env-close) c-call:int
  (env sys:system-area-pointer)
  (flags c-call:unsigned-int))

(def-alien-routine ("dbx_set_data_dir" %db-set-data-dir) c-call:int
  (env sys:system-area-pointer)
  (dir c-call:c-string))

(def-alien-routine ("dbx_set_log_dir" %db-set-log-dir) c-call:int
  (env sys:system-area-pointer)
  (dir c-call:c-string))

(def-alien-routine ("dbx_set_tmp_dir" %db-set-tmp-dir) c-call:int
  (env sys:system-area-pointer)
  (dir c-call:c-string))

(def-alien-routine ("dbx_env_remove" %db-env-remove) c-call:int
  (env sys:system-area-pointer)
  (home c-call:c-string)
  (flags c-call:unsigned-int))

(def-alien-routine ("dbx_env_dbremove" %db-env-dbremove) c-call:int
  (env sys:system-area-pointer)
  (txnid sys:system-area-pointer)
  (file c-call:c-string)
  (database c-call:c-string)
  (flags c-call:unsigned-int))

(def-alien-routine ("dbx_env_dbrename" %db-env-dbrename) c-call:int
  (env sys:system-area-pointer)
  (txnid sys:system-area-pointer)
  (file c-call:c-string)
  (database c-call:c-string)
  (new-name c-call:c-string)
  (flags c-call:unsigned-int))


(def-alien-routine ("dbx_txn_begin" %db-txn-begin) c-call:int
  (dbenv sys:system-area-pointer)
  (parent sys:system-area-pointer)
  (txnid sys:system-area-pointer :out)
  (flags c-call:unsigned-int))

(def-alien-routine ("dbx_txn_commit" %db-txn-commit) c-call:int
  (txnid sys:system-area-pointer)
  (flags c-call:unsigned-int))

(def-alien-routine ("dbx_txn_abort" %db-txn-abort) c-call:int
  (txnid sys:system-area-pointer))


(def-alien-routine ("dbx_checkpoint" %db-checkpoint) c-call:int
  (env sys:system-area-pointer)
  (kbyte c-call:unsigned-int)
  (min c-call:unsigned-int)
  (flags c-call:unsigned-int))


(def-alien-routine ("dbx_lock_detect" %db-lock-detect) c-call:int
  (env sys:system-area-pointer)
  (flags c-call:unsigned-int)
  (atype c-call:unsigned-int)
  (aborted c-call:int :out))

(def-alien-routine ("dbx_set_lk_detect" %db-set-lk-detect) c-call:int
  (env sys:system-area-pointer)
  (detect c-call:unsigned-int))


(def-alien-routine ("db_strerror" %db-strerror) c-call:c-string
  (errno c-call:int))

(def-alien-routine ("db_version" db-version) c-call:c-string
  (major c-call:int :out)
  (minor c-call:int :out)
  (patch c-call:int :out))
