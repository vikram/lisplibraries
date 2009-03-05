/*Copyright (c) 2006-2007, Arthur Smyles
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.
   2. Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
         documentation and/or other materials provided with the distribution.

         THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
         AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
         IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
         ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
         LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
         CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
         SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
         INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
         CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
         ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
         POSSIBILITY OF SUCH DAMAGE.
*/
/**
 * Need this file because swig does not handle nested struct defs properly
 */
%insert("swiglisp") %{
(cffi:defcstruct __db_ilock
	(pgno :unsigned-int)
        (fileid :uint8 :count 20)
	(type :unsigned-int))
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
%}
