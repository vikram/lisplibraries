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
%insert("swiglisp") %{
(cl:defconstant TXN_CHILDCOMMIT         #x0001  " Txn has committed. ")
(cl:defconstant TXN_CDSGROUP            #x0002  " CDS group handle. ")
(cl:defconstant TXN_COMPENSATE          #x0004  " Compensating transaction. ")
(cl:defconstant TXN_DEADLOCK            #x0008  " Txn has deadlocked. ")
(cl:defconstant TXN_LOCKTIMEOUT         #x0010  " Txn has a lock timeout. ")
(cl:defconstant TXN_MALLOC              #x0020  " Structure allocated by TXN system. ")
(cl:defconstant TXN_NOSYNC              #x0040  " Do not sync on prepare and commit. ")
(cl:defconstant TXN_NOWAIT              #x0080  " Do not wait on locks. ")
(cl:defconstant TXN_PRIVATE             #x0100  " Txn owned by cursor.. ")
(cl:defconstant TXN_READ_COMMITTED      #x0200  " Txn has degree 2 isolation. ")
(cl:defconstant TXN_READ_UNCOMMITTED    #x0400  " Txn has degree 1 isolation. ")
(cl:defconstant TXN_RESTORED            #x0800  " Txn has been restored. ")
(cl:defconstant TXN_SNAPSHOT            #x1000  " Snapshot Isolation. ")
(cl:defconstant TXN_SYNC                #x2000  " Write and sync on prepare/commit. ")
(cl:defconstant TXN_WRITE_NOSYNC        #x4000  " Write only on prepare/commit. ")

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

%}
