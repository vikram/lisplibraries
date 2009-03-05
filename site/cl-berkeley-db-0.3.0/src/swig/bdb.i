/* Copyright (c) 2006-2007, Arthur Smyles
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

%module "cffi"

%insert("lisphead") %{
(in-package #:cl-berkeley-db.common-lisp.net)
%}
%insert("swiglisp") %{
;SNIP-START
%}
%import <sys/types.h>
%insert("swiglisp") %{
;from inttypes.h
%}
%import <inttypes.h>
%insert("swiglisp") %{
;from stdint.h
%}
%import <stdint.h>
%insert("swiglisp") %{
;from stddef.h.h
%}
%import <stddef.h>
%import <stdio.h>
%import <unistd.h>
%import <pthread.h>
%insert("swiglisp") %{
;SNIP-END
%}

#define __NO_SYSTEM_INCLUDES
/*Swig doesn't handle these well because they have internal structs and defs */
%ignore __db_txn;
%ignore __db_env;
%ignore __dbc;
%ignore __db;
%ignore __db_ilock;
%ignore DB_VERSION_STRING;

%include db.h
%include db_txn.i
%include db.i
%include dbc.i
%include db_env.i
%insert("swiglisp") %{
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


%}
