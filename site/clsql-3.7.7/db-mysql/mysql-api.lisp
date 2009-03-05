;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          mysql-api.lisp
;;;; Purpose:       Low-level MySQL interface using UFFI
;;;; Programmers:   Kevin M. Rosenberg based on
;;;;                Original code by Pierre R. Mai
;;;; Date Started:  Feb 2002
;;;;
;;;; $Id: mysql-api.lisp 11062 2006-08-30 10:47:38Z kevin $
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;; and Copyright (c) 1999-2001 by Pierre R. Mai
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:mysql)

;;;; Modifications from original code
;;;;  - Updated C-structures to conform to structures in MySQL 3.23.46
;;;;  - Changed from CMUCL interface to UFFI
;;;;  - Added and call a C-helper file to support 64-bit integers
;;;;    that are used in a few routines.
;;;;  - Removed all references to interiors of C-structions, this will
;;;;    increase robustness when MySQL's internal structures change.

;;;; Type definitions

;;; Basic Types

(uffi:def-foreign-type mysql-socket :int)
(uffi:def-foreign-type mysql-bool :byte)
(uffi:def-foreign-type mysql-byte :unsigned-char)

(uffi:def-enum mysql-net-type
    (:tcp-ip
     :socket
     :named-pipe))

(uffi:def-struct mysql-net
    (vio :pointer-void)
  (fd mysql-socket)
  (fcntl :int)
  (buff (* :unsigned-char))
  (buff-end (* :unsigned-char))
  (write-pos (* :unsigned-char))
  (read-pos (* :unsigned-char))
  (last-error (:array :char 200))
  (last-errno :unsigned-int)
  (max-packet :unsigned-int)
  (timeout :unsigned-int)
  (pkt-nr :unsigned-int)
  (error mysql-bool)
  (return-errno mysql-bool)
  (compress mysql-bool)
  (no-send-ok mysql-bool)
  (remain-in-buf :unsigned-long)
  (length :unsigned-long)
  (buf-length :unsigned-long)
  (where-b :unsigned-long)
  (return-status (* :unsigned-int))
  (reading-or-writing :unsigned-char)
  (save-char :char))

;;; Mem-Root
(uffi:def-struct mysql-used-mem
    (next :pointer-self)
  (left :unsigned-int)
  (size :unsigned-int))

(uffi:def-struct mysql-mem-root
    (free (:struct-pointer mysql-used-mem))
  (used (:struct-pointer mysql-used-mem))
  (pre-alloc (:struct-pointer mysql-used-mem))
  (min-alloc :unsigned-int)
  (block-size :unsigned-int)
  (error-handler :pointer-void))

;;; MYSQL-FIELD
(uffi:def-enum mysql-field-types
    (:decimal
     :tiny
     :short
     :long
     :float
     :double
     :null
     :timestamp
     :longlong
     :int24
     :date
     :time
     :datetime
     :year
     :newdate
     (:enum 247)
     (:set 248)
     (:tiny-blob 249)
     (:medium-blob 250)
     (:long-blob 251)
     (:blob 252)
     (:var-string 253)
     (:string 254)
     (:geometry 255)))

#+mysql-client-v3
(uffi:def-struct mysql-field
    (name (* :char))
  (table (* :char))
  (def (* :char))
  (type mysql-field-types)
  (length :unsigned-int)
  (max-length :unsigned-int)
  (flags :unsigned-int)
  (decimals :unsigned-int))

;; structure changed in mysql 4 client
#+(and mysql-client-v4 (not mysql-client-v4.1))
(uffi:def-struct mysql-field
    (name (* :char))
  (table (* :char))
  (org_table (* :char))
  (db (* :char))
  (def (* :char))
  (length :unsigned-long)
  (max-length :unsigned-long)
  (flags :unsigned-int)
  (decimals :unsigned-int)
  (type mysql-field-types))

#+(or mysql-client-v4.1 mysql-client-v5)
(uffi:def-struct mysql-field
    (name (* :char))
  (org_name (* :char))
  (table (* :char))
  (org_table (* :char))
  (db (* :char))
  (catalog_db (* :char))
  (def (* :char))
  (length :unsigned-long)
  (max-length :unsigned-long)
  (name-length :unsigned-int)
  (org-name-length :unsigned-int)
  (table-length :unsigned-int)
  (org-table-length :unsigned-int)
  (db-length :unsigned-int)
  (catalog-length :unsigned-int)
  (def-length :unsigned-int)
  (flags :unsigned-int)
  (decimals :unsigned-int)
  (charsetnr :unsigned-int)
  (type mysql-field-types))


(uffi:def-struct mysql-time
    (year :unsigned-int)
  (month :unsigned-int)
  (day :unsigned-int)
  (hour :unsigned-int)
  (minute :unsigned-int)
  (second :unsigned-int)
  (second-part :unsigned-long)
  (neg mysql-bool)
  (time-type :int))

;;; MYSQL-ROWS

(uffi:def-array-pointer mysql-row (* :unsigned-char))

(uffi:def-array-pointer mysql-field-vector (* mysql-field))

(uffi:def-foreign-type mysql-field-offset :unsigned-int)

(uffi:def-struct mysql-rows
    (next :pointer-self)
  (data mysql-row))

(uffi:def-foreign-type mysql-row-offset (:struct-pointer mysql-rows))

(uffi:def-struct mysql-data
    (rows-high32 :unsigned-long)
  (rows-low32 :unsigned-long)
  (fields :unsigned-int)
  (data (:struct-pointer mysql-rows))
  (alloc (:struct mysql-mem-root)))

;;; MYSQL
(uffi:def-struct mysql-options
    (connect-timeout :unsigned-int)
  (client-flag :unsigned-int)
  (compress mysql-bool)
  (named-pipe mysql-bool)
  (port :unsigned-int)
  (host (* :char))
  (init-command (* :char))
  (user (* :char))
  (password (* :char))
  (unix-socket (* :char))
  (db (* :char))
  (my-cnf-file (* :char))
  (my-cnf-group (* :char))
  (charset-dir (* :char))
  (charset-name (* :char))
  (use-ssl mysql-bool)
  (ssl-key (* :char))
  (ssl-cert (* :char))
  (ssl-ca (* :char))
  (ssl-capath (* :char)))

(uffi:def-enum mysql-option
    (:connect-timeout
     :compress
     :named-pipe
     :init-command
     :read-default-file
     :read-default-group))

(uffi:def-enum mysql-status
    (:ready
     :get-result
     :use-result))

(uffi:def-struct mysql-mysql
    (net (:struct mysql-net))
  (connected-fd (* :char))
  (host (* :char))
  (user (* :char))
  (passwd (* :char))
  (unix-socket (* :char))
  (server-version (* :char))
  (host-info (* :char))
  (info (* :char))
  (db (* :char))
  (port :unsigned-int)
  (client-flag :unsigned-int)
  (server-capabilities :unsigned-int)
  (protocol-version :unsigned-int)
  (field-count :unsigned-int)
  (server-status :unsigned-int)
  (thread-id :unsigned-long)
  (affected-rows-high32 :unsigned-long)
  (affected-rows-low32 :unsigned-long)
  (insert-id-high32 :unsigned-long)
  (insert-id-low32 :unsigned-long)
  (extra-info-high32 :unsigned-long)
  (extra-info-low32 :unsigned-long)
  (packet-length :unsigned-long)
  (status mysql-status)
  (fields (:struct-pointer mysql-field))
  (field-alloc (:struct mysql-mem-root))
  (free-me mysql-bool)
  (reconnect mysql-bool)
  (options (:struct mysql-options))
  (scramble-buff (:array :char 9))
  (charset :pointer-void)
  (server-language :unsigned-int))


;;; MYSQL-RES
(uffi:def-struct mysql-mysql-res
    (row-count-high32 :unsigned-long)
  (row-count-low32 :unsigned-long)
  (field-count :unsigned-int)
  (current-field :unsigned-int)
  (fields (:struct-pointer mysql-field))
  (data (:struct-pointer mysql-data))
  (data-cursor (:struct-pointer mysql-rows))
  (field-alloc (:struct mysql-mem-root))
  (row mysql-row)
  (current-row mysql-row)
  (lengths (* :unsigned-long))
  (handle (:struct-pointer mysql-mysql))
  (eof mysql-bool))

#+(or mysql-client-v4.1 mysql-client-v5)
(uffi:def-enum mysql-field-types
    (:ready
     :get-result
     :use-result))

#+(or mysql-client-v4.1 mysql-client-v5)
(uffi:def-struct mysql-bind
    (length (* :unsigned-long))
  (is-null (* mysql-bool))
  (buffer :pointer-void)
  (buffer-type :int)
  (buffer-length :unsigned-long)
  ;; internal use
  (inter_buffer (* :unsigned-char))
  (offset :unsigned-long)
  (internal-length :unsigned-long)
  (param-number :unsigned-int)
  (pack-length :unsigned-int)
  (is-signed mysql-bool)
  (long-data-used mysql-bool)
  (internal-is-null mysql-bool)
  (store-param-func :pointer-void)
  (fetch-result :pointer-void)
  (skip-result :pointer-void))

;;;; The Foreign C routines
(declaim (inline mysql-init))
(uffi:def-function "mysql_init"
  ((mysql (* mysql-mysql)))
  :module "mysql"
  :returning (* mysql-mysql))

;; Need to comment this out for LW 4.2.6
;; ? bug in LW version
#-lispworks (declaim (inline mysql-real-connect))
(uffi:def-function "mysql_real_connect"
    ((mysql (* mysql-mysql))
     (host :cstring)
     (user :cstring)
     (passwd :cstring)
     (db :cstring)
     (port :unsigned-int)
     (unix-socket :cstring)
     (clientflag :unsigned-long))
  :module "mysql"
  :returning (* mysql-mysql))

(declaim (inline mysql-close))
(uffi:def-function "mysql_close"
    ((sock (* mysql-mysql)))
  :module "mysql"
  :returning :void)

(declaim (inline mysql-select-db))
(uffi:def-function "mysql_select_db"
  ((mysql (* mysql-mysql))
   (db :cstring))
  :module "mysql"
  :returning :int)

(declaim (inline mysql-query))
(uffi:def-function "mysql_query"
    ((mysql (* mysql-mysql))
     (query :cstring))
  :module "mysql"
  :returning :int)

 ;;; I doubt that this function is really useful for direct Lisp usage,
;;; but it is here for completeness...

(declaim (inline mysql-real-query))
(uffi:def-function "mysql_real_query"
    ((mysql (* mysql-mysql))
     (query :cstring)
     (length :unsigned-int))
  :module "mysql"
  :returning :int)

(declaim (inline mysql-shutdown))
(uffi:def-function "mysql_shutdown"
  ((mysql (* mysql-mysql)))
  :module "mysql"
  :returning :int)

(declaim (inline mysql-dump-debug-info))
(uffi:def-function "mysql_dump_debug_info"
  ((mysql (* mysql-mysql)))
  :module "mysql"
  :returning :int)

(declaim (inline mysql-refresh))
(uffi:def-function "mysql_refresh"
  ((mysql (* mysql-mysql))
   (refresh-options :unsigned-int))
  :module "mysql"
  :returning :int)

(declaim (inline mysql-kill))
(uffi:def-function "mysql_kill"
    ((mysql (* mysql-mysql))
     (pid :unsigned-long))
  :module "mysql"
  :returning :int)

(declaim (inline mysql-ping))
(uffi:def-function "mysql_ping"
    ((mysql (* mysql-mysql)))
  :module "mysql"
  :returning :int)

(declaim (inline mysql-stat))
(uffi:def-function "mysql_stat"
  ((mysql (* mysql-mysql)))
  :module "mysql"
  :returning :cstring)

(declaim (inline mysql-get-server-info))
(uffi:def-function "mysql_get_server_info"
    ((mysql (* mysql-mysql)))
  :module "mysql"
  :returning :cstring)

(declaim (inline mysql-get-host-info))
(uffi:def-function "mysql_get_host_info"
    ((mysql (* mysql-mysql)))
  :module "mysql"
  :returning :cstring)

(declaim (inline mysql-get-proto-info))
(uffi:def-function "mysql_get_proto_info"
  ((mysql (* mysql-mysql)))
  :module "mysql"
  :returning :unsigned-int)

(declaim (inline mysql-list-dbs))
(uffi:def-function "mysql_list_dbs"
  ((mysql (* mysql-mysql))
   (wild :cstring))
  :module "mysql"
  :returning (* mysql-mysql-res))

(declaim (inline mysql-list-tables))
(uffi:def-function "mysql_list_tables"
  ((mysql (* mysql-mysql))
   (wild :cstring))
  :module "mysql"
  :returning (* mysql-mysql-res))

(declaim (inline mysql-list-fields))
(uffi:def-function "mysql_list_fields"
  ((mysql (* mysql-mysql))
   (table :cstring)
   (wild :cstring))
  :module "mysql"
  :returning (* mysql-mysql-res))

(declaim (inline mysql-list-processes))
(uffi:def-function "mysql_list_processes"
  ((mysql (* mysql-mysql)))
  :module "mysql"
  :returning (* mysql-mysql-res))

(declaim (inline mysql-store-result))
(uffi:def-function "mysql_store_result"
  ((mysql (* mysql-mysql)))
  :module "mysql"
  :returning (* mysql-mysql-res))

(declaim (inline mysql-use-result))
(uffi:def-function "mysql_use_result"
  ((mysql (* mysql-mysql)))
  :module "mysql"
  :returning (* mysql-mysql-res))

(declaim (inline mysql-options))
(uffi:def-function "mysql_options"
  ((mysql (* mysql-mysql))
   (option mysql-option)
   (arg :cstring))
  :module "mysql"
  :returning :int)

(declaim (inline mysql-free-result))
(uffi:def-function "mysql_free_result"
    ((res (* mysql-mysql-res)))
  :module "mysql"
  :returning :void)

(declaim (inline mysql-row-seek))
(uffi:def-function "mysql_row_seek"
  ((res (* mysql-mysql-res))
   (offset mysql-row-offset))
  :module "mysql"
  :returning mysql-row-offset)

(declaim (inline mysql-field-seek))
(uffi:def-function "mysql_field_seek"
  ((res (* mysql-mysql-res))
  (offset mysql-field-offset))
  :module "mysql"
  :returning mysql-field-offset)

(declaim (inline mysql-fetch-row))
(uffi:def-function "mysql_fetch_row"
    ((res (* mysql-mysql-res)))
  :module "mysql"
  :returning (* (* :unsigned-char)))

(declaim (inline mysql-fetch-lengths))
(uffi:def-function "mysql_fetch_lengths"
  ((res (* mysql-mysql-res)))
  :module "mysql"
  :returning (* :unsigned-long))

(declaim (inline mysql-fetch-field))
(uffi:def-function "mysql_fetch_field"
  ((res (* mysql-mysql-res)))
  :module "mysql"
  :returning (* mysql-field))

(declaim (inline mysql-fetch-fields))
(uffi:def-function "mysql_fetch_fields"
  ((res (* mysql-mysql-res)))
  :module "mysql"
  :returning (* mysql-field))

(declaim (inline mysql-fetch-field-direct))
(uffi:def-function "mysql_fetch_field_direct"
  ((res (* mysql-mysql-res))
   (field-num :unsigned-int))
  :module "mysql"
  :returning (* mysql-field))

(declaim (inline mysql-escape-string))
(uffi:def-function "mysql_escape_string"
    ((to (* :unsigned-char))
     (from (* :unsigned-char))
     (length :unsigned-int))
  :module "mysql"
  :returning :unsigned-int)

(declaim (inline mysql-debug))
(uffi:def-function "mysql_debug"
    ((debug :cstring))
  :module "mysql"
  :returning :void)

(declaim (inline clsql-mysql-num-rows))
(uffi:def-function "clsql_mysql_num_rows"
    ((res (* mysql-mysql-res))
     (p-high32 (* :unsigned-int)))
  :module "clsql-mysql"
  :returning :unsigned-int)

#+(or mysql-client-v4.1 mysql-client-v5)
(uffi:def-foreign-type mysql-stmt-ptr :pointer-void)

#+(or mysql-client-v4.1 mysql-client-v5)
(uffi:def-function "mysql_stmt_init"
    ((res (* mysql-mysql-res)))
  :module "clsql-mysql"
  :returning mysql-stmt-ptr)

#+(or mysql-client-v4.1 mysql-client-v5)
(uffi:def-function "mysql_stmt_prepare"
    ((stmt mysql-stmt-ptr)
     (query :cstring)
     (length :unsigned-long))
  :module "clsql-mysql"
  :returning :int)

#+(or mysql-client-v4.1 mysql-client-v5)
(uffi:def-function "mysql_stmt_param_count"
    ((stmt mysql-stmt-ptr))
  :module "clsql-mysql"
  :returning :unsigned-int)

#+(or mysql-client-v4.1 mysql-client-v5)
(uffi:def-function "mysql_stmt_bind_param"
    ((stmt mysql-stmt-ptr)
     (bind (* mysql-bind)))
  :module "clsql-mysql"
  :returning :short)

#+(or mysql-client-v4.1 mysql-client-v5)
(uffi:def-function "mysql_stmt_bind_result"
    ((stmt mysql-stmt-ptr)
     (bind (* mysql-bind)))
  :module "clsql-mysql"
  :returning :short)

#+(or mysql-client-v4.1 mysql-client-v5)
(uffi:def-function "mysql_stmt_result_metadata"
    ((stmt mysql-stmt-ptr))
  :module "clsql-mysql"
  :returning (* mysql-mysql-res))


#+(or mysql-client-v4.1 mysql-client-v5)
(uffi:def-function "mysql_stmt_execute"
    ((stmt mysql-stmt-ptr))
  :module "clsql-mysql"
  :returning :int)

#+(or mysql-client-v4.1 mysql-client-v5)
(uffi:def-function "mysql_stmt_store_result"
    ((stmt mysql-stmt-ptr))
  :module "clsql-mysql"
  :returning :int)

#+(or mysql-client-v4.1 mysql-client-v5)
(uffi:def-function "mysql_stmt_fetch"
    ((stmt mysql-stmt-ptr))
  :module "clsql-mysql"
  :returning :int)

#+(or mysql-client-v4.1 mysql-client-v5)
(uffi:def-function "mysql_stmt_free_result"
    ((stmt mysql-stmt-ptr))
  :module "clsql-mysql"
  :returning :short)

#+(or mysql-client-v4.1 mysql-client-v5)
(uffi:def-function "mysql_stmt_close"
    ((stmt mysql-stmt-ptr))
  :module "clsql-mysql"
  :returning :short)

#+(or mysql-client-v4.1 mysql-client-v5)
(uffi:def-function "mysql_stmt_errno"
    ((stmt mysql-stmt-ptr))
  :module "clsql-mysql"
  :returning :unsigned-int)

#+(or mysql-client-v4.1 mysql-client-v5)
(uffi:def-function "mysql_stmt_error"
    ((stmt mysql-stmt-ptr))
  :module "clsql-mysql"
  :returning :cstring)


;;;; Equivalents of C Macro definitions for accessing various fields
;;;; in the internal MySQL Datastructures


(declaim (inline mysql-num-rows))
(defun mysql-num-rows (res)
  (uffi:with-foreign-object (p-high32 :unsigned-int)
    (let ((low32 (clsql-mysql-num-rows res p-high32))
	  (high32 (uffi:deref-pointer p-high32 :unsigned-int)))
      (if (zerop high32)
	  low32
	(make-64-bit-integer high32 low32)))))

(uffi:def-function "clsql_mysql_affected_rows"
    ((mysql (* mysql-mysql))
     (p-high32 (* :unsigned-int)))
  :returning :unsigned-int
  :module "clsql-mysql")

(defun mysql-affected-rows (mysql)
  (uffi:with-foreign-object (p-high32 :unsigned-int)
    (let ((low32 (clsql-mysql-affected-rows mysql p-high32))
	  (high32 (uffi:deref-pointer p-high32 :unsigned-int)))
      (if (zerop high32)
	  low32
	(make-64-bit-integer high32 low32)))))

(uffi:def-function "clsql_mysql_insert_id"
    ((res (* mysql-mysql))
     (p-high32 (* :unsigned-int)))
  :returning :unsigned-int
  :module "clsql-mysql")

(defun mysql-insert-id (mysql)
  (uffi:with-foreign-object (p-high32 :unsigned-int)
  (let ((low32 (clsql-mysql-insert-id mysql p-high32))
	(high32 (uffi:deref-pointer p-high32 :unsigned-int)))
    (if (zerop high32)
	low32
      (make-64-bit-integer high32 low32)))))


(declaim (inline mysql-num-fields))
(uffi:def-function "mysql_num_fields"
  ((res (* mysql-mysql-res)))
  :returning :unsigned-int
  :module "mysql")

(declaim (inline clsql-mysql-eof))
(uffi:def-function ("mysql_eof" clsql-mysql-eof)
  ((res (* mysql-mysql-res)))
  :returning :char
  :module "mysql")

(declaim (inline mysql-eof))
(defun mysql-eof (res)
  (if (zerop (clsql-mysql-eof res))
      nil
    t))

(declaim (inline mysql-error))
(uffi:def-function ("mysql_error" mysql-error)
  ((mysql (* mysql-mysql)))
  :returning :cstring
  :module "mysql")

(declaim (inline mysql-error-string))
(defun mysql-error-string (mysql)
  (uffi:convert-from-cstring (mysql-error mysql)))

(declaim (inline mysql-errno))
(uffi:def-function "mysql_errno"
  ((mysql (* mysql-mysql)))
  :returning :unsigned-int
  :module "mysql")

(declaim (inline mysql-info))
(uffi:def-function ("mysql_info" mysql-info)
  ((mysql (* mysql-mysql)))
  :returning :cstring
  :module "mysql")

(declaim (inline mysql-info-string))
(defun mysql-info-string (mysql)
  (uffi:convert-from-cstring (mysql-info mysql)))

(declaim (inline clsql-mysql-data-seek))
(uffi:def-function "clsql_mysql_data_seek"
  ((res (* mysql-mysql-res))
   (offset-high32 :unsigned-int)
   (offset-low32 :unsigned-int))
  :module "clsql-mysql"
  :returning :void)

(defun mysql-data-seek (res offset)
  (multiple-value-bind (high32 low32) (split-64-bit-integer offset)
    (clsql-mysql-data-seek res high32 low32)))
