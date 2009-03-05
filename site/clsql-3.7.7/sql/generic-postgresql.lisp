;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;;
;;;; $Id: generic-postgresql.lisp 11026 2006-08-14 06:54:32Z kevin $
;;;;
;;;; Generic postgresql layer, used by db-postgresql and db-postgresql-socket
;;;;
;;;; This file is part of CLSQL.
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:clsql-sys)

(defclass generic-postgresql-database (database)
  ((has-table-pg_roles :type boolean :reader has-table-pg_roles :initform nil))
  (:documentation "Encapsulate same behavior across postgresql and postgresql-socket backends."))



;; Object functions

(defmethod database-get-type-specifier (type args database
					(db-type (eql :postgresql)))
  (declare (ignore type args database))
  "VARCHAR")

(defmethod database-get-type-specifier ((type (eql 'string)) args database
					(db-type (eql :postgresql)))
  (declare (ignore database))
  (if args
      (format nil "CHAR(~A)" (car args))
    "VARCHAR"))

(defmethod database-get-type-specifier ((type (eql 'tinyint)) args database
					(db-type (eql :postgresql)))
  (declare (ignore args database))
  "INT2")

(defmethod database-get-type-specifier ((type (eql 'smallint)) args database
					(db-type (eql :postgresql)))
  (declare (ignore args database))
  "INT2")

(defmethod database-get-type-specifier ((type (eql 'wall-time)) args database
					(db-type (eql :postgresql)))
  (declare (ignore args database))
  "TIMESTAMP WITHOUT TIME ZONE")

(defmethod database-get-type-specifier ((type (eql 'number)) args database
                                        (db-type (eql :postgresql)))
  (declare (ignore database))
  (cond
   ((and (consp args) (= (length args) 2))
    (format nil "NUMERIC(~D,~D)" (first args) (second args)))
   ((and (consp args) (= (length args) 1))
    (format nil "NUMERIC(~D)" (first args)))
   (t
    "NUMERIC")))

;;; Backend functions

(defun owner-clause (owner)
  (cond
   ((stringp owner)
    (format
     nil
     " AND (relowner=(SELECT usesysid FROM pg_user WHERE (usename='~A')))"
     owner))
   ((null owner)
    (format nil " AND (relowner<>(SELECT usesysid FROM pg_user WHERE usename='postgres'))"))
   (t "")))

(defun has-table (name database)
  (let ((name-retrieved
         (caar (database-query
                (format nil "SELECT relname FROM pg_class WHERE relname='~A'"
                        name)
                database nil nil))))
    (if (and (stringp name-retrieved) (plusp (length name-retrieved)))
        t
        nil)))

(defmethod slot-unbound (class (obj generic-postgresql-database)
                         (slot (eql 'has-table-pg_roles)))
  ;; Lazily cache slot value
  (declare (ignore class))
  (setf (slot-value obj 'has-table-pg_roles) (has-table "pg_roles" obj)))

(defun database-list-objects-of-type (database type owner)
  (mapcar #'car
	  (database-query
	   (format nil
                   (if (and (has-table-pg_roles database)
                            (not (eq owner :all)))
                       "
 SELECT c.relname
 FROM pg_catalog.pg_class c
      LEFT JOIN pg_catalog.pg_roles r ON r.oid = c.relowner
      LEFT JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace
 WHERE c.relkind IN ('~A','')
       AND n.nspname NOT IN ('pg_catalog', 'pg_toast')
       AND pg_catalog.pg_table_is_visible(c.oid)
       ~A"
                       "SELECT relname FROM pg_class WHERE (relkind =
'~A')~A")
		   type
		   (owner-clause owner))
	   database nil nil)))

(defmethod database-list-tables ((database generic-postgresql-database)
                                 &key (owner nil))
  (database-list-objects-of-type database "r" owner))

(defmethod database-list-views ((database generic-postgresql-database)
                                &key (owner nil))
  (database-list-objects-of-type database "v" owner))

(defmethod database-list-indexes ((database generic-postgresql-database)
                                  &key (owner nil))
  (database-list-objects-of-type database "i" owner))


(defmethod database-list-table-indexes (table (database generic-postgresql-database)
					&key (owner nil))
  (let ((indexrelids
	 (database-query
	  (format
	   nil
	   "select indexrelid from pg_index where indrelid=(select relfilenode from pg_class where relname='~A'~A)"
	   (string-downcase table)
	   (owner-clause owner))
	  database :auto nil))
	(result nil))
    (dolist (indexrelid indexrelids (nreverse result))
      (push
       (caar (database-query
	      (format nil "select relname from pg_class where relfilenode='~A'"
		      (car indexrelid))
	      database nil nil))
       result))))

(defmethod database-list-attributes ((table string)
				     (database generic-postgresql-database)
                                     &key (owner nil))
  (let* ((owner-clause
          (cond ((stringp owner)
                 (format nil " AND (relowner=(SELECT usesysid FROM pg_user WHERE usename='~A'))" owner))
                ((null owner) " AND (not (relowner=1))")
                (t "")))
         (result
	  (mapcar #'car
		  (database-query
		   (format nil "SELECT attname FROM pg_class,pg_attribute WHERE pg_class.oid=attrelid AND attisdropped = FALSE AND relname='~A'~A"
                           (string-downcase table)
                           owner-clause)
                   database nil nil))))
    (if result
        (remove-if #'(lambda (it) (member it '("cmin"
                                               "cmax"
                                               "xmax"
                                               "xmin"
                                               "oid"
                                               "ctid"
                                               ;; kmr -- added tableoid
                                               "tableoid") :test #'equal))
                   result))))

(defmethod database-attribute-type (attribute (table string)
				    (database generic-postgresql-database)
                                    &key (owner nil))
  (let ((row (car (database-query
		   (format nil "SELECT pg_type.typname,pg_attribute.attlen,pg_attribute.atttypmod,pg_attribute.attnotnull FROM pg_type,pg_class,pg_attribute WHERE pg_class.oid=pg_attribute.attrelid AND pg_class.relname='~A' AND pg_attribute.attname='~A' AND pg_attribute.atttypid=pg_type.oid~A"
			   (string-downcase table)
			   (string-downcase attribute)
			   (owner-clause owner))
		   database nil nil))))
    (when row
      (destructuring-bind (typname attlen atttypmod attnull) row

        (setf attlen (parse-integer attlen :junk-allowed t)
              atttypmod (parse-integer atttypmod :junk-allowed t))

        (let ((coltype (ensure-keyword typname))
              (colnull (if (string-equal "f" attnull) 1 0))
              collen
              colprec)
           (setf (values collen colprec)
                 (case coltype
                   ((:numeric :decimal)
                    (if (= -1 atttypmod)
                        (values nil nil)
                        (values (ash (- atttypmod 4) -16)
                                (boole boole-and (- atttypmod 4) #xffff))))
                   (otherwise
                    (values
                     (cond ((and (= -1 attlen) (= -1 atttypmod)) nil)
                           ((= -1 attlen) (- atttypmod 4))
                           (t attlen))
                     nil))))
           (values coltype collen colprec colnull))))))

(defmethod database-create-sequence (sequence-name
				     (database generic-postgresql-database))
  (database-execute-command
   (concatenate 'string "CREATE SEQUENCE " (sql-escape sequence-name))
   database))

(defmethod database-drop-sequence (sequence-name
				   (database generic-postgresql-database))
  (database-execute-command
   (concatenate 'string "DROP SEQUENCE " (sql-escape sequence-name)) database))

(defmethod database-list-sequences ((database generic-postgresql-database)
                                    &key (owner nil))
  (database-list-objects-of-type database "S" owner))

(defmethod database-set-sequence-position (name (position integer)
                                                (database generic-postgresql-database))
  (values
   (parse-integer
    (caar
     (database-query
      (format nil "SELECT SETVAL ('~A', ~A)" name position)
      database nil nil)))))

(defmethod database-sequence-next (sequence-name
				   (database generic-postgresql-database))
  (values
   (parse-integer
    (caar
     (database-query
      (concatenate 'string "SELECT NEXTVAL ('" (sql-escape sequence-name) "')")
      database nil nil)))))

(defmethod database-sequence-last (sequence-name (database generic-postgresql-database))
  (values
   (parse-integer
    (caar
     (database-query
      (concatenate 'string "SELECT LAST_VALUE FROM " sequence-name)
      database nil nil)))))

(defun postgresql-database-list (connection-spec type)
  (destructuring-bind (host name user password) connection-spec
    (declare (ignore name))
    (let ((database (database-connect (list host "template1" user password)
				      type)))
      (unwind-protect
	   (progn
	     (setf (slot-value database 'clsql-sys::state) :open)
	     (mapcar #'car (database-query "select datname from pg_database"
					   database nil nil)))
	(progn
	  (database-disconnect database)
	  (setf (slot-value database 'clsql-sys::state) :closed))))))

(defmethod database-list (connection-spec (type (eql :postgresql)))
  (postgresql-database-list connection-spec type))

(defmethod database-list (connection-spec (type (eql :postgresql-socket)))
  (postgresql-database-list connection-spec type))

#+nil
(defmethod database-describe-table ((database generic-postgresql-database) table)
  ;; MTP: LIST-ATTRIBUTE-TYPES currently executes separate queries for
  ;; each attribute. It would be more efficient to have a single SQL
  ;; query return the type data for all attributes. This code is
  ;; retained as an example of how to do this for PostgreSQL.
  (database-query
   (format nil "select a.attname, t.typname
                               from pg_class c, pg_attribute a, pg_type t
                               where c.relname = '~a'
                                   and a.attnum > 0
                                   and a.attrelid = c.oid
                                   and a.atttypid = t.oid"
           (sql-escape (string-downcase table)))
   database :auto nil))

;;; Prepared statements

(defvar *next-prepared-id-num* 0)
(defun next-prepared-id ()
  (let ((num (incf *next-prepared-id-num*)))
    (format nil "CLSQL_PS_~D" num)))

(defclass postgresql-stmt ()
  ((database :initarg :database :reader database)
   (id :initarg :id :reader id)
   (bindings :initarg :bindings :reader bindings)
   (field-names :initarg :field-names :accessor stmt-field-names)
   (result-types :initarg :result-types :reader result-types)))

(defun clsql-type->postgresql-type (type)
  (cond
    ((in type :int :integer) "INT4")
    ((in type :short) "INT2")
    ((in type :bigint) "INT8")
    ((in type :float :double :number) "NUMERIC")
    ((and (consp type) (in (car type) :char :varchar)) "VARCHAR")
    (t
     (error 'sql-user-error
	    :message
	    (format nil "Unknown clsql type ~A." type)))))

(defun prepared-sql-to-postgresql-sql (sql)
  ;; FIXME: Convert #\? to "$n". Don't convert within strings
  (declare (simple-string sql))
  (with-output-to-string (out)
    (do ((len (length sql))
	 (param 0)
	 (in-str nil)
	 (pos 0 (1+ pos)))
	((= len pos))
      (declare (fixnum len param pos))
      (let ((c (schar sql pos)))
	(declare (character c))
	(cond
	 ((or (char= c #\") (char= c #\'))
	  (setq in-str (not in-str))
	  (write-char c out))
	 ((and (char= c #\?) (not in-str))
	  (write-char #\$ out)
	  (write-string (write-to-string (incf param)) out))
	 (t
	  (write-char c out)))))))

(defmethod database-prepare (sql-stmt types (database generic-postgresql-database) result-types field-names)
  (let ((id (next-prepared-id)))
    (database-execute-command
     (format nil "PREPARE ~A (~{~A~^,~}) AS ~A"
	     id
	     (mapcar #'clsql-type->postgresql-type types)
	     (prepared-sql-to-postgresql-sql sql-stmt))
     database)
    (make-instance 'postgresql-stmt
		   :id id
		   :database database
		   :result-types result-types
		   :field-names field-names
		   :bindings (make-list (length types)))))

(defmethod database-bind-parameter ((stmt postgresql-stmt) position value)
  (setf (nth (1- position) (bindings stmt)) value))

(defun binding-to-param (binding)
  (typecase binding
    (string
     (concatenate 'string "'" (sql-escape-quotes binding) "'"))
    (t
     binding)))

(defmethod database-run-prepared ((stmt postgresql-stmt))
  (with-slots (database id bindings field-names result-types) stmt
    (let ((query (format nil "EXECUTE ~A (~{~A~^,~})"
			 id (mapcar #'binding-to-param bindings))))
      (cond
       ((and field-names (not (consp field-names)))
	(multiple-value-bind (res names)
	    (database-query query database result-types field-names)
	  (setf field-names names)
	  (values res names)))
       (field-names
	(values (nth-value 0 (database-query query database result-types nil))
		field-names))
       (t
	(database-query query database result-types field-names))))))

;;; Capabilities

(defmethod db-type-has-fancy-math? ((db-type (eql :postgresql)))
  t)

(defmethod db-type-default-case ((db-type (eql :postgresql)))
  :lower)

(defmethod db-type-has-prepared-stmt? ((db-type (eql :postgresql)))
  t)

(defmethod db-type-has-prepared-stmt? ((db-type (eql :postgresql-socket)))
  t)

