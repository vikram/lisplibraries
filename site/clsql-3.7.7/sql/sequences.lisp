;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;;
;;;; $Id: sequences.lisp 9453 2004-05-23 15:44:02Z kevin $
;;;;
;;;; Generic sequence implementation. Backends should use native sequences if
;;;; are available.
;;;;
;;;; This file is part of CLSQL.
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:clsql-sys)

(defclass generic-database (database)
  ()
  (:documentation "Encapsulate same behavior across backends."))


;;; Sequence functions

(defun %sequence-name-to-table (sequence-name database)
  (concatenate 'string
	       (convert-to-db-default-case "_CLSQL_SEQ_" database)
	       (sql-escape sequence-name)))

(defun %table-name-to-sequence-name (table-name database)
  (and (>= (length table-name) 11)
       (string-equal (subseq table-name 0 11)
		     (convert-to-db-default-case "_CLSQL_SEQ_" database))
       (subseq table-name 11)))

(defmethod database-create-sequence (sequence-name database)
  (let ((table-name (%sequence-name-to-table sequence-name database)))
    (database-execute-command
     (concatenate 'string "CREATE TABLE " table-name
		  " (last_value int NOT NULL PRIMARY KEY, increment_by int, min_value int, is_called char(1))")
     database)
    (database-execute-command 
     (concatenate 'string "INSERT INTO " table-name
		  " VALUES (1,1,1,'f')")
     database)))

(defmethod database-drop-sequence (sequence-name database)
  (database-execute-command
   (concatenate 'string "DROP TABLE " (%sequence-name-to-table sequence-name database)) 
   database))

(defmethod database-list-sequences (database &key (owner nil))
  (declare (ignore owner))
  (mapcan #'(lambda (s)
	      (let ((sn (%table-name-to-sequence-name s database)))
		(and sn (list sn))))
	  (database-list-tables-and-sequences database)))

(defmethod database-set-sequence-position (sequence-name position database)
  (database-execute-command
   (format nil "UPDATE ~A SET last_value=~A,is_called='t'" 
	   (%sequence-name-to-table sequence-name database)
           position)
   database)
  position)

(defmethod database-sequence-next (sequence-name database)
  (without-interrupts
   (let* ((table-name (%sequence-name-to-table sequence-name database))
	  (tuple
	   (car (database-query 
		 (concatenate 'string "SELECT last_value,is_called FROM " 
			      table-name)
		 database :auto nil))))
     (cond
       ((char-equal (schar (second tuple) 0) #\f)
	(database-execute-command
	 (format nil "UPDATE ~A SET is_called='t'" table-name)
	 database)
	(car tuple))
       (t
	(let ((new-pos (1+ (car tuple))))
	 (database-execute-command
	  (format nil "UPDATE ~A SET last_value=~D" table-name new-pos)
	  database)
	 new-pos))))))
	     
(defmethod database-sequence-last (sequence-name database)
  (without-interrupts
   (caar (database-query 
	  (concatenate 'string "SELECT last_value FROM " 
		       (%sequence-name-to-table sequence-name database))
	  database :auto nil))))
