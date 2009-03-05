;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          clsql-sqlite.asd
;;;; Purpose:       ASDF file for CLSQL SQLite3 backend
;;;; Programmer:    Aurelio Bignoli
;;;; Date Started:  Oct 2004
;;;;
;;;; $Id: clsql-sqlite3.asd 10191 2004-12-07 06:11:55Z kevin $
;;;;
;;;; This file, part of CLSQL, is Copyright (c) 2004 by Aurelio Bignoli
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(defpackage #:clsql-sqlite3-system (:use #:asdf #:cl))
(in-package #:clsql-sqlite3-system)

(defsystem clsql-sqlite3
  :name "cl-sql-sqlite3"
  :author "Aurelio Bignoli <aurelio@bignoli.it>"
  :maintainer "Aurelio Bignoli"
  :licence "Lessor Lisp General Public License"
  :description "Common Lisp Sqlite3 Driver"
  :long-description "cl-sql-sqlite3 package provides a database driver to SQLite Versione 3 database library."


  :depends-on (clsql clsql-uffi)
  :components
  ((:module :db-sqlite3
	    :components
	    ((:file "sqlite3-package")
	     (:file "sqlite3-loader" :depends-on ("sqlite3-package"))
	     (:file  "sqlite3-api" :depends-on ("sqlite3-loader"))
	     (:file "sqlite3-sql" :depends-on ("sqlite3-api"))))))
