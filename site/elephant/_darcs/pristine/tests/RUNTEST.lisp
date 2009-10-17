;;; RUNTEST.lisp
;;;
;;; part of
;;;
;;; Elephant: an object-oriented database for Common Lisp
;;;
;;; Copyright (c) 2005,2006,2007 by Robert L. Read
;;; <rread@common-lisp.net>
;;;
;;; Elephant users are granted the rights to distribute and use this software
;;; as governed by the terms of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.

;; If you are only using one back-end, you may prefer:
;; SQLDB-test.lisp or BerkeleyDB-tests.lisp
(asdf:operate 'asdf:load-op :elephant)
(asdf:operate 'asdf:load-op :ele-clsql)
(asdf:operate 'asdf:load-op :ele-bdb)
(asdf:operate 'asdf:load-op :ele-sqlite3)
;; (asdf:operate 'asdf:load-op :postmodern)

(asdf:operate 'asdf:load-op :elephant-tests)

(in-package "ELEPHANT-TESTS")

;; Test Postgres backend
(setq *default-spec* *testpg-spec*)
(time (do-backend-tests))

;; Test BDB backend
(setq *default-spec* *testbdb-spec*)
(time (do-backend-tests))

;; Test SQLite 3
(setq *default-spec* *testsqlite3-spec*)
(do-backend-tests)

;; Test Postmodern interface to Postgres
;; Note:  One almost certainly has to execute "createdb elepm" as the user "postgres"
;; in order for this to work.
;; The Postemodern interface also apparently requries that "plpgsql language be loaded as 
;; a query language into the postgres instance.  This can be created by executing:
;; "create language plpgsql;" inside postgres itself.
(asdf:operate 'asdf:load-op :ele-postmodern)
(setq *default-spec* *testpm-spec*)
(time (do-backend-tests))


;; Test a Migration of data from BDB to postgres
(do-migration-tests *testbdb-spec* *testpg-spec*)

;; An example usage.
(open-store *testpg-spec*)
(add-to-root "x1" "y1")
(get-from-root "x1")

(add-to-root "x2" '(a 4 "spud"))
(get-from-root "x2")




