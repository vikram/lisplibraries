;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;
;;; tests.lisp -- package definition
;;; 
;;; Initial version 9/02/2004 by Ben Lee
;;; <blee@common-lisp.net>
;;; 
;;; part of
;;;
;;; Elephant: an object-oriented database for Common Lisp
;;;
;;; Copyright (c) 2004 by Andrew Blumberg and Ben Lee
;;; <ablumberg@common-lisp.net> <blee@common-lisp.net>
;;;
;;; Elephant users are granted the rights to distribute and use this software
;;; as governed by the terms of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.

(in-package :ele-tests)

;; Putting this in to make the test work; I have no idea what it means...
(deftype array-or-pointer-char () '(or array t))

(defvar *bdb-spec* 
  `(:bdb . ,(namestring
	     (merge-pathnames 
	      #p"tests/testbdb/" 
	      (asdf:component-pathname (asdf:find-system 'elephant-tests))))))

(defvar *testbdb-spec* 
  `(:bdb
    ,(namestring
      (merge-pathnames 
       #p"tests/testdb/" 
       (asdf:component-pathname (asdf:find-system 'elephant-tests)))))
  "The primary test spec for testing berkeley db backends")

(defvar *testbdb-spec2* 
  `(:bdb
    ,(namestring
      (merge-pathnames 
       #p"tests/testdb2/" 
       (asdf:component-pathname (asdf:find-system 'elephant-tests)))))
  "A second bdb test directory for bdb-to-bdb tests")
  
(defvar *testbdb-spec-oid*
  `(:bdb
    ,(namestring
      (merge-pathnames
       #p"tests/testdb-oid/"
       (asdf:component-pathname (asdf:find-system 'elephant-tests))))))

(defvar *testpg-spec*
  '(:clsql (:postgresql "localhost.localdomain" "test" "postgres" "")))

(defvar *testpg-spec2*
  '(:clsql (:postgresql "localhost.localdomain" "test2" "postgres" "")))

(defvar *testpm-spec* 
  '(:postmodern (:postgresql "127.0.0.1" "elepm" "postgres" "")))

(defvar *testsqlite3-spec*
  `(:clsql (:sqlite3 
	    ,(namestring
	      (merge-pathnames 
	       #p"tests/sqlite3-test.db"
	       (asdf:component-pathname (asdf:find-system 'elephant-tests))))))
  "This is of the form '(filename &optional init-function),")

(defvar *testsqlite3-spec2*
  `(:clsql (:sqlite3 
	    ,(namestring
	      (merge-pathnames 
	       #p"tests/sqlite3-test2.db"
	       (asdf:component-pathname (asdf:find-system 'elephant-tests))))))
  "This is of the form '(filename &optional init-function),")

(defvar *testsqlite3-spec-oid*
  `(:clsql (:sqlite3 
	    ,(namestring
	      (merge-pathnames 
	       #p"tests/sqlite3-test-oid.db"
	       (asdf:component-pathname (asdf:find-system 'elephant-tests))))))
  "This is of the form '(filename &optional init-function),")

(defvar *testsqlite3-memory-spec*
  '(:clsql (:sqlite3 :memory))
  "Using :memory: as a file name will get you an completely in-memory system")





;;
;; COMPREHENSIVE TESTING
;;

(defun run-regression-tests (type)
  (let (sc1 sc2 oid)
    (case type
      (:BDB 
       (setf sc1 *testbdb-spec*
	     sc2 *testbdb-spec2*
	     oid *testbdb-spec-oid*))
      (:SQLITE
       (setf sc1 *testsqlite3-spec*
	     sc2 *testsqlite3-spec2*))
      (:POSTGRES
       (setf sc1 *testpg-spec*
	     sc2 *testpg-spec*))
      (t (error "Unrecognized data store type: ~A" type)))
    (let ((*test-spec-primary* sc1)
	  (*test-spec-secondary* sc2))
      (declare (special *test-spec-secondary* *test-spec-primary*))
      (do-backend-tests sc1))))

;;
;; GUIDE TO SUBTESTS
;;
;; 1) Call (do-backend-tests <specname>) to test the standard API
;; 3) To test migration: (do-migration-tests <first-spec> <second-spec>) inserting a second
;;    spec, typically a bdb spec or create another instance of a sql db depending on 
;;    your configuration
;; 4) A data store is green if it passes do-backend-tests and can succesfully be
;;    used as spec1 or spec2 argument in the migration test
;;

(defvar *default-spec* nil
  "Set this at the REPL to have the following interfaces default to a given spec
   mostly here to save typing...")

(defun do-backend-tests (&optional (spec *default-spec*))
  "Will test a specific backend based on the spec.  Note, 
   if you run a :bdb backend test it will load berkeley db
   specific tests which should silently succeed if you
   test another backend"
  (when (and (consp spec) (symbolp (car spec)))
    (with-open-store (spec)
      (when (eq (car spec) :bdb)
	(asdf:operate 'asdf:load-op :elephant-tests-bdb))
      (do-tests))))
  
(defun do-test-spec (testname &optional (spec *default-spec*))
  "For easy interactive running of single tests while debugging"
  (when spec
    (with-open-store (spec)
      (5am:debug! testname))))

(defun do-migration-tests (spec1 spec2 &optional oid-spec)
  "Interface to do explicit migration tests between backends"
  (let ((*test-spec-primary* spec1)
	(*test-spec-secondary* spec2))
    (declare (special *test-spec-primary* *test-spec-secondary*))
    (if oid-spec
	(set-oid-spec oid-spec)
	(set-oid-spec nil))
    (do-test 'migrate-ipclass)
    (when oid-spec
      (set-oid-spec nil))))
    
(defun do-migration-test-spec (test spec1 spec2)
  (let ((*test-spec-primary* spec1)
	(*test-spec-secondary* spec2))
    (declare (special *test-spec-primary* *test-spec-secondary*))
    (print (do-test test))))


;;
;; Various test groups 
;;

(defun do-indexing-tests (&optional (spec *default-spec*))
  "Just test indexing"
  (with-open-store (spec) 
    (make-stress-classes)
    (print (do-test 'indexing-basic))
    (print (do-test 'indexing-inherit))
    (print (do-test 'indexing-range))
    (print (do-test 'indexing-wipe-index))
    (print (do-test 'indexing-reconnect-db))
    (print (do-test 'indexing-change-class))
    (print (do-test 'indexing-redef-class))
    (print (do-test 'indexing-timing))
    ))

(defun do-collection-tests (&optional (spec *default-spec*))
  "Just test indexing"
  (with-open-store (spec) 
    (print (do-test 'basicpersistence))
    (print (do-test 'testoid))
    (print (do-test 'btree-make))
    (print (do-test 'btree-put))
    (print (do-test 'btree-get))
    (print (do-test 'remove-kv))
    (print (do-test 'removed))
    (print (do-test 'map-btree))
    (print (do-test 'indexed-btree-make))
    (print (do-test 'indexed-btree-make))
    (print (do-test 'add-indices))
    (print (do-test 'test-indices))
    (print (do-test 'indexed-put))
    (print (do-test 'indexed-get))
    (print (do-test 'simple-slot-get))
    (print (do-test 'indexed-get-from-slot1))
    (print (do-test 'indexed-get-from-slot2))
    (print (do-test 'remove-kv-indexed))
    (print (do-test 'no-key-nor-indices))
    (print (do-test 'remove-kv-from-slot1))
    (print (do-test 'no-key-nor-indices-slot1))
    (print (do-test 'remove-kv-from-slot2))
    (print (do-test 'no-key-nor-indices-slot2))
    (print (do-test 'map-indexed))
    (print (do-test 'get-first))
    (print (do-test 'get-first2))
    (print (do-test 'get-last))
    (print (do-test 'get-last2))
    (print (do-test 'set))
    (print (do-test 'set2))
    (print (do-test 'set-range))
    (print (do-test 'set-range2))
    (print (do-test 'rem-kv))
    (print (do-test 'rem-idexkv))
    (print (do-test 'make-indexed2))
    (print (do-test 'add-indices2))
    (print (do-test 'put-indexed2))
    (print (do-test 'get-indexed2))
    (print (do-test 'get-from-index3))
    (print (do-test 'dup-test))
    (print (do-test 'nodup-test))
    (print (do-test 'prev-nodup-test))
    (print (do-test 'pnodup-test))
    (print (do-test 'pprev-nodup-test))
    (print (do-test 'cur-del1))
     (print (do-test 'indexed-delete))
     (print (do-test 'test-deleted))
     (print (do-test 'indexed-delete2))
     (print (do-test 'test-deleted2))
     (print (do-test 'cur-del2))
     (print (do-test 'get-both))
     (print (do-test 'pget-both))
     (print (do-test 'pget-both-range))
     (print (do-test 'pcursor))
     (print (do-test 'newindex))
     (print (do-test 'pcursor2))
     (print (do-test 'add-get-remove))
     (print (do-test 'add-get-remove-symbol))
     (print (do-test 'existsp))
    ))

(defun do-cur-del2-test (&optional (spec *default-spec*))
  "Just test indexing"
  (with-open-store (spec) 
     (print (do-test 'cur-del2))
    ))

(defun do-crazy-pg-tests()
  "Specific problematic pg tests"
  (open-store *testpg-spec*)
  (do-test 'indexed-btree-make)
  (do-test 'add-indices)
  (do-test 'test-indices)
  (do-test 'indexed-put)
  (do-test 'indexed-get)
  (close-store))


;;
;; UTILITIES
;;

(defun find-slot-def (class-name slot-name)
  (find-if #'(lambda (slot-def)
	       (eq (slot-definition-name slot-def) slot-name))
	   (class-slots (find-class class-name))))

(defmacro finishes (&body body)
  `(handler-case
    (progn ,@body)
    (error () nil)
    (warning () t)
    (condition () t)
    (:no-error (&rest rest) (declare (ignore rest)) t)))

(defmacro signals-condition (&body body)
  `(handler-case
       (progn ,@body)
     (condition () t)
     (:no-error (&rest rest) (declare (ignore rest)) nil)))

(defmacro signals-specific-condition ((condition) &body body)
  `(handler-case
       (progn ,@body)
     (,condition () t)
     (:no-error (&rest rest) (declare (ignore rest)) nil)))

(defmacro signals-error (&body body)
  `(handler-case
    (progn ,@body)
    (error () t)
    (:no-error (&rest rest) (declare (ignore rest)) nil)))

(defmacro is-not-null (&body body)
  `(not (null (progn ,@body))))

(defmacro are-not-null (&rest forms)
  `(values
    ,@(loop for form in forms
	    collect `(is-not-null ,form))))

