(defpackage :sqlite-tests
  (:use :cl :sqlite :5am :iter :bordeaux-threads)
  (:export :run-all-tests))

(in-package :sqlite-tests)

(def-suite sqlite-suite)

(defun run-all-tests ()
  (run! 'sqlite-suite))

(in-suite sqlite-suite)

(test test-connect
  (with-open-database (db ":memory:")))

(test test-disconnect-with-statements
  (finishes
    (with-open-database (db ":memory:")
      (prepare-statement db "create table users (id integer primary key, user_name text not null, age integer null)"))))

(defmacro with-inserted-data ((db) &body body)
  `(with-open-database (,db ":memory:")
     (execute-non-query ,db "create table users (id integer primary key, user_name text not null, age integer null)")
     (execute-non-query ,db "insert into users (user_name, age) values (?, ?)" "joe" 18)
     (execute-non-query ,db "insert into users (user_name, age) values (?, ?)" "dvk" 22)
     (execute-non-query ,db "insert into users (user_name, age) values (?, ?)" "qwe" 30)
     ,@body))

(test create-table-insert-and-error
  (with-inserted-data (db)
    (signals error
      (execute-non-query db "insert into users (user_name, age) values (?, ?)" nil nil))))

(test test-select-single
  (with-inserted-data (db)
    (is (= (execute-single db "select id from users where user_name = ?" "dvk")
           2))))

(test test-select-m-v
  (with-inserted-data (db)
    (is (equalp (multiple-value-list (execute-one-row-m-v db "select id, user_name, age from users where user_name = ?" "joe"))
                (list 1 "joe" 18)))))

(test test-select-list
  (with-inserted-data (db)
    (is (equalp (execute-to-list db "select id, user_name, age from users")
                '((1 "joe" 18) (2 "dvk" 22) (3 "qwe" 30))))))

(test test-iterate
  (with-inserted-data (db)
    (is (equalp (iter (for (id user-name age) in-sqlite-query "select id, user_name, age from users where age < ?" on-database db with-parameters (25))
                      (collect (list id user-name age)))
                '((1 "joe" 18) (2 "dvk" 22))))))

(test test-loop-with-prepared-statement
  (with-inserted-data (db)
    (is (equalp (loop
                   with statement = (prepare-statement db "select id, user_name, age from users where age < ?")
                   initially (bind-parameter statement 1 25)
                   while (step-statement statement)
                   collect (list (statement-column-value statement 0) (statement-column-value statement 1) (statement-column-value statement 2))
                   finally (finalize-statement statement))
                '((1 "joe" 18) (2 "dvk" 22))))))

#+thread-support
(defparameter *db-file* "/tmp/test.sqlite")

#+thread-support
(defun ensure-table ()
  (with-open-database (db *db-file*)
    (execute-non-query db "CREATE TABLE IF NOT EXISTS FOO (v NUMERIC)")))

#+thread-support
(test test-concurrent-inserts
  (when (probe-file *db-file*)
    (delete-file *db-file*))
  (ensure-table)
  (unwind-protect
       (do-zillions 10 10000)
    (when (probe-file *db-file*)
    (delete-file *db-file*))))

#+thread-support
(defun do-insert (n timeout)
  "Insert a nonsense value into foo"
  (ignore-errors
    (with-open-database (db *db-file* :busy-timeout timeout)
      (iter (repeat 10000)
            (execute-non-query db "INSERT INTO FOO (v) VALUES (?)" n)))
    t))

#+thread-support
(defun do-zillions (max-n timeout)
  (iterate (for n from 1 to max-n)
           (collect 
               (bt:make-thread (let ((n n))
                                 (lambda ()
                                   (do-insert n timeout))))
             into threads)
           (finally
            (iter (for thread in threads)
                  (for all-ok = t)
                  (for thread-result = (bt:join-thread thread))
                  (unless thread-result
                    (setf all-ok nil))
                  (finally (is-true all-ok "One of inserter threads encountered a SQLITE_BUSY error"))))))
