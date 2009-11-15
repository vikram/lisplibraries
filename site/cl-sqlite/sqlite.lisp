(defpackage :sqlite
  (:use :cl :iter)
  (:export :sqlite-handle
           :connect
           :set-busy-timeout
           :disconnect
           :sqlite-statement
           :prepare-statement
           :finalize-statement
           :step-statement
           :reset-statement
           :statement-column-value
           :bind-parameter
           :execute-non-query
           :execute-to-list
           :execute-single
           :execute-one-row-m-v
           :last-insert-rowid
           :with-transaction
           :with-open-database))

(in-package :sqlite)

;(declaim (optimize (speed 3) (safety 0) (debug 0)))

(defclass sqlite-handle ()
  ((handle :accessor handle)
   (database-path :accessor database-path)
   (cache :accessor cache)
   (statements :initform nil :accessor sqlite-handle-statements))
  (:documentation "Class that encapsulates the connection to the database. Use connect and disconnect."))

(defmethod initialize-instance :after ((object sqlite-handle) &key (database-path ":memory:") &allow-other-keys)
  (cffi:with-foreign-object (ppdb 'sqlite-ffi:p-sqlite3)
    (let ((error-code (sqlite-ffi:sqlite3-open database-path ppdb)))
      (if (eq error-code :ok)
          (setf (handle object) (cffi:mem-ref ppdb 'sqlite-ffi:p-sqlite3)
                (database-path object) database-path)
          (error "Received error code ~A when trying to open sqlite3 database ~A"
                 error-code database-path))))
  (setf (cache object) (make-instance 'sqlite.cache:mru-cache :cache-size 16 :destructor #'really-finalize-statement)))

(defun connect (database-path &key busy-timeout)
  "Connect to the sqlite database at the given DATABASE-PATH. Returns the SQLITE-HANDLE connected to the database. Use DISCONNECT to disconnect.
   Operations will wait for locked databases for up to BUSY-TIMEOUT milliseconds; if BUSY-TIMEOUT is NIL, then operations on locked databases will fail immediately."
  (let ((db (make-instance 'sqlite-handle
                           :database-path (etypecase database-path
                                            (string database-path)
                                            (pathname (namestring database-path))))))
    (when busy-timeout
      (set-busy-timeout db busy-timeout))
    db))

(defun set-busy-timeout (db milliseconds)
  "Sets the maximum amount of time to wait for a locked database."
  (sqlite-ffi:sqlite3-busy-timeout (handle db) milliseconds))

(defun disconnect (handle)
  "Disconnects the given HANDLE from the database. All further operations on the handle are invalid."
  (sqlite.cache:purge-cache (cache handle))
  (iter (with statements = (copy-list (sqlite-handle-statements handle)))
        (declare (dynamic-extent statements))
        (for statement in statements)
        (really-finalize-statement statement))
  (let ((error-code (sqlite-ffi:sqlite3-close (handle handle))))
    (unless (eq error-code :ok)
      (error "Received error code ~A when trying to close ~A (connected to ~A)" error-code handle (database-path handle)))))

(defclass sqlite-statement ()
  ((db :reader db :initarg :db)
   (handle :accessor handle)
   (sql :reader sql :initarg :sql)
   (columns-count :accessor resultset-columns-count)
   (columns-names :accessor resultset-columns-names)
   (parameters-count :accessor parameters-count)
   (parameters-names :accessor parameters-names))
  (:documentation "Class that represents the prepared statement."))

(defmethod initialize-instance :after ((object sqlite-statement) &key &allow-other-keys)
  (cffi:with-foreign-object (p-statement 'sqlite-ffi:p-sqlite3-stmt)
    (cffi:with-foreign-object (p-tail '(:pointer :char))
      (cffi:with-foreign-string (sql (sql object))
        (let ((error-code (sqlite-ffi:sqlite3-prepare (handle (db object)) sql -1 p-statement p-tail)))
          (unless (eq error-code :ok)
            (error "Error when trying to prepare sqlite statement '~A'. Code: ~A, message: ~A" (sql object) error-code (sqlite-ffi:sqlite3-errmsg (handle (db object)))))
          (unless (zerop (cffi:mem-ref (cffi:mem-ref p-tail '(:pointer :char)) :uchar))
            (error "SQL string '~A' contains more than one SQL statements" (sql object)))
          (setf (handle object) (cffi:mem-ref p-statement 'sqlite-ffi:p-sqlite3-stmt)
                (resultset-columns-count object) (sqlite-ffi:sqlite3-column-count (handle object))
                (resultset-columns-names object) (loop
                                                    for i below (resultset-columns-count object)
                                                    collect (sqlite-ffi:sqlite3-column-name (handle object) i))
                (parameters-count object) (sqlite-ffi:sqlite3-bind-parameter-count (handle object))
                (parameters-names object) (loop
                                             for i from 1 to (parameters-count object)
                                             collect (sqlite-ffi:sqlite3-bind-parameter-name (handle object) i))))))))

(defun prepare-statement (db sql)
  "Prepare the statement to the DB that will execute the commands that are in SQL.

Returns the SQLITE-STATEMENT.

SQL must contain exactly one statement.
SQL may have some positional (not named) parameters specified with question marks.

Example:

 select name from users where id = ?"
  (or (sqlite.cache:get-from-cache (cache db) sql)
      (let ((statement (make-instance 'sqlite-statement :db db :sql sql)))
        (push statement (sqlite-handle-statements db))
        statement)))

(defun really-finalize-statement (statement)
  (setf (sqlite-handle-statements (db statement))
        (delete statement (sqlite-handle-statements (db statement))))
  (sqlite-ffi:sqlite3-finalize (handle statement)))

(defun finalize-statement (statement)
  "Finalizes the statement and signals that associated resources may be released.
Note: does not immediately release resources because statements are cached."
  (progn
    (let ((error-code (sqlite-ffi:sqlite3-reset (handle statement))))
     (unless (eq error-code :ok)
       (error "When resetting statement ~A (sql: ~A), error ~A (~A)" statement (sql statement) error-code (sqlite-ffi:sqlite3-errmsg (handle (db statement))))))
    (sqlite.cache:put-to-cache (cache (db statement)) (sql statement) statement)))

(defun step-statement (statement)
  "Steps to the next row of the resultset of STATEMENT.
Returns T is successfully advanced to the next row and NIL if there are no more rows."
  (let ((error-code (sqlite-ffi:sqlite3-step (handle statement))))
    (case error-code
      (:done nil)
      (:row t)
      (t (error "When stepping statement ~A (sql: ~A), error ~A (~A)" statement (sql statement) error-code (sqlite-ffi:sqlite3-errmsg (handle (db statement))))))))

(defun reset-statement (statement)
  "Resets the STATEMENT and prepare it to be called again."
  (let ((error-code (sqlite-ffi:sqlite3-reset (handle statement))))
    (unless (eq error-code :ok)
      (error "When resetting statment ~A (sql: ~A), error ~A (~A)" statement (sql statement) error-code (sqlite-ffi:sqlite3-errmsg (handle (db statement)))))))

(defun statement-column-value (statement column-number)
  "Returns the COLUMN-NUMBER-th column's value of the current row of the STATEMENT. Columns are numbered from zero.
Returns:
 * NIL for NULL
 * INTEGER for integers
 * DOUBLE-FLOAT for floats
 * STRING for text
 * (SIMPLE-ARRAY (UNSIGNED-BYTE 8)) for BLOBs"
  (let ((type (sqlite-ffi:sqlite3-column-type (handle statement) column-number)))
    (ecase type
      (:null nil)
      (:text (sqlite-ffi:sqlite3-column-text (handle statement) column-number))
      (:integer (sqlite-ffi:sqlite3-column-int64 (handle statement) column-number))
      (:float (sqlite-ffi:sqlite3-column-double (handle statement) column-number))
      (:blob (let* ((blob-length (sqlite-ffi:sqlite3-column-bytes (handle statement) column-number))
                    (result (make-array (the fixnum blob-length) :element-type '(unsigned-byte 8)))
                    (blob (sqlite-ffi:sqlite3-column-blob (handle statement) column-number)))
               (loop
                  for i below blob-length
                  do (setf (aref result i) (cffi:mem-aref blob :unsigned-char i)))
               result)))))

(defun execute-non-query (db sql &rest parameters)
  "Executes the query SQL to the database DB with given PARAMETERS. Returns nothing.

Example:

\(execute-non-query db \"insert into users (user_name, real_name) values (?, ?)\" \"joe\" \"Joe the User\")

See BIND-PARAMETER for the list of supported parameter types."
  (declare (dynamic-extent parameters))
  (let ((stmt (prepare-statement db sql)))
    (iter (for i from 1)
          (declare (type fixnum i))
          (for value in parameters)
          (bind-parameter stmt i value))
    (step-statement stmt)
    (finalize-statement stmt)
    (values)))

(defun execute-to-list (db sql &rest parameters)
  "Executes the query SQL to the database DB with given PARAMETERS. Returns the results as list of lists.

Example:

\(execute-to-list db \"select id, user_name, real_name from users where user_name = ?\" \"joe\")
=>
\((1 \"joe\" \"Joe the User\")
 (2 \"joe\" \"Another Joe\")) 

See BIND-PARAMETER for the list of supported parameter types."
  (declare (dynamic-extent parameters))
  (let ((stmt (prepare-statement db sql))
        result)
    (iter (for i from 1)
          (declare (type fixnum i))
          (for value in parameters)
          (bind-parameter stmt i value))
    (loop (if (step-statement stmt)
              (push (iter (for i from 0 below (the fixnum (sqlite-ffi:sqlite3-column-count (handle stmt))))
                          (declare (type fixnum i))
                          (collect (statement-column-value stmt i)))
                    result)
              (return)))
    (finalize-statement stmt)
    (nreverse result)))

(defun execute-one-row-m-v (db sql &rest parameters)
  "Executes the query SQL to the database DB with given PARAMETERS. Returns the first row as multiple values.

Example:
\(execute-one-row-m-v db \"select id, user_name, real_name from users where id = ?\" 1)
=>
\(values 1 \"joe\" \"Joe the User\")

See BIND-PARAMETER for the list of supported parameter types."
  (let ((stmt (prepare-statement db sql)))
    (unwind-protect
         (progn
           (iter (for i from 1)
                 (declare (type fixnum i))
                 (for value in parameters)
                 (bind-parameter stmt i value))
           (if (step-statement stmt)
               (return-from execute-one-row-m-v
                 (values-list (iter (for i from 0 below (the fixnum (sqlite-ffi:sqlite3-column-count (handle stmt))))
                                    (declare (type fixnum i))
                                    (collect (statement-column-value stmt i)))))
               (return-from execute-one-row-m-v
                 (values-list (loop repeat (the fixnum (sqlite-ffi:sqlite3-column-count (handle stmt))) collect nil)))))
      (finalize-statement stmt))))

(defun statement-parameter-index (statement parameter-name)
  (sqlite-ffi:sqlite3-bind-parameter-index (handle statement) parameter-name))

(defun bind-parameter (statement parameter value)
  "Sets the PARAMETER-th parameter in STATEMENT to the VALUE.
Parameters are numbered from one.
Supported types:
 * NULL. Passed as NULL
 * INTEGER. Passed as an 64-bit integer
 * STRING. Passed as a string
 * FLOAT. Passed as a double
 * (VECTOR (UNSIGNED-BYTE 8)) and VECTOR that contains integers in range [0,256). Passed as a BLOB"
  (let ((index (etypecase parameter
                 (integer parameter)
                 (string (statement-parameter-index statement parameter)))))
    (declare (type fixnum index))
    (let ((error-code (typecase value
                        (null (sqlite-ffi:sqlite3-bind-null (handle statement) index))
                        (integer (sqlite-ffi:sqlite3-bind-int64 (handle statement) index value))
                        (double-float (sqlite-ffi:sqlite3-bind-double (handle statement) index value))
                        (real (sqlite-ffi:sqlite3-bind-double (handle statement) index (coerce value 'double-float)))
                        (string (sqlite-ffi:sqlite3-bind-text (handle statement) index value -1 (sqlite-ffi:destructor-transient)))
                        ((vector (unsigned-byte 8)) (cffi:with-pointer-to-vector-data (ptr value)
                                                      (sqlite-ffi:sqlite3-bind-blob (handle statement) index ptr (length value) (sqlite-ffi:destructor-transient))))
                        (vector (cffi:with-foreign-object (array :unsigned-char (length value))
                                  (loop
                                     for i from 0 below (length value)
                                     do (setf (cffi:mem-aref array :unsigned-char i) (aref value i)))
                                  (sqlite-ffi:sqlite3-bind-blob (handle statement) index array (length value) (sqlite-ffi:destructor-transient))))
                        (t (error "Do not know how to pass value ~A of type ~A to sqlite" value (type-of value))))))
      (unless (eq error-code :ok)
        (error "When binding parameter ~A to value ~A for statment ~A (sql: ~A), error ~A (~A)" parameter value statement (sql statement) error-code (sqlite-ffi:sqlite3-errmsg (handle (db statement))))))))

(defun execute-single (db sql &rest parameters)
  "Executes the query SQL to the database DB with given PARAMETERS. Returns the first column of the first row as single value.

Example:
\(execute-single db \"select user_name from users where id = ?\" 1)
=>
\"joe\"

See BIND-PARAMETER for the list of supported parameter types."
  (declare (dynamic-extent parameters))
  (let ((stmt (prepare-statement db sql)))
    (unwind-protect
         (progn
           (iter (for i from 1)
                 (declare (type fixnum i))
                 (for value in parameters)
                 (bind-parameter stmt i value))
           (if (step-statement stmt)
               (statement-column-value stmt 0)
               nil))
      (finalize-statement stmt))))

(defun last-insert-rowid (db)
  "Returns the auto-generated ID of the last inserted row on the database connection DB."
  (sqlite-ffi:sqlite3-last-insert-rowid (handle db)))

(defmacro with-transaction (db &body body)
  "Wraps the BODY inside the transaction."
  (let ((ok (gensym "TRANSACTION-COMMIT-"))
        (db-var (gensym "DB-"))
        (result (gensym "RESULT-")))
    `(let (,ok
           (,db-var ,db))
       (execute-non-query ,db-var "begin transaction")
       (unwind-protect
            (progn
              (let ((,result (progn
                               ,@body)))
                (setf ,ok t)
                ,result))
         (if ,ok
             (execute-non-query ,db-var "commit transaction")
             (execute-non-query ,db-var "rollback transaction"))))))

(defmacro with-open-database ((db path &key busy-timeout) &body body)
  `(let ((,db (connect ,path :busy-timeout ,busy-timeout)))
     (unwind-protect
          (progn ,@body)
       (disconnect ,db))))

(defmacro-driver (FOR vars IN-SQLITE-QUERY query-expression ON-DATABASE db &optional WITH-PARAMETERS parameters)
  (let ((statement (gensym "STATEMENT-"))
        (kwd (if generate 'generate 'for)))
    `(progn (with ,statement = (prepare-statement ,db ,query-expression))
            (finally-protected (when ,statement (finalize-statement ,statement)))
            ,@(when parameters
                    (list `(initially ,@(iter (for i from 1)
                                              (for value in parameters)
                                              (collect `(sqlite:bind-parameter ,statement ,i ,value))))))
            (,kwd ,(if (symbolp vars)
                       `(values ,vars)
                       `(values ,@vars))
                  next (progn (if (step-statement ,statement)
                                  (values ,@(iter (for i from 0 below (if (symbolp vars) 1 (length vars)))
                                                  (collect `(statement-column-value ,statement ,i))))
                                  (terminate)))))))

(defmacro-driver (FOR vars ON-SQLITE-STATEMENT statement)
  (let ((statement-var (gensym "STATEMENT-"))
        (kwd (if generate 'generate 'for)))
    `(progn (with ,statement-var = ,statement)
            (,kwd ,(if (symbolp vars)
                       `(values ,vars)
                       `(values ,@vars))
                  next (progn (if (step-statement ,statement-var)
                                  (values ,@(iter (for i from 0 below (if (symbolp vars) 1 (length vars)))
                                                  (collect `(statement-column-value ,statement-var ,i))))
                                  (terminate)))))))