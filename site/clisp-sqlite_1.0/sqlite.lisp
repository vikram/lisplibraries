(defpackage :sqlite.raw
  (:use :ffi :cl :ext)
  (:export "SQLITE_OPEN"
           "SQLITE_CLOSE"
           "SQLITE_EXEC"
           "SQLITE_LAST_INSERT_ROWID"
           "SQLITE_CHANGES"
           "SQLITE_COMPLETE"))
(defpackage :sqlite
  (:use :ffi :cl :ext :sqlite.raw)
  (:export "SQL"
           "DATABASE-OPENP"
           "DATABASE-NAME"
           "DATABASE-ADDRESS"
           "WITH-QUERY"
           "SQLITE-LAST-INSERT-ROWID"
           "SQLITE-OPEN"
           "WITH-OPEN-DB"
           "SQLITE-CLOSE"))

;; Represents a database pointer returned from sqlite
(in-package :sqlite.raw)
(default-foreign-language :stdc)
(def-c-type sqlite-ptr c-pointer)

(def-call-out sqlite_open (:library "libsqlite.so")
  (:arguments (name c-string) (mode sint32)
              (errors (c-ptr c-string) :out))
  (:return-type sqlite-ptr)
  (:documentation "Attempt to open a database. Returns a
database-pointer and nil on success, or nil and an error string on
failure. You must free the error message with foreign-free. The first
argument is the name of the database file to open, the second argument
is ignored, but pass zero for safety."))

(def-call-out sqlite_close (:library "libsqlite.so")
  (:arguments (db sqlite-ptr))
  (:documentation "Close a database. Pass a database handle returned
by sqlite_open, and don't use it again."))

(def-c-type sqlite-callback
  (c-function (:arguments (any c-pointer)
                          (argc sint32)
                          (argv (c-array-ptr c-string))
                          (column-names (c-array-ptr c-string)))
              (:return-type sint32)))

(def-call-out sqlite_exec (:library "libsqlite.so")
  (:arguments (db sqlite-ptr)
              (sql c-string)
              (callback sqlite-callback)
              (any c-pointer)
              (err-msg (c-ptr c-string) :out))
  (:return-type uint32)
  (:documentation "Run a query, generating a callback on each returned
row. Arg0 is a database handle returned by sqlite_open. Arg1 is an SQL
query string. Callback is a function whose arguments are (any argc
vals coldata), where any is the same any passed to sqlite_exec,
integer argc is the length of the vals vector, string vector vals
represents column data, and string vector coldata is argc column names
followed by argc column types. The callback function should return 0
on success or nonzero if the query should terminate. Lastly,
sqlite_exec returns 0 and nil on success or an error code and string
error message on failure. You must free the error message with
foreign-free. The error code is one of the +sqlite-..+ constants."))

(def-call-out sqlite_last_insert_rowid (:library "libsqlite.so")
  (:arguments (db sqlite-ptr))
  (:return-type sint32)
  (:documentation "Return the id of the last inserted row"))

(def-call-out sqlite_changes (:library "libsqlite.so")
  (:arguments (db sqlite-ptr))
  (:return-type sint32)
  (:documentation "The numbers of rows inserted, deleted, modified by
sqlite_exec or since the last sqlite_compile."))

(def-call-out sqlite_complete (:library "libsqlite.so")
  (:arguments (sql c-string))
  (:return-type boolean)
  (:documentation "Returns true if the sql query is complete (may not
detect syntax errors)."))

(in-package :sqlite)
(defclass database ()
  ((address :reader database-address
            :initform nil
            :initarg :address
            :documentation "Stores the foreign address of the database connection.")
   (openp :reader database-openp
           :initform nil
           :initarg :openp
           :documentation "Is the current address open?")
   (name :reader database-name
         :initform nil
         :initarg :name
         :documentation "The print name of the database")))

(defmethod print-object ((db database) stream)
  (format stream "#<~s ~s ~s>" (class-name (class-of db)) (database-name db)
          (if (database-openp db) :open :closed)))

(defun sqlite-close (db)
  "Close a database connection."
  (sqlite_close (database-address db))
  (setf (slot-value db 'openp) nil)
  db)

(defun sqlite-exec (db query callback &optional (argument nil))
  ;;  "A wrapper for sqlite_exec which does error checking. Like sqlite_exec, your callback function's signature should either be
  ;;(&rest r) or (any argc vals names). Your callback must return 0 if the query is to continue, or any other number if the query should stop. If
  ;;your callback fails to return an integer, you'll get odd-looking FFI errors about coercing to sint32."
  (multiple-value-bind (code err)
      (sqlite_exec (database-address db) query callback argument)
    (if (zerop code)
        t
      (error "sql error ~s: ~s. Offending query: ~s" code err query))))

(defun sqlite-last-insert-rowid (db)
  (sqlite_last_insert_rowid (database-address db)))
(defun sqlite-changes (db)
  (sqlite_changes (database-address db)))
(defun sqlite-complete (sql)
  (sqlite_complete sql))

(defun sqlite-open (name &optional (mode 0))
  "Open a database, checking for errors."
  (multiple-value-bind (db err)
      (sqlite_open (namestring name) mode)
    (unless (null err)
      (error "sqlite_open: ~s" err))
    (make-instance 'database :address db :name name :openp t)))

(defmacro with-open-db ((db filename) &body body)
  "Convenience function for making sure database handles get freed."
  `(let ((,db (sqlite-open ,filename)))
     (unwind-protect
         (progn ,@body)
       (sqlite-close ,db))))

(defun sql (db query)
  "Execute an sql query. If *sql-convert-values* is set, then the
result strings will be interpreted by sql-dtranslate. This function is
fairly inefficient because it aggregates the whole table by pushing
the results onto a list and reversing them just before returning, but
good for interacting with the prompt. It's probably better to use
with-query in production code."
  (let* ((rows nil)
         (appender (lambda (any argc argv column-names)
                     (declare (ignore any argc column-names))
                     (push (coerce argv 'list) rows)
                     0)))
    (sqlite-exec db query appender)
    (nreverse rows)))

(defmacro with-query ((db arg-list query &rest qargs) &body body)
  "A thoroughly useful function, your body is executed for each row
returned by query. The columns in the result are assigned to the
variables in arg-list so they can be easily accessed."
  (let ((qproc (gensym))
        (realquery (gensym))
        (reallambda (gensym))
        (argv (gensym))
        (argc (gensym))
        (column-names (gensym))
        (any (gensym)))
    `(let* ((,realquery ,(if qargs
                             `(format nil ,(if (listp query)
                                               (apply #'ext:string-concat query)
                                             query)
                                      ,@qargs)
                           (if (listp query)
                               (apply #'string-concat query)
                             query)))
            (,reallambda (lambda ,arg-list ,@body))
            (,qproc (lambda (,any ,argc ,argv ,column-names)
                      (declare (ignore ,any ,argc ,column-names))
                      (let ,(loop for i from 0
                                  for arg in arg-list
                                  collect `(,arg (aref ,argv ,i)))
                        ,@body)
                      0)))
            (sqlite-exec ,db ,realquery ,qproc))))

(defvar *sql-convert-values* nil
  "If non-nil, values returned from sql will be converted from strings to lisp types")
