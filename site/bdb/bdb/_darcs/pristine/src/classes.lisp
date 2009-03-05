
(in-package :bdb)

;;;; * BDB Classes

(defclass db-env ()
  ((db-env-handle :accessor db-env-handle :initarg :handle))
  (:documentation "holds database environment"))

(defclass db-ext-env (db-env)
  ((buf-reader :accessor buf-reader :initarg :buf-reader)
   (buf-writer :accessor buf-writer :initarg :buf-writer))
  (:documentation "extended db environment using special buffer
                   readers and writers. All Databases and Cursor
                   opened according to this environment are using
                   the reader and writer functions"))

(defclass db ()
  ((db-handle :accessor db-handle :initarg :handle))
  (:documentation "low level database handle"))

(defclass cursor ()
  ((cursor-handle :accessor cursor-handle :initarg :handle)))

(defclass db-sequence ()
  ((sequ-handle :accessor sequ-handle :initarg :handle)))

(defclass db-sequence-txn (db-sequence)
  ((db :accessor db :initarg :db)))

(defclass ext-cursor (cursor)
  ((env :accessor db-get-env :initarg :env))
  (:documentation "this cursor is generated, when you use db-cursor with
                   an database being type of db-ext")
  )

(defclass txn ()
  ((txn-handle :accessor txn-handle :initarg :handle)))

;; higher level database classes, to handle primary and secondary databases

(defclass db-std (db)
  ()
  (:documentation "simple standart database class for databases without
                   transaction support"))

(defclass db-txn (db)
  ((db-assoc :accessor db-assoc
	     :initform (list)
	     :documentation "pairs of callbacks and secondaries, if
                             primary database, else hold primary as reference")
   (sec-handle :accessor db-sec-handle
	       :initform nil))
  (:documentation "transactional database class"))

(defclass db-ext (db-txn)
  ((env :accessor db-get-env :initarg :env))
  (:documentation "when using db-open with an environment of db-ext-env,
                   this will be returned. Using this type of Database, the
                   user don't need to worry about cbuffers"))

(defmethod is-primary ((db db-txn))
  (and (null (db-sec-handle db))
       (listp (db-assoc db))))

(defmethod is-secondary ((db db-txn))
  (not (is-primary db)))

(defmethod buf-reader ((db db-ext))
  (buf-reader (db-get-env db)))

(defmethod buf-writer ((db db-ext))
  (buf-writer (db-get-env db)))

(defmethod buf-reader ((cursor ext-cursor))
  (buf-reader (db-get-env cursor)))

(defmethod buf-writer ((cursor ext-cursor))
  (buf-writer (db-get-env cursor)))

(defmethod db-get-env ((sequ db-sequence-txn))
  (db-get-env (db sequ)))