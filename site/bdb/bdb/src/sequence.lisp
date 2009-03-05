
(in-package :bdb)

;;;; * Sequences

(defmethod db-sequ-open ((db db) key &key txn
			      initial-value
			      create exclusive threaded
			      seq-dec seq-inc seq-wrap)
  (let ((sequ (%bdb-sequ-create db))
	(ok))
    (unwind-protect
	 (progn
	   (%db-sequ-set-flags sequ
			       :seq-dec seq-dec
			       :seq-inc seq-inc
			       :seq-wrap seq-wrap)
	   (when initial-value
	     (%db-sequ-init-value sequ initial-value))
	   (%db-sequ-open sequ
			  txn
			  (cbuffer-data key) (cbuffer-size key)
			  :create create
			  :exclusive exclusive
			  :threaded threaded)
	   (setq ok
		 (initialize-sequence db sequ)))
      (unless ok
	(%db-sequ-close sequ)))))

(defmethod db-sequ-open ((db db-txn) key &key txn
			 initial-value
			 create exclusive threaded
			 seq-dec seq-inc seq-wrap)
  (with-txn (txn (db-get-env db) :txn txn)
    (call-next-method db key
		      :txn txn :initial-value initial-value
		      :create create
		      :exclusive exclusive
		      :threaded threaded
		      :seq-dec seq-dec
		      :seq-inc seq-inc
		      :seq-wrap seq-wrap)))

(defmethod db-sequ-open ((db db-ext) key &key txn
			  initial-value
			  create exclusive threaded
			  seq-dec seq-inc seq-wrap)
  (with-cbuffer key-buffer
    (funcall (buf-writer db) key key-buffer)
    (call-next-method db key-buffer
		      :txn txn :initial-value initial-value
		      :create create
		      :exclusive exclusive
		      :threaded threaded
		      :seq-dec seq-dec
		      :seq-inc seq-inc
		      :seq-wrap seq-wrap)))

(defmethod db-sequ-close ((sequ db-sequence))
  (%db-sequ-close (sequ-handle sequ)))

(defmethod initialize-sequence ((db db) sequ)
  (make-instance 'db-sequence :handle sequ))

(defmethod initialize-sequence ((db db-txn) sequ)
  (make-instance 'db-sequence-txn
		 :handle sequ
		 :db db))

(defmethod db-sequ-get ((sequ db-sequence) &key txn (delta 1) no-sync)
  (%db-sequ-get sequ txn delta :txn-no-sync no-sync))

(defmethod db-sequ-get ((sequ db-sequence-txn) &key txn (delta 1) no-sync)
  (with-txn (txn (db-get-env sequ) :txn txn)
    (call-next-method sequ
		      :txn txn
		      :delta delta
		      :no-sync no-sync)))

(defmethod db-sequ-get-flags ((sequ db-sequence))
  (let ((value (%db-sequ-get-flags sequ)))
    (with-flag-system (find-flag-system :bdb-flags)
      (flag-list value
		 '(:seq-dec :seq-inc :seq-wrap)))))

(defmethod db-sequ-remove ((sequ db-sequence) &key txn no-sync)
  (%db-sequ-remove sequ txn :txn-no-sync no-sync))

(defmethod db-sequ-remove ((sequ db-sequence-txn) &key txn no-sync)
  (with-txn (txn (db-get-env sequ) :txn txn)
    (call-next-method sequ
		      :txn txn
		      :no-sync no-sync)))

(defmethod+ ("db_sequence_set_cachesize" db-sequ-set-cachesize) :int
  (sequ :pointer :class (db-sequence sequ-handle))
  (size :int32))

(defmethod+ ("db_sequence_get_cachesize" db-sequ-get-cachesize) :int
  (sequ :pointer :class (db-sequence sequ-handle))
  (size :out :int32))