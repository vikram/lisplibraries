;; This software is Copyright (c) 2006, Steffen Siering.
;; <COMPANY> grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :bdb)

;;;; * Cursors

(defmethod db-cursor (db &key txn write)
  (make-instance 'cursor
		 :handle (%db-cursor db txn
				     :write write)))

(defmethod db-cursor ((db db) &key txn write)
  (db-cursor (db-handle db) :txn txn :write write))

(defmethod db-cursor ((db db-txn) &key txn write)
  (db-cursor (if (is-secondary db)
		 (db-sec-handle db)
		 (db-handle db))
	     :txn txn
	     :write write))

(defmethod db-cursor ((db db-ext) &key txn write)
  (make-instance 'ext-cursor
		 :handle (%db-cursor (if (is-secondary db)
					 (db-sec-handle db)
					 (db-handle db))
				     txn
				     :write write)
		 :env (db-get-env db)))

(defmethod db-cursor-close ((cursor cursor))
  (%db-cursor-close cursor)
  (setf (slot-value cursor 'cursor-handle) nil))

(defmethod db-cursor-get ((cursor cursor) &key key data
		      ;;flags
		      current first get-both get-both-range
		      get-recno join-item last
		      next next-dup next-no-dup
		      prev prev-no-dup set set-range set-recno
		      multiple multiple-key rmw not-found)
  (let ((tmp-key-buf (make-get-buffer key))
	(tmp-data-buf (make-get-buffer data)))
    (labels ((get-by-buffer (key data)
	       (multiple-value-bind (ret-code key-ptr key-size
					      data-ptr data-size)
		   (%db-cursor-get cursor
				   (cbuffer-data key) (cbuffer-size key)
				   (cbuffer-length key)
				   (cbuffer-data data) (cbuffer-size data)
				   (cbuffer-length data)
				   :current current :first first
				   :get-both get-both
				   :get-both-range get-both-range
				   :get-recno get-recno :join-item join-item
				   :last last :next next :next-dup next-dup
				   :next-no-dup next-no-dup :prev prev
				   :prev-no-dup prev-no-dup :set set
				   :set-range set-range :set-recno set-recno
				   :multiple multiple
				   :multiple-key multiple-key
				   :rmw rmw)
		 (handle-get-code
		  ret-code
		  (lambda () (values (make-cbuffer-from-pointer key-ptr key-size)
				(make-cbuffer-from-pointer data-ptr
							   data-size)))
		  (lambda ()
		    (free-cbuffer key)
		    (free-cbuffer data)
		    (handle-error not-found ret-code))
		  (lambda ()
		    (get-by-buffer (cbuffer-ensure-size key key-size)
				   (cbuffer-ensure-size data data-size)))
		  (lambda ()
		    (free-cbuffer key)
		    (free-cbuffer data)
		    (bdb-check-error ret-code))))))
      (get-by-buffer tmp-key-buf tmp-data-buf))))

(defmethod db-cursor-get ((cursor ext-cursor) &key key data
			  ;;flags
			  current first get-both get-both-range
			  get-recno join-item last
			  next next-dup next-no-dup
			  prev prev-no-dup set set-range set-recno
			  multiple multiple-key rmw not-found)
  (let ((key-buffer (when key (alloc-cbuffer)))
	(data-buffer (when data (alloc-cbuffer))))
    (unwind-protect
	 (multiple-value-bind (ret-key ret-data)
	     (call-next-method cursor
			       :key (when key
				      (funcall (buf-writer cursor)
					       key key-buffer)
				      key-buffer)
			       :data (when data
				       (funcall (buf-writer cursor)
						data data-buffer)
				       data-buffer)
			       :current current :first first
			       :get-both get-both
			       :get-both-range get-both-range
			       :get-recno get-recno :join-item join-item
			       :last last :next next :next-dup next-dup
			       :next-no-dup next-no-dup :prev prev
			       :prev-no-dup prev-no-dup :set set
			       :set-range set-range :set-recno set-recno
			       :multiple multiple
			       :multiple-key multiple-key
			       :rmw rmw :not-found not-found)
	   (unwind-protect
		(values (funcall (buf-reader cursor) ret-key)
			(funcall (buf-reader cursor) ret-data))
	     (progn
	       (free-cbuffer ret-key)
	       (free-cbuffer ret-data))))
      (progn
	(when data-buffer (free-cbuffer data-buffer))
	(when key-buffer (free-cbuffer key-buffer))))))

(defun build-cursor-get-function (write-fn read-fn)
  (lambda (cursor &key key data
	   ;;flags
	   current first get-both get-both-range
	   get-recno join-item last
	   next next-dup next-no-dup
	   prev prev-no-dup set set-range set-recno
	   multiple multiple-key rmw not-found)
    (let ((key-buffer (when key (alloc-cbuffer)))
	  (data-buffer (when data (alloc-cbuffer))))
      (unwind-protect
	   (multiple-value-bind (ret-key ret-data)
	       (db-cursor-get cursor
			      :key (when key
				     (funcall write-fn key key-buffer)
				     key-buffer)
			      :data (when data
				      (funcall write-fn data data-buffer)
				      data-buffer)
			       :current current :first first
			       :get-both get-both
			       :get-both-range get-both-range
			       :get-recno get-recno :join-item join-item
			       :last last :next next :next-dup next-dup
			       :next-no-dup next-no-dup :prev prev
			       :prev-no-dup prev-no-dup :set set
			       :set-range set-range :set-recno set-recno
			       :multiple multiple
			       :multiple-key multiple-key
			       :rmw rmw :not-found not-found)
	     (unwind-protect
		  (values (funcall read-fn ret-key)
			  (funcall read-fn ret-data))
	       (progn
		 (free-cbuffer ret-key)
		 (free-cbuffer ret-data))))
	(progn
	  (when data-buffer (free-cbuffer data-buffer))
	  (when key-buffer (free-cbuffer key-buffer)))))))

(defmethod db-cursor-put ((cursor cursor) key data &key
		      after before current
		      key-first key-last no-dup-data)
  (%db-cursor-put cursor
		  (cbuffer-data key) (cbuffer-size key)
		  (cbuffer-data data) (cbuffer-size data)
		  :after after
		  :before before
		  :current current
		  :key-first key-first
		  :key-last key-last
		  :no-dup-data no-dup-data))

(defmethod db-cursor-put ((cursor ext-cursor) key data &key
			  after before current
			  key-first key-last no-dup-data)
  (with-cbuffer key-buf
    (with-cbuffer data-buf
      (funcall (buf-writer cursor) key key-buf)
      (funcall (buf-writer cursor) data data-buf)
      (call-next-method cursor key data
		     :after after
		     :before before
		     :current current
		     :key-first key-first
		     :key-last key-last
		     :no-dup-data no-dup-data))))

(defun build-cursor-put-function (write-fn)
  (lambda (cursor key data &key
	   after before current
	   key-first key-last no-dup-data)
    (with-cbuffer key-buf
      (with-cbuffer data-buf
	(funcall write-fn key key-buf)
	(funcall write-fn data data-buf)
	(db-cursor-put cursor key data
		       :after after
		       :before before
		       :current current
		       :key-first key-first
		       :key-last key-last
		       :no-dup-data no-dup-data)))))

(defmethod+ ("db_cursor_del" db-cursor-del) :int
  (cursor :pointer :class (cursor cursor-handle))
  (flags :uint32 :const 0))

(defmethod+ ("db_cursor_count" db-cursor-count) :int
  (cursor :pointer :class (cursor cursor-handle))
  (count :out :uint32)
  (flags :uint32 :const 0))

(defmethod db-cursor-dup ((cursor cursor) &key position)
  (make-instance 'cursor
		 :handle (%db-cursor-dup cursor :position position)))