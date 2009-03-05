;;;Copyright (c) 2006-2007, Arthur Smyles
;;;All rights reserved.
;;;
;;;Redistribution and use in source and binary forms, with or without
;;;modification, are permitted provided that the following conditions are met:
;;;
;;;1. Redistributions of source code must retain the above copyright notice,
;;;   this list of conditions and the following disclaimer.
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;         documentation and/or other materials provided with the distribution.
;;;
;;;         THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;;;         AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;;         IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;;         ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
;;;         LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;;         CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;;;         SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;;         INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;;;         CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;;;         ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;;         POSSIBILITY OF SUCH DAMAGE.
;;;         :
(in-package #:cl-berkeley-db.common-lisp.net)
;;;; Making the lisp interface to dbt immutable. 
(defstruct (dbt (:constructor nil)) pointer)
(defstruct (db-dbt (:constructor nil) (:include dbt)))
(defstruct (data-dbt (:constructor nil) (:include dbt)))
(defstruct (app-dbt (:constructor nil) (:include data-dbt))); used by append_recno function
(defstruct (buffer-dbt (:constructor nil) (:include dbt)))

(defun dbt-finalizer (dbt-pointer &key data app_data)
  (lambda ()
    (when data (foreign-free (foreign-slot-value dbt-pointer '__db_dbt 'data)))
    (when app_data (foreign-free (foreign-slot-value dbt-pointer '__db_dbt 'app_data)))
    (foreign-free dbt-pointer)))

(defmethod initialize-instance ((instance dbt) &key (offset 0 offset?) (len 0 len?) 
					       (pointer (foreign-alloc '__db_dbt) pointer?)
					       &allow-other-keys)
      (setf (dbt-pointer instance) pointer)
      (unless pointer?
	(with-foreign-slots ((flags doff dlen app_data ulen size data) pointer __db_dbt)
			    (setf flags (if (or offset? len?) DB_DBT_PARTIAL 0)
				  doff offset
				  dlen len
				  ulen 0
				  data (null-pointer)
				  size 0
				  app_data (null-pointer))))
	;(setf app_data (foreign-alloc :uint :initial-element (sb-kernel:get-lisp-obj-address instance)))
	;user class has to do finalization this
	;(tg:finalize instance (dbt-finalizer pointer)))
    instance)

(defmethod initialize-instance ((instance db-dbt) &key &allow-other-keys)
  (declare (ignore args))
  (let ((instance (call-next-method)))
    (with-foreign-slots ((data flags) (dbt-pointer instance) __db_dbt)
	(setf flags (logior DB_DBT_MALLOC flags)))
    (tg:finalize instance (dbt-finalizer (dbt-pointer instance) :data t))
    instance))

(defmethod initialize-instance ((instance data-dbt) &key bytes &allow-other-keys)
  (let ((instance (call-next-method)))
    (with-foreign-slots ((data size flags) (dbt-pointer instance) __db_dbt)
	(setf data (foreign-string-alloc bytes)
	      size (length bytes)))
         (tg:finalize instance (dbt-finalizer (dbt-pointer instance) :data t))
    instance))

(defmethod initialize-instance ((instance app-dbt) &key bytes pointer appmalloc &allow-other-keys)
  (declare (ignorable bytes))
 (with-foreign-slots ((data flags) pointer __db_dbt)
     ;free the original data
     (foreign-free data)
     (let ((instance (call-next-method)))
	(when appmalloc (setf flags (logior DB_DBT_APPMALLOC flags)))
	instance)))
    
(defmethod initialize-instance ((instance buffer-dbt) &key size &allow-other-keys)
  (let ((instance (call-next-method)))
    (with-foreign-slots ((data ulen flags) (dbt-pointer instance) __db_dbt)
	(setf data (foreign-alloc :uint8 :initial-element 0 :count size)
	      ulen size
	      flags (logior DB_DBT_USERMEM flags))
	(tg:finalize instance (dbt-finalizer (dbt-pointer instance) :data t))
    instance)))

(defun db-dbt (&optional (offset 0 offset?) (len 0))
  (if offset?
    (make-instance 'db-dbt :offset offset :len len)
    (make-instance 'db-dbt)))

(defun data-dbt (bytes &optional (offset 0 offset?) (len 0))
  (if offset?
    (make-instance 'data-dbt :bytes bytes :offset offset :len len)
    (make-instance 'data-dbt :bytes bytes)))

(defun app-dbt (bytes pointer &optional appmalloc)
  (make-instance 'app-dbt :bytes bytes :pointer pointer :appmalloc appmalloc))

(defun buffer-dbt (size &optional (offset 0 offset?) (len 0))
  (if offset?
    (make-instance 'buffer-dbt :size size :offset offset :len len)
    (make-instance 'buffer-dbt :size size)))

(defmacro dbt-slot-function (name slot)
  `(defun ,name (dbt) (when dbt (foreign-slot-value (dbt-pointer dbt) '__db_dbt ',slot))))

(defun dbt-data (dbt)
  (when dbt
  (with-foreign-slots ((data size) (dbt-pointer dbt) __db_dbt)
     (byte-vector data size))))

(dbt-slot-function dbt-size size)
(dbt-slot-function dbt-ulen ulen)
(dbt-slot-function dbt-dlen dlen)
(dbt-slot-function dbt-doff doff)
(dbt-slot-function dbt-flags flags)
				       

      
