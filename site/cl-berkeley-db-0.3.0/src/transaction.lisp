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
;;;
(in-package #:cl-berkeley-db.common-lisp.net)

(define-condition aborting (db-condition)
  ((transaction :reader aborting-txn :initarg :txn)))

(defstruct (txn (:constructor nil)) pointer)

(defmethod initialize-instance ((instance txn) &key pointer &allow-other-keys)
  (let ((api_handler (foreign-alloc :uint :initial-element (sb-kernel:get-lisp-obj-address instance))))
	  (setf (txn-pointer instance) pointer 
	        (foreign-slot-value pointer '__db_txn 'api_internal) api_handler)
	  (tg:finalize instance (lambda () (foreign-free api_handler)))
	  instance))

(defmacro define-txn-method (name (&rest args) (slot &optional handle-errors) &body body)
    `(define-struct-method ,name ,args (,slot __db_txn :cstruct-reader txn-pointer ,@(when handle-errors `(:handle-errors ,handle-errors)))
			           ,@body))

(define-env-method env-txn-checkpoint (env kbyte minutes &rest flags) (txn_checkpoint t)
   (ff-call :uint32 kbyte :uint32 minutes :uint32 (apply #'logior flags)))

(define-env-method env-txn (env preplist &rest flags) (txn_recover t)
  (error "not implemented"))

(define-env-method env-txn-stats (env &rest flags) (txn_stat t)
  (error "not implemented"))

(define-env-method env-print-txn-stats (env &rest flags) (txn_stat_print t)
   (ff-call :uint32 (apply #'logior flags)))

(define-env-method (setf env-timeout) (timeout env &rest flags) (set_timeout t)
   (ff-call :uint32 timeout :uint32 (apply #'logior flags)))

(define-env-method env-timeout (env &rest flags) (get_timeout t)
   (with-foreign-object (result :uint32)
	(ff-call :pointer result :uint32 (apply #'logior flags))
	(mem-aref result :uint32)))

(define-env-method (setf env-tx-max) (maximum env) (set_tx_max t)
   (ff-call :uint32 maximum))

(define-env-method env-tx-max (env) (get_tx_max t)
   (with-foreign-object (result :uint32)
	(ff-call :pointer result)
	(mem-aref result :uint32)))

(define-env-method (setf env-tx-timestamp) (env timestamp) (set_tx_timestamp t)
   (error "not implemented"))

(define-env-method env-txt-timestamp (env) (get_tx_timestamp t) 
  (error "not implemented"))
  
(define-env-method env-txn-begin (env &optional parent &rest flags) (txn_begin t)
  (with-foreign-object (tid :pointer)
     (ff-call :pointer (if parent (txn-pointer parent) (null-pointer)) 
	      :pointer tid :uint32 (apply #'logior flags))
     (make-instance 'txn :pointer (mem-aref tid :pointer))))

(define-txn-method txn-abort (txn) (abort t)
   (signal 'aborting :txn txn)
   (ff-call))

(define-txn-method txn-commit (txn &rest flags) (commit t)
  (ff-call :uint32 (apply #'logior flags)))

(define-txn-method txn-discard (txn &rest flags) (discard t)
  (ff-call :uint32 (apply #'logior flags)))

(define-txn-method txn-id (txn) (id :uint32)
  (ff-call))

;TODO learn more about this
(define-txn-method txn-prepare (txn gid) (prepare t)
  (ff-call :pointer gid))

(define-txn-method (setf txn-name) (name txn) (set_name t)
  (ff-call :string name))

(define-txn-method txn-name (txn) (get_name t)
  (with-foreign-object (result :string)
     (ff-call :pointer result)
     (mem-aref result :string)))

(define-txn-method (setf txn-timeout) (timeout txn &rest flags) (set_timeout t)
   (ff-call db_timeout_t timeout :uint32 (apply #'logior flags)))


