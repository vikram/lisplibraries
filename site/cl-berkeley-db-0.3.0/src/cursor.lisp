;;;;Copyright (c) 2006-2007, Arthur Smyles
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

(defstruct (cursor (:constructor nil) (:copier nil)) pointer)

(defmethod initialize-instance ((instance cursor) &key pointer)
  (setf (cursor-pointer instance) pointer)
  instance)

;synonym to db-cursor
(defun cursor (&rest args)
  (apply #'db-cursor args))

(defmacro define-cursor-method (name (&rest args) (slot &optional handle-errors) &body body)
    `(define-struct-method ,name ,args (,slot __dbc :cstruct-reader cursor-pointer ,@(when handle-errors `(:handle-errors ,handle-errors)))
			           ,@body))

(define-db-method db-cursor (db &optional txn &rest flags) (cursor t)
   (with-foreign-object (result :pointer)
       (ff-call :pointer (if txn (txn-pointer txn) (null-pointer)) 
		:pointer result
		:uint32 (apply #'logior flags))
       (make-instance 'cursor :pointer (mem-aref result :pointer))))

(define-cursor-method cursor-close (cursor) (c_close t)
   (ff-call))

(define-cursor-method cursor-count (cursor &rest flags) (c_count t)
   (with-foreign-object (result :uint32)
	(ff-call :pointer result :uint32 (apply #'logior flags))
	(mem-aref result :uint32)))

(define-cursor-method cursor-delete (cursor &rest flags) (c_del t)
   (ff-call :uint32 (apply #'logior flags)))

(define-cursor-method copy-cursor (cursor &rest flags) (c_dup t)
   (with-foreign-object (result :pointer)
	(ff-call :pointer result :uint32 (apply #'logior flags))
	(make-instance 'cursor :pointer (mem-aref result :pointer))))

(define-cursor-method cursor-get (cursor &optional key data &rest flags) (c_get t)
  (unless data (setf data (db-dbt)))
  (unless key (setf key (db-dbt)))
  (handler-case (ff-call :pointer (dbt-pointer key) :pointer (dbt-pointer data) :uint32 (apply #'logior flags))
    (not-found () (values nil nil))
    (:no-error (x) (declare (ignore x)) (values data key))))

(define-cursor-method cursor-pget (cursor key &optional data &rest flags &aux (primary (db-dbt))) (c_pget t)
   (unless data (setf data (db-dbt)))
   (handler-case (ff-call :pointer (dbt-pointer key) :pointer (dbt-pointer primary) :pointer (dbt-pointer data)
			  :uint32 (apply #'logior flags))
     (not-found () (values nil nil nil))
     (:no-error (x) (declare (ignore x)) (values data key primary))))

(define-cursor-method cursor-put (cursor key data &rest flags) (c_put t)
  (ff-call :pointer (dbt-pointer key) :pointer (dbt-pointer data) :uint32 (apply #'logior flags)))

;These functions are variations of the cursor-get
;TODO add rest of get flag and pget
;
(defun cursor-get-first (cursor &rest flags)
  (cursor-get cursor nil nil (apply #'logior DB_FIRST flags)))

(defun cursor-get-next (cursor &rest flags)
  (cursor-get cursor nil nil (apply #'logior DB_NEXT flags)))

(defun cursor-get-last (cursor &rest flags)
  (cursor-get cursor nil nil (apply #'logior DB_LAST flags)))

(defun cursor-get-prev (cursor &rest flags)
  (cursor-get cursor nil nil (apply #'logior DB_PREV flags)))

(defun cursor-get-current (cursor &rest flags)
  (cursor-get cursor nil nil (apply #'logior DB_CURRENT flags)))
	
(defun cursor-get-at (cursor key &rest flags)
  (cursor-get cursor key nil (apply #'logior DB_SET flags)))

(defun cursor-get-at-range (cursor key &rest flags)
  (cursor-get cursor key nil (apply #'logior DB_SET_RANGE flags)))

(defun cursor-get-both (cursor key data &rest flags)
  (cursor-get cursor key data (apply #'logior DB_GET_BOTH flags)))

