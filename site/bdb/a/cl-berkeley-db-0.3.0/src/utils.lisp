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

(defmacro with-transaction ((name &rest args) &body body)
  `(let ((,name (env-txn-begin ,@args)))
     (restart-case
      (handler-bind 
	((serious-condition (lambda (err) 
			      (print err)
			      (print (compute-restarts err))
			      (invoke-restart 'abort)
			      (print "aborted")
			      (error err))))
	(multiple-value-prog1 (progn ,@body)
	  (txn-commit ,name)))
      (abort () 
	(print "aborting txn")
	(txn-abort ,name)
	(when (find-restart 'abort) 
		     (print "txn: calling higher abort")
		     (print (compute-restarts))
		     (invoke-restart 'abort))
		 ))))

;TODO Figure out why aborting doesn't get called from with-transaction restart? is this a problem with sbcl or is there something in the spec causing this problem 
(defmacro with-cursor ((name db &optional txn &rest flags) &body body)
  (let ((cursor-closed (make-symbol (format nil "~A?" (string name)))))
    `(let ((,name (db-cursor ,db ,@(when (or flags txn) (list txn))  ,@flags))
	   (,cursor-closed nil))
       (declare (ignore ,cursor-closed))
       (restart-case
	 (unwind-protect
	   (progn ,@body)
	   (cursor-close ,name))
	 (abort () 
	   (print "closing cursor")
	   (cursor-close ,name)
	   (when (find-restart 'abort) 
			(print "cursor: calling higher abort") 
			(invoke-restart 'abort))
		   )))))
