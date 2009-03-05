;;; Copyright (c) 2005-2006, Vladimir Sekissov. All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; WRITTEN BY: Vladimir Sekissov svg@surnet.ru

(in-package :cl-muproc.compat)

;;; DEBUGGING

(defmacro %with-debugging-stack% (&body body)
  "This macro sets up a debuggable stack, enabling stack dumps
in error and signal handlers - default no-op implementation"
  `(progn ,@body))

;;; PROCESSES

;; It is safe probably to accesss hash-table without lock under SBCL.
;; All basic operations are protected by spinlocks.
(defvar *process-plists* (make-hash-table :test #'eq))

(declaim (inline process-id))
(defun process-id (muproc)
  (slot-value muproc 'sb-thread::os-thread))

(declaim (inline process-plist))
(defun process-plist (muproc)
  (gethash (process-id muproc) *process-plists*))

(declaim (inline (setf process-plist)))
(defun (setf process-plist) (new muproc)
  (setf (gethash (process-id muproc) *process-plists*) new))

(defun mk-drop-process-plist (muproc)
  (let ((pid (process-id muproc)))
    #'(lambda () (remhash pid *process-plists*))))

(defun %process-alive-p% (muproc)
  (sb-thread:thread-alive-p muproc))

(declaim (inline %process-plist%))
(defun %process-plist% (muproc)
  (process-plist muproc))

(declaim (inline (setf %process-plist%)))
(defun (setf %process-plist%) (new muproc)
  (setf (process-plist muproc) new))

(defun %process-priority% (muproc)
  (declare (ignore muproc))
  0)

(defun %process-run-function% (name fn &rest args)
  (let ((muproc (make-thread #'(lambda () (apply fn args)) :name name)))
    (setf (process-plist muproc) '())
    (sb-ext:finalize muproc (mk-drop-process-plist muproc))
    muproc))

;;; TIMERS

(defmacro %with-timeout% ((seconds &body timeout-forms) &body body)
  "Execute BODY; if execution takes more than SECONDS seconds, terminate
and evaluate TIMEOUT-FORMS."
  `(handler-case (sb-ext:with-timeout ,seconds ,@body)
     (sb-ext:timeout () ,@timeout-forms)))
