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

;;; SCHEDULING

(defmacro %with-exclusive-access% (&body body)
  `(with-lock-held (*giant-lock*) ,@body))

(defmacro %with-lock% ((lock) &body body)
  `(with-lock-held (,lock) ,@body))

(declaim (inline %make-lock%))
(defun %make-lock% (&key name)
  (make-lock name))

;;; SPECIALS

(defvar *giant-lock* (%make-lock%))

;;; QUEUES
(declaim (inline %make-queue%))
(defun %make-queue% ()
  (make-mbox :capacity nil))

(declaim (inline %enqueue%))
(defun %enqueue% (queue packet)
  (nth-value 0 (mbox-send queue packet)))

(declaim (inline %dequeue%))
(defun %dequeue% (queue)
  (nth-value 0 (mbox-read queue)))

(declaim (inline %queue-empty-p%))
(defun %queue-empty-p% (queue)
  (mbox-emptyp queue))

;;; PROCESSES

(defun %all-processes% ()
  (all-threads))

(defun %current-process% ()
  (current-thread))

(defun %process-interrupt% (muproc fn &rest args)
  (interrupt-thread muproc #'(lambda () (apply fn args))))

(defun %process-p% (obj)
  (threadp obj))

;;; TIMERS
(eval-when (:load-toplevel :execute)
  (timer:enable-timers))

(defun %make-timer% (fn muproc)
  (timer:make-timer #'(lambda () (funcall fn muproc))))

(defun %schedule-timer-relative% (timer relative-expiry-time &optional repeat-period)
  (timer:schedule-timer-relative timer relative-expiry-time repeat-period))

(defun %unschedule-timer% (timer)
  (timer:unschedule-timer timer))
