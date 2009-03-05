;;; Copyright (c) 2005-2006, Mu Aps. All rights reserved.

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

;;; WRITTEN BY: KLAUS HARBO klaus@harbo.net / klaus@mu.dk

(in-package :cl-muproc.compat)

;;; DEBUGGING

(defmacro %with-debugging-stack% (&body body)
  "This macro sets up a debuggable stack, enabling stack dumps
in error and signal handlers."
  `(dbg::with-debugger-stack () ,@body))

;;; SCHEDULING

(defmacro %with-exclusive-access% (&body body)
  ;; do not try to obtain `*giant-lock*' if we are
  ;; executing without preemption and interrupts
  `(mp:with-lock (*giant-lock*) ,@body))

(defmacro %with-lock% ((lock) &body body)
  `(mp:with-lock (,lock) ,@body))

(defmacro %make-lock% (&key name)
  `(mp:make-lock :name ,name))

;;; SPECIALS

(defvar *giant-lock* (%make-lock%))

;;; QUEUES

(defmacro %make-queue% ()
  `(mp:make-mailbox))

(defmacro %enqueue% (queue packet)
  `(mp:mailbox-send ,queue ,packet))

(defmacro %dequeue% (queue)
  `(mp:mailbox-read ,queue :timeout nil))

(defmacro %queue-empty-p% (queue)
  `(mp:mailbox-empty-p ,queue))

;;; PROCESSES

(defmacro %all-processes% ()
  `(mp:list-all-processes))

(defmacro %current-process% ()
  "Returns a reference to the currently running muproc.  The reference
is identical to that of the underlying LISP process."
  'mp:*current-process*)

(defmacro %process-alive-p% (muproc)
  `(mp:process-alive-p ,muproc))

(defmacro %process-interrupt% (muproc fn &rest args)
  `(mp:process-interrupt ,muproc ,fn ,@args))

(defmacro %process-p% (obj)
  `(typep ,obj 'mp:process))

(defmacro %process-plist% (muproc)
  `(mp:process-plist ,muproc))

(defmacro %process-priority% (muproc)
  `(mp:process-priority ,muproc))

(defmacro %process-run-function% (name fn &rest args)
   `(mp:process-run-function ,name nil ,fn ,@args))


;;; TIMERS

(defmacro %make-timer% (fn muproc)
  `(mp:make-timer ,fn ,muproc))

(defmacro %schedule-timer-relative% (timer relative-expiry-time &optional repeat-period)
  `(if ,repeat-period
       (mp:schedule-timer-relative ,timer ,relative-expiry-time ,repeat-period)
       (mp:schedule-timer-relative ,timer ,relative-expiry-time)))

(defmacro %unschedule-timer% (timer)
  `(mp:unschedule-timer ,timer))

(defun invoke-with-timeout (timeout bodyfn timeoutfn)
  (block timeout
    (let* ((process mp:*current-process*)
           (unsheduled? nil)
           (timer (mp:make-timer
                   #'(lambda ()
                       (mp:process-interrupt process
                                             #'(lambda ()
                                                 (unless unsheduled?
                                                   (return-from timeout
                                                     (funcall timeoutfn)))))))))
      (mp:schedule-timer-relative timer timeout)
      (unwind-protect (funcall bodyfn)
        (mp:without-interrupts
         (mp:unschedule-timer timer)
         (setf unsheduled? t))))))


(defmacro %with-timeout% ((seconds &body timeout-forms) &body body)
  "Execute BODY; if execution takes more than SECONDS seconds, terminate
and evaluate TIMEOUT-FORMS."
  `(invoke-with-timeout ,seconds #'(lambda () ,@body)
                        #'(lambda () ,@timeout-forms)))

