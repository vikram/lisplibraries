;;;; Copyright (c) 2004-2005, Dirk H.P. Gerrits
;;;; All rights reserved.
;;;;
;;;; This software is licensed under the terms of the BSD license, stated in the
;;;; file named LICENSE.
;;;;
;;;;----------------------------------------------------------------------------
;;;;
;;;; This file implements sending messages among processes.
;;;;
;;;;----------------------------------------------------------------------------

(in-package :erlisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Receiving
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *time-units* '((:ms . 1) (:s . 1000) (:min . 60000)))
  
  (defun time-unit-p (obj)
    (and (symbolp obj)
         (assoc (symbol-name obj) *time-units*
                :test #'equal :key #'symbol-name)))
  
  (defun timeout-pattern-p (pattern)
    "Return whether PATTERN is of the form (:TIMEOUT TIME [TIME-UNIT])."
    (timeout-factor pattern))

  (defun timeout-factor (pattern)
    "When PATTERN is (:TIMEOUT TIME [TIME-UNIT]) return the multiplication factor to convert the specified time to milliseconds.  Return NIL otherwise."
    (flet ((factor (timeout time &optional (time-unit :ms))
             (declare (ignore time))
             (and (eq timeout :timeout)
                  (cdr (time-unit-p time-unit)))))
      (ignore-errors (apply #'factor pattern))))

  (defun timeout-milliseconds (pattern)
    "When PATTERN is (:TIMEOUT TIME [TIME-UNIT]), return a form evaluating to the number of milliseconds to wait."
    `(* ,(timeout-factor pattern) ,(cadr pattern)))
  
  (defun parse-receive-clauses (clauses)
    (loop with timeout = nil
          for ((pattern . body) . more-clauses) on clauses
          if (timeout-pattern-p pattern)
            do (if more-clauses
                   (error "Timeout clause should be the last clause in a RECEIVE.")
                   (setq timeout
                         (cons (timeout-milliseconds pattern) body)))
          else
            collect pattern into patterns
            collect body into bodies
          finally (return (values patterns bodies timeout))))

  (defun maybe-wrap-in-timeout (timeout form)
    (if timeout
        `(with-timeout ,timeout ,form)
        form))
  
  (defun generate-receive (matcher clauses extra-args)
    (multiple-value-bind (patterns bodies timeout)
        (parse-receive-clauses clauses)
      (let ((message-var (gensym "MESSAGE")))
        (maybe-wrap-in-timeout timeout
         `(with-slots (mutex message-received mailbox) (current-process)
            (flet ((matcher (,message-var)
                     (match-with-matcher (,matcher ,@extra-args)
                         ,message-var
                       ,@(loop for pattern in patterns
                               for body in bodies
                               collect `(,pattern #'(lambda () ,@body))))))
              (with-mutex (mutex)
                (let ((body nil))
                  (loop until (setq body (nth-value 1 (mailbox-take-if #'matcher mailbox)))
                        do (event-wait message-received mutex))
                  (mailbox-reset mailbox)
                  (funcall body))))))))))

(defmacro receive-with-matcher ((matcher &rest extra-args) &body clauses)
  "Block until the current process's mailbox contains a message matching one of the CLAUSES according to MATCHER and EXTRA-ARGS.
A final clause of the form (:TIMEOUT milliseconds) can be used to specify an (approximate) maximum blocking time."
  (generate-receive matcher clauses extra-args))

(defmacro receive (&body clauses)
  "Block until the current process's mailbox contains a message matching one of the CLAUSES according to the default pattern matcher.
A final clause of the form (:TIMEOUT milliseconds) can be used to specify an (approximate) maximum blocking time."
  (generate-receive *default-pattern-matcher* clauses '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sending
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun send (to-process message)
  "Sends MESSAGE from the current process to TO-PROCESS."
  (send-using-nodes (process-node (current-process))
                    (current-process)
                    (process-node to-process)
                    to-process
                    message))

(defgeneric send-using-nodes (from-node from-process to-node to-process message)
  (:documentation "Sends MESSAGE from FROM-PROCESS at FROM-NODE to TO-PROCESS at TO-NODE."))

(defmethod send-using-nodes ((from-node remote-node) from-process to-node to-process message)
  (error "Can only send messages from a local process."))

(defmethod send-using-nodes (from-node from-process (to-node remote-node) to-process message)
  (error "Distribution is not implemented yet."))

(defmethod send-using-nodes (from-node from-process
                             (to-node local-node) (to-process threaded-process)
                             message)
  (with-slots (mutex message-received mailbox) to-process
    (with-mutex (mutex)
      (mailbox-push message mailbox)
      (event-notify message-received))))
