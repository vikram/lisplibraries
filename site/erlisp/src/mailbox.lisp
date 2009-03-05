;;;; Copyright (c) 2004-2005, Dirk H.P. Gerrits
;;;; All rights reserved.
;;;;
;;;; This software is licensed under the terms of the BSD license, stated in the
;;;; file named LICENSE.
;;;;
;;;;----------------------------------------------------------------------------
;;;;
;;;; This file implements the process mailbox class.
;;;;
;;;; TODO: I don't like the TAKE-IF/RESET protocol very much, neither the
;;;;       interface, nor the implementation.  Is there a better way to only
;;;;       examine each message at most once for each RECEIVE?
;;;;
;;;;----------------------------------------------------------------------------

(in-package :erlisp)

(defclass mailbox ()
  ((non-matching :initform '() :accessor mailbox-non-matching :documentation
    "Received messages not matching any clause in the current RECEIVE statement.")
   (newly-arrived :initarg :contents :accessor mailbox-newly-arrived :documentation
    "Received messages not yet matched against clauses in a RECEIVE statement."))
  (:documentation
   "A mailbox holding all the received but unprocessed messages of a process."))

(defmethod print-object ((mailbox mailbox) stream)
  (with-slots (non-matching newly-arrived) mailbox
    (print-unreadable-object (mailbox stream :type t :identity t)
      (format stream "(~{~S ~}|~{ ~S~})" non-matching newly-arrived))))

(defgeneric make-mailbox (&key initial-contents)
  (:documentation "Return a new mailbox with initial messages from INITIAL-CONTENTS.")  
  (:method (&key initial-contents)
    (make-instance 'mailbox
                   :contents (if (listp initial-contents)
                                 (copy-list initial-contents)
                                 (coerce initial-contents 'list)))))

(defgeneric mailbox-contents (mailbox)
  (:documentation "Return a sequence of all messages in MAILBOX.")
  (:method ((mailbox mailbox))
    (append (mailbox-non-matching mailbox)
            (mailbox-newly-arrived mailbox))))

(defgeneric mailbox-length (mailbox)
  (:documentation "Return the total number of messages in MAILBOX.")
  (:method ((mailbox mailbox))
    (+ (length (mailbox-non-matching mailbox))
       (length (mailbox-newly-arrived mailbox)))))

(defgeneric mailbox-push (message mailbox)
  (:documentation "Add newly-arrived MESSAGE to the MAILBOX.  Returns MAILBOX.")
  (:method (message (mailbox mailbox))
    (setf (mailbox-newly-arrived mailbox)
          (append (mailbox-newly-arrived mailbox) (list message)))
    mailbox))

(defgeneric mailbox-take-if (predicate mailbox)
  (:documentation "Remove the first newly-arrived message in MAILBOX satisfying PREDICATE.
If a message was found and removed, two values are returned: the message, and the non-nil value returned by PREDICATE being called on that message.
If no message was found, NIL,NIL is returned.
All non-matching messages are no longer newly-arrived.")
  (:method (predicate (mailbox mailbox))
    (loop for (message . more-messages) on (mailbox-newly-arrived mailbox)
          for i upfrom 0
          for bool = (funcall predicate message)
          when bool
            do (setf (mailbox-newly-arrived mailbox)
                     (append (mailbox-non-matching mailbox)
                             non-matching
                             more-messages))
               (setf (mailbox-non-matching mailbox)
                     '())
               (return (values message bool))
          else
            collect message into non-matching
          if (not more-messages)
            do (setf (mailbox-non-matching mailbox)
                     non-matching)
               (setf (mailbox-newly-arrived mailbox)
                     '())
               (return (values nil nil)))))

(defgeneric mailbox-reset (mailbox)
  (:documentation "All messages in MAILBOX get to be newly-arrived again.
This way, they'll be examined by the next MAILBOX-TAKE-IF again.")
  (:method ((mailbox mailbox))
    (with-slots (non-matching newly-arrived) mailbox
      (setf (mailbox-newly-arrived mailbox)
            (append (mailbox-non-matching mailbox) (mailbox-newly-arrived mailbox)))
      (setf (mailbox-non-matching mailbox)
            '()))))
