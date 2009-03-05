;;;; Copyright (c) 2004-2005, Dirk H.P. Gerrits
;;;; All rights reserved.
;;;;
;;;; This software is licensed under the terms of the BSD license, stated in the
;;;; file named LICENSE.
;;;;
;;;;----------------------------------------------------------------------------
;;;;
;;;; This file implements concurrent processes.
;;;;
;;;; TODO: threads are far too heavy-weight for processes.
;;;;
;;;;----------------------------------------------------------------------------

(in-package :erlisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Process interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass process ()
  ((node :initarg :node :accessor process-node)
   (parent :initarg :parent :accessor process-parent)
   (mailbox :initarg :mailbox :initform (make-mailbox) :accessor process-mailbox))
  (:documentation "A concurrent process."))

(defmethod print-object ((process process) stream)
  (with-slots (name) process
    (print-unreadable-object (process stream :type t :identity t))))

(defgeneric current-process ()
  (:documentation "Return the currently running process on this node."))

(defgeneric spawn (thunk &key node linked arguments)
  (:documentation "Spawn a new process running THUNK and return it.

When NODE is provided it specifies the node on which the process
should be created.  By default this is the local node.

When LINKED is true, the spawning process gets notified when the
spawned process terminates.  LINKED is false by default.

When ARGUMENTS is provided it should be a list of arguments for
THUNK.  Otherwise THUNK is called with no arguments."))
  
(defmethod spawn (thunk &key (node (current-node)) linked arguments)
  (check-type arguments list)
  (when linked
    (error "Linking processes is not currently supported."))
  (let ((thunk (if arguments
                   #'(lambda () (apply thunk arguments))
                   thunk)))
    (let ((thunk #'(lambda () (handler-case (funcall thunk)
				(condition () nil)))))
      (make-process node (current-process) thunk))))

(defgeneric make-process (node parent thunk))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; OS thread processes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass threaded-process (process)
  ((thread :initarg :thread :accessor process-thread)
   (mutex :initform (make-mutex) :accessor process-mutex)
   (message-received :initform (make-event) :accessor process-message-received))
  (:documentation "A concurrent process implemented as an operating system thread."))

(defmethod print-object ((process threaded-process) stream)
  (with-slots (thread name) process
    (print-unreadable-object (process stream :type t :identity t)
      (format stream "thread ~S named ~S" thread name))))

(defvar *threaded-process-cache* (make-hash-table))

(defvar *threaded-process-mutex* (make-mutex))

(defmacro cached-threaded-process (thread)
  `(gethash ,thread *threaded-process-cache*))

(defmethod make-process ((node local-node) parent thunk)
  (let ((process (make-instance 'threaded-process
                                 :node node
                                 :parent parent)))
    (setf (process-thread process)
          (make-thread #'(lambda ()
                           (setf (process-thread process)
                                 (current-thread))
                           (with-mutex (*threaded-process-mutex*)
                             (setf (cached-threaded-process (current-thread))
                                   process))
                           (funcall thunk))))
    process))

(defmethod current-process ()
  (with-mutex (*threaded-process-mutex*)
    (or (cached-threaded-process (current-thread))
        (setf (cached-threaded-process (current-thread))
              (make-instance 'threaded-process
                             :node (current-node)
                             :parent nil
                             :thread (current-thread))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Distributed processes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod make-process ((node remote-node) parent thunk)
  (error "Distribution is not implemented yet."))
