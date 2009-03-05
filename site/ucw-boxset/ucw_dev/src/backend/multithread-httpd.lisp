;; -*- lisp -*-

(in-package :it.bese.ucw)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass multithread-httpd-backend (httpd-backend)
    ((control-thread :accessor control-thread)
     (accept-thread :accessor accept-thread)
     (workers :accessor workers :initform '())
     (available-workers :accessor available-workers :initform '())
     (worker-count :accessor worker-count
                   :initarg :initial-worker-count
                   :initform 4
                   :documentation "The number of workers to
initially create and once the server is running how many are
currently hanging around.")
     (worker-max :accessor worker-max
                 :initarg :worker-max
                 :initform 32
                 :documentation "WORKER-MAX is the upper limit
for the number of workers.")
     (request-timeout-length :accessor request-timeout-length :initform 90))
    (:documentation "Generic multithreaded backend.

2 main threads:

 * accept-thread sits in a tight loop waiting for incoming
   connections on the backend's socket. When it gets a connection
   it sends the CONNECTION message to control thread with the
   newly created stream.

 * control-thread sits and reacts to various messages:

   1) SHUTDOWN - sends SHUTDOWN messages to the workers and then
      closes all the sockets and streams.

   2) CONNECTION - Removes a worker from the pool and sends it a
      CONNECTION message. A new worker will be spawned if none
      are available up to worker-max.  When the worker is
      finished it sends a WORKER-DONE message.

   3) WORKER-DONE - A worker has finished processing a request
      and will be restored to the pool.  4) ERROR - A worker has
      encountered an error.

Seperate lists are kept for all workers for use when shutting and
of the available-workers for handling request."))

  (defclass pending-request ()
    ((network-stream :accessor network-stream :initarg :network-stream)
     (peer-address :accessor peer-address :initarg :peer-address)
     (request-start-time :accessor request-start-time :initform (get-universal-time)))
    (:documentation "Used to keep track of how old an unallocated request is."))

  (defclass httpd-worker ()
    ((thread :accessor thread :initform nil)
     (stop-flag :accessor stop-flag :initform nil))))

;;;; The multi thread server

;;;; These are just same convience wrappers around slime's mp
;;;; "primitives"

(defmacro spawn ((name-control &rest name-args) &body body)
  `(progn
     (ucw.backend.dribble "Spawing new thread named ~S with body ~S."
                          (format nil ,name-control ,@name-args) ',body)
     (swank-backend:spawn (lambda () ,@body)
       :name (format nil ,name-control ,@name-args))))

(defmacro send (thread &rest message-contents)
  (rebinding (thread)
    (with-unique-names (message)
      `(let ((,message (list ,@message-contents)))
         (ucw.backend.dribble "Sending ~S to ~S." ,message ,thread)
         (swank-backend:send ,thread ,message)))))

(defmacro receive (&body message-match-clauses)
  (with-unique-names (message)
    `(let ((,message (swank-backend:receive)))
       (ucw.backend.dribble "~S received ~S." (swank-backend:current-thread) ,message)
       (list-match-case ,message
         ,@message-match-clauses
         (?_
          (error "Unknown message ~S received by ~S." ?_ (swank-backend:current-thread)))))))

(defmethod make-worker (&optional number)
  (let ((w (make-instance 'httpd-worker)))
    (setf (thread w) (spawn ("an httpd worker ~a" number)
                       (httpd-worker-loop w)))
    w))

(defun next-available-worker! (backend)
  "Get the next available worker from the backend. If none are
available but we aren't above the max-workers then allocate a new
one."
  (if (available-workers backend)
      (pop (available-workers backend))
      (when (< (worker-count backend) (worker-max backend))
        (ucw.backend.info "Spawning new worker thread because there weren't enough available.")
        (let ((new-worker (make-worker (worker-count backend))))
          (push new-worker (workers backend))
          (incf (worker-count backend))
          new-worker))))

(defmethod pending-request-validp ((backend multithread-httpd-backend)
                                   (pending-request pending-request))
  "Returns T if PENDING-REQUEST is young enough to still be servicable.

This is calculated by comparing the time at which the request
came in with BACKEND's request-timeout-length."
  (< (- (get-universal-time) (request-start-time pending-request))
     (request-timeout-length backend)))

(defun allocate-worker (backend pending-request)
  ;; if we have a worker, tell it to handle the request,
  ;; otherwise requeue the request in the control thread and hopefully
  ;; in the meantime a worker will finish up.
  (if-bind worker (next-available-worker! backend)
    (send (thread worker) 'connection backend pending-request)
    ;; safeguard against a request being requeued to many times.
    (if (pending-request-validp backend pending-request)
        (send (control-thread backend) 'connection pending-request) ;requeue
        (progn
          (close (network-stream pending-request))
          (ucw.backend.error "Incoming request dropped because no workers were available for ~a seconds."
                             (request-timeout-length backend))))))

(defun httpd-controller-loop (backend)
  ;; start the accept-thread
  (send (accept-thread backend) 'start)
  ;; XXX: Leakage: This implementation never removes worker threads
  ;; from swank:*known-processes* (on openmcl).
  (flet ((control-loop-error (condition)
           (if *debug-on-error*
               (swank:swank-debugger-hook condition nil)
               (progn
                 (ucw.backend.error
                  "There was an error in the control loop that was ignored because there is no debugger attached.~%~a"
                  condition)
                 (invoke-restart 'ignore-error)))))
    (loop
       (handler-bind ((error #'control-loop-error))
         (restart-case 
             (receive
               ((shutdown)
                ;; XXX: Not sure we're doing the \"right\" thing with the
                ;; socket, what happens if we're accepting a connection when
                ;; we close the socket?
                ;; If a thread is in an infinite loop I don't think that it
                ;; is going to be shutdown.
                (iterate
                  (for worker in (workers backend))
                  (send (thread worker) 'shutdown))
                (setf (workers backend) '()
                      (available-workers backend) '()
                      (worker-count backend) 0)
                ;; close the socket
                (swank-backend:close-socket (socket backend))
                (setf (socket backend) nil)
                (return-from httpd-controller-loop (values)))
                                
               ((connection ?pending-request)
                (allocate-worker backend ?pending-request))
                                
               ((worker-done ?worker)
                (push ?worker (available-workers backend)))
                                
               ((error ?worker ?condition)
                ;; remove ?worker from backend's workers
                (ucw.backend.error "Worker thread ~S reported ~S." ?worker ?condition)
                (setf (workers backend) (remove ?worker (workers backend)))
                (decf (worker-count backend))))
           (ignore-error ()
             :report "Ignore the error and continue processing."
             nil)
           (kill-control-thread ()
             :report "Return from httpd-controller-loop immediately."
             (return-from httpd-controller-loop nil))
           (shutdown-backend (&optional (force nil))
             :report "Send the backend a shutdown message."
             (spawn ("Shutdown-backend thread")
               (shutdown-backend backend :force force))
             nil))))))

(defun httpd-worker-loop/handle (worker backend pending-request)
  (let ((stream (network-stream pending-request)))
    (flet ((abort-worker (condition)
             (break)
             (ignore-errors
               (ucw.backend.error "Error in worker loop: ~A." condition))
             ;; set the stop flag so that if a connection
             ;; comes in before the shutdown message we
             ;; don't end up here again.
             (setf (stop-flag worker) t)
             (when *debug-on-error*
               (restart-case
                (swank:swank-debugger-hook condition nil)
                (kill-worker ()
                             :report "Kill this worker."
                             t)))
             (send (control-thread backend) 'error worker condition)
             (send (thread worker) 'shutdown)
             (return-from httpd-worker-loop/handle)))
      (handler-bind ((stream-error (lambda (c)
                                     (when (eq (stream-error-stream c) stream)
                                       (abort-backend-request c))))
                     (error #'abort-worker))
        (unwind-protect
             (catch 'abort-backend-request
               (let* ((request (read-request backend stream))
                      (response (make-response request))) 
                 (handle-request backend request response)))
          (ignore-errors
            (close stream)))
        ;; only send worker-done after everything else is complete
        ;; (closing the stream) and there haven't been any errors.
        (send (control-thread backend) 'worker-done worker)))))

(defun httpd-worker-loop (worker)
  (loop
     (receive
       ((shutdown) (return-from httpd-worker-loop nil))
       ((connection ?backend ?pending-request)
        (unless (stop-flag worker)
          (httpd-worker-loop/handle worker ?backend ?pending-request))))))

(defun httpd-accept-loop (backend)
  (loop
     ;; loop until we get a start message
     (receive
       ((start) (return))))
  (iter (for (values stream peer) = (accept-connection (socket backend)
                                                       :element-type '(unsigned-byte 8)))
        (while (socket backend))
        (send (control-thread backend)
              'connection
              (make-instance 'pending-request :network-stream stream
                             :peer-address peer))))

(defmethod startup-backend ((backend multithread-httpd-backend) &rest initargs)
  (declare (ignore initargs))
  (setf (workers backend) (iterate
                            (for n from 0 to (1- (worker-count backend)))
                            (collect (make-worker n)))
        (available-workers backend) (copy-list (workers backend))
        (accept-thread backend) (spawn ("httpd accept connection thread")
                                  (httpd-accept-loop backend))
        (control-thread backend) (spawn ("httpd controller thread")
                                   (httpd-controller-loop backend)))
  backend)

(defmethod shutdown-backend ((backend multithread-httpd-backend)
                             &key force &allow-other-keys)
  (macrolet ((kill-thread-and-catch-error (thread)
                   (rebinding (thread)
                     `(block kill-worker
                        (handler-bind ((error (lambda (c)
                                                (warn "Error while killing ~S: ~S." ,thread c)
                                                (return-from kill-worker (values)))))
                          (swank-backend:kill-thread ,thread))))))
    (if force
        (progn
	  (loop
	     for worker in (workers backend)
	     do (kill-thread-and-catch-error worker))
	  (setf (workers backend) '()
		(available-workers backend) '()
		(worker-count backend) 0)
	  (kill-thread-and-catch-error (accept-thread backend))
	  (kill-thread-and-catch-error (control-thread backend)))
	(progn
	  (when (swank-backend:thread-alive-p (accept-thread backend))
	    (kill-thread-and-catch-error (accept-thread backend)))
	  (send (control-thread backend) 'shutdown)))))

;; Copyright (c) 2003-2006 Edward Marco Baringer
;; All rights reserved. 
;; 
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;; 
;;  - Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 
;;  - Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 
;;  - Neither the name of Edward Marco Baringer, nor BESE, nor the names
;;    of its contributors may be used to endorse or promote products
;;    derived from this software without specific prior written permission.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
