;;;; Copyright (c) 2004-2005, Dirk H.P. Gerrits
;;;; All rights reserved.
;;;;
;;;; This software is licensed under the terms of the BSD license, stated in the
;;;; file named LICENSE.
;;;;
;;;;----------------------------------------------------------------------------
;;;;
;;;; This file tests messaging among processes.
;;;;
;;;;----------------------------------------------------------------------------

(in-package :erlisp)
(def-suite :erlisp.messaging :in :erlisp)
(in-suite :erlisp.messaging)

(test receive-from-self
  (let* ((process (current-process))
         (mailbox (process-mailbox process))
         (old-contents (mailbox-contents mailbox)))
    (mailbox-push #1='#:message mailbox)
    (is (eql (receive-with-matcher (case-matcher)
               (#1# #2='#:return))
             #2#))
    (is (eql (mailbox-length mailbox) (length old-contents)))
    (is-true (every #'eql (mailbox-contents mailbox) old-contents))))

(test receive-with-timeout
  (let ((start (get-internal-real-time)))
    (is (eql (receive ((:timeout 5) 42))
             42))
    (finishes (receive ((:timeout 5 :ms) 42)))
    (finishes (receive ((:timeout 5/1000 :s) 42)))
    (finishes (receive ((:timeout 5/60000 :min) 42)))
    (let* ((end (get-internal-real-time))
           (delta-ms (/ (* 1000 (- end start)) internal-time-units-per-second)))
      (is (<= 10 delta-ms 50)))))

(test send-to-self
  (let* ((process (current-process))
         (mailbox (process-mailbox process))
         (old-contents (mailbox-contents mailbox))
         (new-contents (concatenate 'list old-contents (list #1='#:message))))
    (test send-to-self.check-arrival
      (send process #1#)
      (is (eql (mailbox-length mailbox) (length new-contents)))
      (is-true (every #'eql (mailbox-contents mailbox) new-contents)))
    (test (send-to-self.check-receival :depends-on send-to-self.check-arrival)
      (is (eql (receive-with-matcher (case-matcher)
                 (#1# #2='#:return))
               #2#))    
      (is (eql (mailbox-length mailbox) (length old-contents)))    
      (is-true (every #'eql (mailbox-contents mailbox) old-contents)))))

(test send/receive-ring
  (let ((parent (current-process))
        (ring (make-array 10))
        (receive-counts (make-array 10 :initial-element 0))
        (send-counts (make-array 10 :initial-element 0)))
    (flet ((pass-message-on (n)
             (receive-with-matcher (case-matcher)
               (0
                (incf (aref receive-counts n))
                (send parent 'done))
               (otherwise
                (incf (aref receive-counts n))
                (send (aref ring (mod (1+ n) 10)) (1- it))
                (incf (aref send-counts n))))))
      (loop for n upfrom 0 below 10
            do (setf (aref ring n)
                     (spawn #'pass-message-on :arguments (list n))))
      ;; send messages around the ring
      (send (aref ring 0) 9)
      (receive-with-matcher (case-matcher)
        (done (pass))))))