;;;; Copyright (c) 2004-2005, Dirk H.P. Gerrits
;;;; All rights reserved.
;;;;
;;;; This software is licensed under the terms of the BSD license, stated in the
;;;; file named LICENSE.
;;;;
;;;;----------------------------------------------------------------------------
;;;;
;;;; This file tests the process classes.
;;;;
;;;;----------------------------------------------------------------------------

(in-package :erlisp)
(def-suite :erlisp.process :in :erlisp)
(in-suite :erlisp.process)

;; Local process creation.
(test local-process-creation
  (let ((process (spawn #'(lambda () 42))))
    (is-true (typep process 'process))
    (is (eq (process-node process) (current-node)))
    (is (eq (process-parent process) (current-process)))
    (is-true (process-mailbox process))
    (is (eql (mailbox-length (process-mailbox process)) 0))
    (is-false (eq (process-thread process) (current-thread)))))

;; Unhandled conditions should only kill the thread they 
;; occur in, rather than interrupting the entire image.
;; This test creates a thread which will produce an 
;; unhandled condition. If that thread quietly disappears,
;; rather than interrupting the image, then the test will
;; succeed.
(test condition-terminates-process
  (flet ((generate-exception () (/ 1 0)))
    (spawn #'generate-exception)
    (is (eql (receive ((:timeout 5) 42))
	     42))))