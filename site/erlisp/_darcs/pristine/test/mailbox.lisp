;;;; Copyright (c) 2004-2005, Dirk H.P. Gerrits
;;;; All rights reserved.
;;;;
;;;; This software is licensed under the terms of the BSD license, stated in the
;;;; file named LICENSE.
;;;;
;;;;----------------------------------------------------------------------------
;;;;
;;;; This file tests the process mailbox class.
;;;;
;;;;----------------------------------------------------------------------------

(in-package :erlisp)
(def-suite :erlisp.mailbox :in :erlisp)
(in-suite :erlisp.mailbox)

;; Mailbox creation.
(test mailbox-creation
  (let ((empty-mailbox (make-mailbox)))
    (is (eq (type-of empty-mailbox) 'mailbox) "EMPTY-MAILBOX isn't really a mailbox")
    (is (eql (mailbox-length empty-mailbox) 0) "EMPTY-MAILBOX isn't empty"))

  (dolist (elements (list (list 1 2 3 4)
                          (make-array 4 :initial-contents '(1 2 3 4)
                                        :adjustable t
                                        :fill-pointer 4)))
    (let ((filled-mailbox (make-mailbox :initial-contents elements)))
      
      (is (eq (type-of filled-mailbox) 'mailbox) "FILLED-MAILBOX isn't really a mailbox")
      (is (eql (mailbox-length filled-mailbox) (length elements))
          "FILLED-MAILBOX doesn't have the right number of initial messages")
      (is-true (every #'eql (mailbox-contents filled-mailbox) elements)
               "FILLED-MAILBOX doesn't have the right initial messages")
      
      (setf (elt elements 1) 42)
      (is-false (find 42 (mailbox-contents filled-mailbox))
                "Mutating initial elements changes FILLED-MAILBOX's current messages")
      
      (if (listp elements)
          (setf (cdr (last elements)) (list 1337))
          (vector-push-extend 1337 elements))
      (is-false (find 1337 (mailbox-contents filled-mailbox))
                "Adding to initial elements adds to FILLED-MAILBOX's current messages"))))

;; Append a message at the end of the mailbox.
(test mailbox-push
  (let ((mailbox (make-mailbox))
        (elements '(1 2 3 4)))
    (loop for element in elements
          for num-messages upfrom 1
          do (mailbox-push element mailbox)
             (is (eql (mailbox-length mailbox) num-messages)
                 "~@(~:R~) push didn't actually add a message" num-messages)
             (is (eql element (elt (mailbox-contents mailbox) (1- num-messages)))
                 "~@(~:R~) push didn't add the right message" num-messages))))

;; Remove the first message in the mailbox satisfying a certain predicate.
(test mailbox-take-if
  (let* ((elements '(1 2 3 4))
         (mailbox (make-mailbox :initial-contents elements)))
    
    (test mailbox-take-if.non-removal
      (mailbox-reset mailbox)
      (multiple-value-bind (message false)
          (mailbox-take-if (constantly nil) mailbox)
        (is (eql message nil) "A message was returned when none should have been found")
        (is (eql false nil) "Predicate result was true when it should be false")
        (is (eql (mailbox-length mailbox) (length elements))
            "A message removed when it shouldn't")
        (is-true (every #'eql (mailbox-contents mailbox) elements)
                 "Non-removed messages got scrambled")))

    (test mailbox-take-if.removal    
      (mailbox-reset mailbox)
      (multiple-value-bind (message true)
          (mailbox-take-if #'(lambda (message)
                               (when (= message 3)
                                 42))
                           mailbox)
        (is (eql message 3) "Wrong message was removed")
        (is (eql true 42) "Predicate result wasn't returned properly")
        (is (eql (mailbox-length mailbox) (1- (length elements)))
            "No message was actually removed")
        (is-true (every #'eql (mailbox-contents mailbox) (remove 3 elements))
                 "Non-removed messages got scrambled")))))
