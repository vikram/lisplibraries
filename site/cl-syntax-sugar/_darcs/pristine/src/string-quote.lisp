;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-syntax-sugar)

(define-syntax string-quote (start-character end-character &key transformer)
  "A simple string quote that unconditionally reads all characters until END-CHARACTER into a string."
  (bind ((reader (make-string-quote-reader end-character transformer)))
    (set-macro-character start-character reader t *readtable*)))

(defun make-string-quote-reader (end-character transformer)
  (unless transformer
    (setf transformer #'identity))
  (named-lambda string-reader (stream &optional char)
    (declare (ignore char))
    (bind ((*toplevel-readtable* (or *toplevel-readtable* *readtable*)))
      (loop
         :with result = (make-array 8 :element-type 'character :adjustable t :fill-pointer 0)
         :with base-char? = t
         :for char = (read-char stream t nil t)
         :until (char= char end-character)
         :do (progn
               (setf base-char? (and base-char?
                                     (typep char 'base-char)))
               (vector-push-extend char result))
         :finally (return (funcall transformer (if base-char?
                                                   (coerce result 'simple-base-string)
                                                   result)))))))
