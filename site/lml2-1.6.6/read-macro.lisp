;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          read-macro.lisp
;;;; Purpose:       Lisp Markup Language functions
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Aug 2002
;;;;
;;;; $Id$
;;;;
;;;; This file, part of LML2, is Copyright (c) 2000-2003 by Kevin Rosenberg.
;;;; Rights of modification and redistribution are in the LICENSE file.
;;;;
;;;; *************************************************************************

(in-package #:lml2)

(defun new-string ()
  (make-array 1024 :fill-pointer 0 :adjustable t :element-type 'character))

(set-macro-character #\[
  #'(lambda (stream char)
      (declare (ignore char))
      (let ((forms '())
            (curr-string (new-string))
            (paren-level 0)
            (got-comma nil))
        (declare (type fixnum paren-level))
        (do ((ch (read-char stream t nil t) (read-char stream t nil t)))
            ((eql ch #\]))
          (if got-comma
              (if (eql ch #\()
                  ;; Starting top-level ,(
                  (progn
                    #+cmu
                    (setf curr-string (coerce curr-string `(simple-array character (*))))

                    (push `(lml2-princ ,curr-string) forms)
                    (setq curr-string (new-string))
                    (setq got-comma nil)
                    (vector-push #\( curr-string)
                    (do ((ch (read-char stream t nil t) (read-char stream t nil t)))
                        ((and (eql ch #\)) (zerop paren-level)))
                      (when (eql ch #\])
                        (format *trace-output* "Syntax error reading #\]")
                        (return nil))
                      (case ch
                        (#\(
                         (incf paren-level))
                        (#\)
                         (decf paren-level)))
                      (vector-push-extend ch curr-string))
                    (vector-push-extend #\) curr-string)
                    (let ((eval-string (read-from-string curr-string))
                          (res (gensym)))
                      (push
                       `(let ((,res ,eval-string))
                          (when ,res
                            (lml2-princ ,res)))
                       forms))
                    (setq curr-string (new-string)))
                ;; read comma, then non #\( char
                (progn
                  (unless (eql ch #\,)
                    (setq got-comma nil))
                  (vector-push-extend #\, curr-string) ;; push previous command
                  (vector-push-extend ch curr-string)))
            ;; previous character is not a comma
            (if (eql ch #\,)
                (setq got-comma t)
              (progn
                (setq got-comma nil)
                (vector-push-extend ch curr-string)))))

        #+cmu
        (setf curr-string (coerce curr-string `(simple-array character (*))))

        (push `(lml2-princ ,curr-string) forms)
        `(progn ,@(nreverse forms)))))
