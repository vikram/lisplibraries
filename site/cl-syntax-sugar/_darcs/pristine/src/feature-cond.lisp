;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-syntax-sugar)

(define-syntax feature-cond (&key (start-character #\*)
                                  (dispatch-character #\#)
                                  end-character
                                  readtable-case)
  (when (and dispatch-character end-character)
    (error "You can not install on both a dispatch character and an end character"))
  (bind ((reader (make-feature-cond-reader end-character readtable-case)))
    (cond
      ((and dispatch-character
            start-character)
       (unless (get-macro-character dispatch-character)
         (make-dispatch-macro-character dispatch-character))
       (set-dispatch-macro-character dispatch-character start-character reader *readtable*))
      ((not (null start-character))
       (set-macro-character start-character reader t *readtable*)))))

(defun make-feature-cond-reader (end-character readtable-case)
  (labels ((feature-cond-reader (stream char &optional dispatched-char)
             (declare (ignore char dispatched-char))
             (bind ((*toplevel-readtable* (or *toplevel-readtable* *readtable*)))
               (with-local-readtable
                 (when readtable-case
                   (setf (readtable-case *readtable*) readtable-case))
                 (bind ((form (read-body stream)))
                   (unless (consp form)
                     (simple-reader-error "Feature-cond expects a list instead of ~S" form))
                   (loop :for (condition . body) :in form :do
                      (when (bind (#+sbcl (sb-ext:*evaluator-mode* :interpret))
                              (eval (process-feature-cond-condition condition)))
                        (return (cond
                                  ((not (consp body))
                                   body)
                                  ((or (null body)
                                       (length= 1 body))
                                   (first body))
                                  (t `(progn ,@body))))))))))
           (read-body (stream)
             (if end-character
                 (with-local-readtable
                   (set-syntax-from-char end-character #\) *readtable*)
                   (read-delimited-list end-character stream t))
                 (read stream t nil t))))
    #'feature-cond-reader))

(defun process-feature-cond-condition (input-form)
  (labels ((recurse (form)
             (cond
               ((consp form)
                (if (member (first form) '(and or not))
                    `(,(first form) ,@(mapcar #'recurse (rest form)))
                    form))
               ((keywordp form)
                `(find ,form *features*))
               ((member form '(t otherwise))
                t)
               ((and (not (null form))
                     (symbolp form))
                (error "To be less confusing feature-cond does not read symbols automatically into the KEYWORD package, please prefix ~S" form)
                #+nil ; this would interpret "foo" as ":foo" but let's just be less confusing than the default CL readers...
                `(find ,(intern (symbol-name form) :keyword) *features*))
               (t
                (error "Don't know how to process feature-cond form ~S" input-form)))))
    (recurse input-form)))
