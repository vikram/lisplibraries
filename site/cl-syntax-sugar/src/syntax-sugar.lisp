;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-syntax-sugar)

(defvar *toplevel-readtable* nil
  "This is bound to the toplevel *readtable* by all the readers in cl-syntax-sugar. This is useful for example to restore the original readtable-case at random point in the nested readers.")

(defmacro define-syntax (&whole whole name args &body body)
  (bind (((name &key readtime-wrapper-result-transformer) (ensure-list name))
         (enabler-name (format-symbol *package* "ENABLE-~A-SYNTAX" name))
         (enabler-function-name (format-symbol *package* "SET-~A-SYNTAX-IN-READTABLE" name))
         (readtime-wrapper-name (format-symbol *package* "WITH-~A-SYNTAX" name))
         ((:values body declarations documentation) (parse-body body :documentation t :whole whole)))
    `(progn
       (defmacro ,enabler-name ,(lambda-list-to-lambda-list-with-quoted-defaults args)
         ,documentation
         `(eval-when (:compile-toplevel :execute)
            (setf *readtable* (copy-readtable *readtable*))
            (,',enabler-function-name ,,@(lambda-list-to-funcall-list args))
            (values)))
       (defun ,enabler-function-name ,args
         ,@declarations
         ,@body
         (values))
       (defun ,readtime-wrapper-name ,args
         (named-lambda ,readtime-wrapper-name (handler)
           (,enabler-function-name ,@(lambda-list-to-funcall-list args))
           (bind ((result (funcall handler))
                  (result-transformer ,readtime-wrapper-result-transformer))
             (if result-transformer
                 (funcall result-transformer result)
                 (if (rest result)
                     `(progn
                        ,@result)
                     (first result))))))
       (export '(,enabler-name ,enabler-function-name ,readtime-wrapper-name)))))

(defmacro with-local-readtable (&body body)
  "Rebind a copy of *readtable*."
  `(bind ((*readtable* (copy-readtable *readtable*)))
     ,@body))

(defun list-readers (&optional (*readtable* *readtable*))
  "A very slow utility for REPL use."
  (loop
     :with result = '()
     :for code :from 0 :below char-code-limit
     :for char = (code-char code)
     :do (bind (((:values fun non-terminating?) (get-macro-character char)))
           (when (or fun
                     non-terminating?)
             (push (list char
                         fun non-terminating?
                         (when (ignore-errors
                                 (get-dispatch-macro-character char #\a)
                                 t)
                           (loop
                              :for code :from 0 :below char-code-limit
                              :for sub = (code-char code)
                              :for fun = (get-dispatch-macro-character char sub)
                              :when fun
                              :collect (list sub fun)))) result)))
     :finally (return result)))
