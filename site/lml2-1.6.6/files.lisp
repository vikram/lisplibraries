;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          files.lisp
;;;; Purpose:       File and directory functions for LML
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Aug 2002
;;;;
;;;; This file, part of LML2, is Copyright (c) 2000-2003 by Kevin Rosenberg.
;;;; Rights of modification and redistribution are in the LICENSE file.
;;;;
;;;; *************************************************************************

(in-package #:lml2)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *output-dir* nil)
  (defvar *sources-dir* nil)
  )

(defun lml-file-name (f &optional (type :source))
  (when (and (consp f) (eql (car f) 'cl:quote))
    (setq f (cadr f)))
  (when (symbolp f)
    (setq f (string-downcase (symbol-name f))))
  (when (stringp f)
    (unless (position #\. f)
      (setq f (concatenate 'string f ".html"))))
  (if (or (and (eq type :source) *sources-dir*)
          (and (eq type :output) *output-dir*))
      (merge-pathnames
       (make-pathname :name (pathname-name f)
                      :type (pathname-type f)
                      :directory (pathname-directory f))
       (ecase type
         (:source *sources-dir*)
         (:output *output-dir*)))
      (if (stringp f)
          (parse-namestring f)
          f)))

(defmacro with-dir ((output &key sources) &body body)
  (let ((output-dir (gensym))
        (sources-dir (gensym)))
  `(let ((,output-dir ,output)
         (,sources-dir ,sources))
    (when (stringp ,output-dir)
      (setq ,output-dir (parse-namestring ,output-dir)))
    (when (stringp ,sources-dir)
      (setq ,sources-dir (parse-namestring ,sources-dir)))
    (unless ,sources-dir
      (setq ,sources-dir ,output-dir))
    (let ((*output-dir* ,output-dir)
          (*sources-dir* ,sources-dir))
      ,@body))))

(defun lml-load-path (file &key optional)
  (if (probe-file file)
      (with-open-file (in file :direction :input)
        (do ((form (read in nil 'eof) (read in nil 'eof)))
            ((eq form 'eof))
          (eval form)))
    (unless optional
      (format *trace-output* "Warning: unable to load LML file ~S" file))))

(defun process-dir (dir &key sources)
  (with-dir (dir :sources sources)
    (let ((lml-files (directory
                      (make-pathname :defaults *sources-dir*
                                     :name :wild
                                     :type "lml"))))
      (dolist (file lml-files)
        (format *trace-output* "~&; Processing ~A~%" file)
        (lml-load-path file)))))

(defun lml-load (file &key optional)
  (lml-load-path (eval `(lml-file-name ,file :source)) :optional optional))

(defun insert-file (file)
  (print-file-contents file *html-stream*))
