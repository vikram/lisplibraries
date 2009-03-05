;;;
;;; This file contains property of Mu Aps.
;;; Copyright (c) 2005.  All rights reserved.
;;;

(in-package :muproc-test)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defparameter *debug-level* 10
    "Integer indicating level of debug output, the higher the number
the more debug output."))

(defmacro dbgformat (level &rest args)
  "Log `args' with `muproc-log-errorstream' iff `*muproc-dedbgformat-level*'
      is not NIL and `level' >= `*muproc-dedbgformat-level*'."
  `(when (and *debug-level* (>= *debug-level* ,level))
    (muproc-log-errorstream (format nil "MUPROC-TEST(~a): ~a" ,level 
			     (format nil ,@args)))))
