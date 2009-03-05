;;;; Copyright (c) 2004-2005, Dirk H.P. Gerrits
;;;; All rights reserved.
;;;;
;;;; This software is licensed under the terms of the BSD license, stated in the
;;;; file named LICENSE.
;;;;
;;;;----------------------------------------------------------------------------
;;;;
;;;; This file implements the pattern matcher for messages.
;;;;
;;;; TODO: the current "matcher" is far too simplistic.
;;;; 
;;;;----------------------------------------------------------------------------

(in-package :erlisp)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *default-pattern-matcher* 'cond-matcher
    "The default pattern-matcher-generating function.

See the documentation of the MATCH-WITH-MATCHER macro for information on the
calling convention of this function.")

  (defmacro default-pattern-matcher ()
    "Return the default pattern-matcher-generating function.

See the documentation of the MATCH-WITH-MATCHER macro for information on the
calling convention of this function."
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       *default-pattern-matcher*))

  (defmacro set-default-pattern-matcher (new-default)
    "Set the default pattern-matcher-generating function to NEW-DEFAULT.

See the documentation of the MATCH-WITH-MATCHER macro for information on the
calling convention of this function."
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (setq *default-pattern-matcher* ,new-default)))

  (defun cond-matcher (object-var patterns bodies &optional (var 'it))
    "Generates a pattern \"matcher\" based on COND.
The object being matched is bound to a variable named VAR (which defaults to IT)
for convenience."
    `(let ((,var ,object-var))
       (declare (ignorable ,var))
       (cond ,@(loop for pattern in patterns
                     for body in bodies
                     collect `(,pattern ,@body)))))

  (defun case-matcher (object-var patterns bodies &optional (var 'it))
    "Generates a pattern \"matcher\" based on CASE.
The object being matched is bound to a variable named VAR (which defaults to IT)
for convenience."
    `(let ((,var ,object-var))
       (declare (ignorable ,var))       
       (case ,var
         ,@(loop for pattern in patterns
                 for body in bodies
                 collect `(,pattern ,@body))))))

(defmacro match-with-matcher ((matcher &rest extra-args) object-form
                              &body clauses)
  "Evaluate OBJECT-FORM and match the value against CLAUSES as specified by MATCHER and EXTRA-ARGS.

CLAUSES is separated into a list of PATTERNS and a list of associated BODIES.  
MATCHER must be the name of a function defined in the macroexpansion-time
environment.  It will be called at macroexpansion-time with a symbol OBJECT-VAR,
the two lists PATTERNS and BODIES of equal length, and all the EXTRA-ARGS.  The
return value should be code that matches the value of the variable named by
OBJECT-VAR sequentially against each of the PATTERNS.  If none of the PATTERNS
matches, the code's evaluation should returns NIL.  Otherwise, the BODY
corresponding to the first matching PATTERN is evaluated as by PROGN and the
resulting value is returned.  The actual manner in which matching is done, and
how this is influenced by the EXTRA-ARGS, is up to the function.

For example, when called with MESSAGE, (<pat1> <pat2>), ((<body1>) (<body2>)),
the return value for a trivial MATCHER based on CASE might be:

 (let ((it MESSAGE))
   (case it
     (<pat1> #'(lambda () <body1>))
     (<pat2> #'(lambda () <body2>))))
"
  (multiple-value-bind (object-var patterns bodies)
      (loop for (pattern . body) in clauses
            collect pattern into patterns
            collect body into bodies
            finally (return (values (gensym) patterns bodies)))
    `(let ((,object-var ,object-form))
       ,(apply matcher object-var patterns bodies extra-args))))

(defmacro match (object-form &body clauses)
  "Evaluate OBJECT-FORM and match the value against CLAUSES with the default pattern matcher."
  `(match-with-matcher (,*default-pattern-matcher*)
       ,object-form
     ,@clauses))