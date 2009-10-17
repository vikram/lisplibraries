;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :cl-user)

;;;; CLPython Package definitions

#+allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (eq excl::*current-case-mode* :case-sensitive-lower)
    (pushnew :clpython-allegro-modern-mode *features*)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun get-external-symbols (pkg)
    (loop for s being each external-symbol in (find-package pkg) collect s))
  
  (defun cascade-external-symbols (pkg &optional used-pkg-list)
    (let (to-export)
      (dolist (p (or used-pkg-list (package-use-list pkg)))
        (unless (eq p (find-package :common-lisp))
          (do-external-symbols (s p)
            ;; Test whether not shadowed (like by symbol in the Common Lisp package) 
            (when (eq s (find-symbol (symbol-name s) pkg))
              (push s to-export)))))
      (export (append to-export (get-external-symbols pkg)) pkg))))


;; The exported symbols are given as #:symbols if case is irrelevant, 
;; and "strings" if case matters.


;;; CLPYTHON.AST - Abstract syntax tree

(defpackage :clpython.ast.reserved
  (:documentation "Reserved words in the grammar")
  ;; A few of these (e.g. `as') are not actually reserved words in CPython yet
  ;; (for backward compatilibity reasons), but will be in a future version.
  (:use )
  (:export "and" "as" "assert" "break" "class" "continue" "def" "del" "elif" "else"
	   "except" "exec" "finally" "for" "from" "global" "if" "import" "in" "is"
	   "lambda" "not" "or" "pass" "print" "raise" "return" "try" "while" "with"
           "yield"
           "is not" "not in"))

(defpackage :clpython.ast.operator
  (:documentation "Unary and binary operators")
  (:use )
  (:export "<" "<=" ">" ">=" "!=" "=="
	   "|" "^" "&" "<<" ">>" "+" "-" "*" "/" "%" "//" "~" "**"
	   "|=" "^=" "&=" "<<=" ">>=" "+=" "-=" "*=" "/=" "*=" "/=" "%=" "//=" "**=")
  (:intern "/t/" "<divmod>" ;; not really operators in the grammar, but used internally.
	   ))

(defpackage :clpython.ast.punctuation
  (:use )
  (:export "@" ":" "[" "]" "{" "}" "(" ")" "=" "." "," "`" ";" "..."))

(defpackage :clpython.ast.token
  (:use )
  (:export "newline" "indent" "dedent" "identifier" "number" "string"))

(defpackage :clpython.ast.node
  (:documentation "Statement and expression nodes")
  (:use )
  (:export "assert-stmt" "assign-stmt" "augassign-stmt" "break-stmt" "classdef-stmt"
	   "continue-stmt" "del-stmt" "exec-stmt" "for-in-stmt" "funcdef-stmt"
	   "global-stmt" "if-stmt" "import-stmt" "import-from-stmt" "module-stmt"
	   "pass-stmt" "print-stmt" "raise-stmt" "return-stmt" "suite-stmt"
	   "try-except-stmt" "try-finally-stmt" "while-stmt" "with-stmt" "yield-stmt"
	   
	   "attributeref-expr" "backticks-expr" "binary-expr" "binary-lazy-expr"
           "bracketed-expr" "call-expr" "comparison-expr" "dict-expr" "generator-expr"
	   "identifier-expr" "if-expr" "lambda-expr" "listcompr-expr" "list-expr"
           "slice-expr" "subscription-expr" "tuple-expr" "unary-expr" "yield-expr"
	   
	   "for-in-clause" "if-clause" ;; not really nodes
           )
  (:intern "clpython-stmt" ;; internal state
	   ))

(defpackage :clpython.ast.node-p
  (:documentation "Statement and expression nodes")
  (:use )
  (:export "assert-stmt-p" "assign-stmt-p" "augassign-stmt-p" "break-stmt-p" "classdef-stmt-p"
	   "continue-stmt-p" "del-stmt-p" "exec-stmt-p" "for-in-p-stmt" "funcdef-stmt-p"
	   "global-stmt-p" "if-stmt-p" "import-stmt-p" "import-from-p-stmt" "module-stmt-p"
	   "pass-stmt-p" "print-stmt-p" "raise-stmt-p" "return-stmt-p" "suite-stmt-p"
	   "try-except-p-stmt" "try-finally-p-stmt" "while-stmt-p" "yield-stmt-p"
           
           "attributeref-expr-p" "backticks-expr-p" "binary-expr-p" "binary-lazy-p-expr"
	   "bracketed-expr-p" "call-expr-p" "comparison-expr-p" "dict-expr-p" "generator-expr-p"
	   "identifier-expr-p" "lambda-expr-p" "listcompr-expr-p" "list-expr-p" "slice-expr-p"
	   "subscription-expr-p" "tuple-expr-p" "unary-expr-p"
           ))

(defpackage :clpython.ast
  (:documentation "Python abstract syntax tree representation")
  (:use :clpython.ast.reserved :clpython.ast.node :clpython.ast.punctuation
	:clpython.ast.operator :clpython.ast.token :clpython.ast.node-p)
  (:import-from :clpython.ast.operator "/t/" "<divmod>")
  (:import-from :clpython.ast.reserved "is not" "not in")
  (:import-from :clpython.ast.node     "clpython-stmt"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (cascade-external-symbols :clpython.ast))


;;; CLPYTHON.USER - Identifiers
;;;
;;; All identifiers, like function, method and variable names, are in the
;;; :clpython.user package.

(defpackage :clpython.user.builtin.function
  (:use )
  (:export "__import__" "abs" "apply" "callable" "chr" "cmp" "coerce" "compile"
	   "delattr" "dir" "divmod" "eval" "execfile" "filter" "getattr" "globals"
	   "hasattr" "hash" "hex" "id" "input" "intern" "isinstance" "issubclass"
	   "iter" "len" "locals" "map" "max" "min" "oct" "open" "ord" "pow" "range"
	   "raw_input" "reduce" "reload" "repr" "round" "setattr" "sorted" "sum"
	   "unichr" "vars" "zip"))

(defpackage :clpython.user.builtin.type.exception
  (:use )
  (:export "ArithmeticError" "AssertionError" "AttributeError" "DeprecationWarning"
	   "EOFError" "EnvironmentError" "Exception" "FloatingPointError"
	   "FutureWarning" "IOError" "ImportError" "ImportWarning" "IndentationError"
           "IndexError" "KeyError" "KeyboardInterrupt" "LookupError" "MemoryError"
           "NameError" "NotImplementedError" "OSError" "OverflowError" "OverflowWarning"
	   "PendingDeprecationWarning" "ReferenceError" "RuntimeError"
	   "RuntimeWarning" "StandardError" "StopIteration" "SyntaxError"
	   "SyntaxWarning" "SystemError" "SystemExit" "TabError" "TypeError"
	   "UnboundLocalError" "UnexpectedEofError" "UnicodeDecodeError"
	   "UnicodeEncodeError" "UnicodeError" "UnicodeTranslateError" "UserWarning"
	   "VMSError" "ValueError" "Warning" "WindowsError" "ZeroDivisionError"))

(defpackage :clpython.user.builtin.type
  (:use :clpython.user.builtin.type.exception)
  (:export "basestring" "bool" "buffer" "classmethod" "complex" "dict" "enumerate" "file"
	   "float" "frozenset" "int" "list" "long" "number" "object" "property" "set"
           "slice" "staticmethod" "str" "super" "tuple" "type" "unicode" "xrange"))

(defpackage :clpython.user.builtin.value
  (:use )
  (:export "None" "Ellipsis" "True" "False" "NotImplemented"))

(defpackage :clpython.user.builtin
  (:use :clpython.user.builtin.function :clpython.user.builtin.type
	:clpython.user.builtin.value ))

(defpackage :clpython.user
  (:documentation "Identifiers")
  (:use :clpython.user.builtin)
  (:export "__getitem__" "__delitem__" "__setitem__" "__new__" "__init__"
	   "__dict__" "__get__" "__del__" "__set__" "__name__" "__all__"
	   "__getattribute__" "__getattr__" "__class__" "__delattr__"
	   "__setattr__" "__call__" "__nonzero__" "__hash__" "__iter__"
	   ;; binary
	   "__add__" "__radd__" "__iadd__"  "__sub__" "__rsub__" "__isub__"
	   "__mul__" "__rmul__" "__imul__"  "__truediv__" "__rtruediv__" "__itruediv__"
	   "__mod__" "__rmod__" "__imod__"  "__floordiv__" "__rfloordiv__" "__ifloordiv__"
	   "__div__" "__rdiv__" "__idiv__"  "__lshift__" "__rlshift__" "__irshift__"
	   "__and__" "__rand__" "__iand__"  "__rshift__" "__rrshift__" "__irshift__"
	   "__or__"  "__ror__"  "__ior__"   "__divmod__" "__rdivmod__"
	   "__xor__" "__rxor__" "__ixor__"  "__pow__" "__rpow__" "__ipow__"
	   ;; unary
	   "__invert__" "__pos__" "__neg__" "__contains__" "__cmp__" "__abs__" "__len__"
           "__float__"
	   ;; comparison
	   "__eq__" "__lt__" "__gt__" "__cmp__" 
	   ;; representation
	   "__repr__" "__str__" "__hex__" "__oct__"
           ;; iterator
           "next"
           ;; with-stmt
           "__enter__" "__exit__"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (cascade-external-symbols :clpython.user.builtin.type))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (cascade-external-symbols :clpython.user.builtin))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (cascade-external-symbols :clpython.user))

;;; CLPYTHON.PACKAGE - Package and Readtables

(defpackage :clpython.package
  (:documentation "Package, readtables, ast/user symbol pretty printer")
  (:use :common-lisp)
  (:export #:in-syntax #:*ast-readtable* #:*user-readtable* #:*ast-user-readtable*
           #:with-ast-user-readtable #:with-ast-user-pprinter
           #:setup-omnivore-readmacro
           #:with-auto-mode-recompile #:whereas #:sans #:named-function #:slurp-file
           #:with-stack-list #:without-redefinition-warnings #:defconstant-once
           #:+max-char-code+ #:char-code-type #:ltv-find-class #:make-weak-key-hash-table
           #:schedule-finalization #:unschedule-finalization
           #:register-feature #:with-gensyms))

;;; CLPYTHON.PARSER - Parser and Lexer

(defpackage :clpython.parser
  (:documentation "Parser and lexer for Python code")
  (:use :common-lisp :clpython.package)
  (:export #:parse #:ast-complete-p ;; parser
           #:*python-form->source-location* #:*module->source-positions*
                      
           #:match-p #:with-matching #:with-perhaps-matching ;; AST pattern matcher
           
           #:walk-py-ast #:with-py-ast ;; code walker
	   #:+normal-target+ #:+delete-target+ #:+augassign-target+ #:+no-target+
	   #:+normal-value+ #:+augassign-value+ #:+no-value+

	   #:py-pprint #:*py-pprint-dispatch* ;; pretty printer
	   ))

;;; CLPYTHON.MODULE - Modules

(defpackage :clpython.module
  (:documentation "Aggregation package for Python modules")
  (:use ))

;;; CLPYTHON - The main package
;;;
;;; This package does not :use other packages, because many symbols conflict
;;; with those of the Common-Lisp package. Reader macros are used instead.

(defpackage :clpython
  (:documentation "CLPython: An implementation of Python in Common Lisp.")
  (:use :common-lisp :clpython.package :clpython.parser)
  (:export #:raise-syntax-error *raise-syntax-error-hook*
           
           #:py-val->string #:py-str-string #:py-repr #:py-bool #:make-module
	   #:*the-none* #:*the-true* #:*the-false* #:*the-ellipsis* #:*the-notimplemented*
	   #:*the-empty-tuple* #:make-tuple-from-list #:create-python-module
	   #:*py-modules* #:dyn-globals #:py-call #:py-class-of #:py-raise #:bind-val
	   #:py-repr-string #:py-attr #:py-attr*
	   #:run #:exception-args #:*exceptions-loaded*
           #:def-py-method #:py-iterate->lisp-list #:py-raise
           #:+the-true+ #:+the-false+ #:object #:py-type

	   ;; compiler
	   #:+standard-module-globals+
	   #:*warn-unused-function-vars* #:*warn-bogus-global-declarations*
           #:with-line-numbers #:*runtime-line-number-hook*

	   ;; module status
	   #:impl-status #:set-impl-status
	   
	   ;; utils
	   ))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (cascade-external-symbols :clpython))
 
(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(clpython:raise-syntax-error clpython:*raise-syntax-error-hook*)
          :clpython.parser))

;;; There are small references from CLPython to the REPL

(defpackage :clpython.app.repl
  (:documentation "Python read-eval-print loop")
  (:use :common-lisp :clpython :clpython.parser)
  (:export #:repl #:*repl-compile* #:*repl-prof*
           #:return-python-toplevel #:*repl-module-globals*)
  (:import-from :clpython #:with-matching))

;; Ensure that CLPython starts with repl's symbol, for code that uses :clpython
;; and has the symbol 'repl somewhere, and export it.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (import 'clpython.app.repl:repl :clpython)
  (export (cons 'clpython.app.repl:repl (get-external-symbols :clpython)) :clpython))

  