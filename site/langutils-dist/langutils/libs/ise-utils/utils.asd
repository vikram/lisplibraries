;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; ASD File for Think Utilities Module

(defpackage #:utils.system
  (:use #:cl #:asdf))

(in-package #:utils.system)

;; Kevin Rosenberg mop hacks
;;#+(or allegro cmu lispworks sbcl scl openmcl)
;;(pushnew :kmr-mop cl:*features*)

(defsystem #:ise-utils
    :description "Think Utilities: A set of helpful utilities used by the Think system"
    :version "1.0"
    :author "Ian Eslick <eslick@media.mit.edu>"
    :licence "Public Domain"    
    :components ((:file "package")
		 (:file "system" :depends-on ("package"))         ;; global system macros
		 (:file "lists" :depends-on ("system"))           ;; list related utilities, tree walking, searching, etc
		 (:file "macros" :depends-on ("system"))          ;; Useful macros, such as aif, awhen, etc.
		 (:file "iteration" :depends-on ("system"))    ;; iteration related utilities
		 (:file "conditionals" :depends-on ("lists"))     ;; anaphoric macros, 
		 (:file "map" :depends-on ("iteration"))          ;; map related utilities
		 (:file "iteration2" :depends-on ("map"))    ;; iteration related utilities
		 (:file "shorthand" :depends-on ("map" "conditionals")) ;; abbreviations
		 (:file "functions" :depends-on ("shorthand"))    ;; function utilities
		 (:file "math" :depends-on ("functions"))         ;; math tools
		 (:file "setf" :depends-on ("functions"))         ;; shortcuts for dealing with places & setf macros
;;		 (:file "clos" :depends-on ("functions"))         ;; utilities for clos
		 (:file "hashutil" :depends-on ("shorthand"))     ;; a wrapper around the hash function
		 (:file "file" :depends-on ("shorthand"))         ;; file utilities
		 (:file "bitvector" :depends-on ("shorthand"))    ;; bitvector utilites (empty for now)
		 (:file "random")                                 ;; Random generation and manipulation
		 (:file "allegro")
		 (:file "split-sequence")
		 (:file "ifstar" :depends-on ("package"))
		 ;; Larger scale utilities, but common enough to be included here
		 (:file "threads")
		 (:file "tokenizer" :depends-on ("lists"))    ;; a simple configurable tokenizer
		 (:file "match" :depends-on ("shorthand"))    ;; structure matching with vars
		 (:file "prof"  :depends-on ("shorthand"))    ;; profiling support
;;		 (:file "interactive" :depends-on ("functions" "clos")) ;; repl aids
		 (:file "log") ;; generic logging facility for production and debugging use
		 (:file "plotutils") ;; additions to cllibs gnuplot interface
		 (:file "time" :depends-on ("shorthand"))     ;; Utility set for parsing time strings
;;		 (:file "monitor") ;; a perf monitoring system; package 'monitor'

		 ;; Move these to a data-structures dsutils or something
		 (:file "queue" :depends-on ("shorthand"))
		 (:file "cache" :depends-on ("queue" "hashutil"))
		 (:file "collections"  :depends-on ("shorthand" "clos" "hashutil")) ;; collection ds
		 (:file "wordseq"  :depends-on ("shorthand"))    ;; manipulate sequences of dictionary words
		 (:file "vechash"  :depends-on ("shorthand"))    ;; a fast hash table for vector keys
;;		 (:file "deftable" :depends-on ("shorthand"))    ;; the deftable package from sun
;;		 (:file "sbtree" :depends-on ("shorthand"))      

;;		 (:file "imports" :depends-on ("vechash")))
		 )
    :serial t
    :in-order-to ((load-op (compile-op :utils)))
    :depends-on ()) ;; (:cl-ppcre :kmrcl :cllib :port))