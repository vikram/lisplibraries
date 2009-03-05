;;; -*- Lisp -*-

(in-package #:cl-user)

(defpackage #:trivial-html-parser-system
  (:use #:cl
        #:asdf))

(in-package #:trivial-html-parser-system)

(defclass closure-source-file (cl-source-file) ())

#+sbcl
(defmethod perform :around ((o compile-op) (s closure-source-file))
  ;; shut up already.  Correctness first.
  (handler-bind ((sb-ext:compiler-note #'muffle-warning))
    (call-next-method)))

;;; Convenience feature: will stop it from breaking into the debugger
;;; under sbcl for full WARNINGs (better to fix the warnings :-).
#+sbcl
(defmethod perform :around ((o compile-op) s)
  (setf (operation-on-failure o) :warn)
  (call-next-method o s))

(defsystem #:trivial-html-parser
  :version "0.1"
  :default-component-class closure-source-file
  :components ((:file "packages")
               (:module :glisp
                        :components ((:file "util")
                                     (:file :dependent
                                            :pathname
            #+CLISP                             "dep-clisp"
            #+(AND :CMU (NOT SCL))              "dep-cmucl"
            #+sbcl                              "dep-sbcl"
            #+SCL                               "dep-scl"
            #+ALLEGRO                           "dep-acl"
            #+GCL                               "dep-gcl"
            #+OPENMCL                           "dep-openmcl"
            #-(OR sbcl CLISP CMU ALLEGRO GCL OPENMCL) #.(error "Configure!")))
                        :serial t)
               (:file "element")
               (:file "mime")
               (:file "clex")
               (:file "lalr")
               (:module :parse
                        :components ((:file "package")
                                     (:file "pt")
                                     (:file "sgml-dtd")
                                     (:file "sgml-parse"))
                        :serial t)
               (:file "html-parser"))
  :serial t
  :depends-on (#:flexi-streams #:cxml))
