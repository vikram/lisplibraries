;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

;; Language utilities asd files

(defpackage #:langutils.system
  (:use #:cl #:asdf #:asdf-config))

(in-package #:langutils.system)

(defsystem-config #:langutils
    :description "Language utilities"
    :version "1.0"
    :author "Ian Eslick"
    :licence "MIT License"
    :depends-on #+think(:meta :cl-prevalence :port :utils)
                #-think(:meta :cl-prevalence :port :ise-utils :cl-ppcre)
    :components
    ((:module "src"
      :components ((:file "package")
		   (:file "reference")
		   (:file "tokens")
		   (:file "stopwords")
		   (:file "tokenize")
		   (:file "lexicon")
		   (:file "lemma")
		   (:file "porter")
		   (:file "contextual-rule-parser")
		   (:file "tagger-data")
		   (:file "tagger")
		   (:file "chunker-constants")
		   (:file "chunker")
		   (:file "concept")
		   (:file "init"))
      :serial t))
    :in-order-to ((load-op (compile-op :langutils)))
    :parameters ((:token-map "langutils::*default-token-map-file*")
		 (:lexicon "langutils::*default-lexicon-file*")
		 (:stems "langutils::*default-stems-file*")
		 (:lexical-rules "langutils::*default-lexical-rule-file*")
		 (:contextual-rules "langutils::*default-contextual-rule-file*")
		 (:stopwords "langutils::*default-stopwords-file*")
		 (:concise-stopwords "langutils::*default-concise-stopwords-file*"))
    :initialization "langutils::init-langutils")


