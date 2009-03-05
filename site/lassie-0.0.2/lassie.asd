;;;; -*- mode: Lisp -*-

(cl:defpackage #:lassie.system
  (:use #:cl #:asdf))

(cl:in-package #:lassie.system)

(defsystem #:lassie
  :name "Lassie"
  :description "Library for Latent Semantic Indexing."
  :author "Gabor Melis"
  :version "0.0.1"
  :licence "MIT"
  :components ((:file "package")
               (:file "indexer")
               (:file "normalizer")
               (:file "mapper")
               (:file "assemble")
               (:file "lsa")
               (:file "lsa-extra"))
  :serial t
  :depends-on (fsvd))
