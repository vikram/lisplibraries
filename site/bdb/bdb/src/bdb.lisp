;; This software is Copyright (c) 2006, Steffen Siering.
;; <COMPANY> grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :bdb)

;;;; * Version Infos

(defcfun "db_strerr" :string
  (error :int))

(defcfun "bdb_version_major" :int)

(defcfun "bdb_verions_minor" :int)

(defcfun "bdb_version_patch" :int)

(defcfun "bdb_version_string" :string)

(defcfun+ ("bdb_version" bdb-version) :string
  (major :out :int)
  (minor :out :int)
  (patch :out :int))
