(defpackage #:clisp-sqlite-system (:use #:cl #:asdf))
(in-package :clisp-sqlite-system)

(defsystem :clisp-sqlite
  :version "1.0"
  :components ((:file "sqlite")))
