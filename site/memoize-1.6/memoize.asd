;; -*- Lisp -*-

(defpackage #:memoize-system
  (:use #:common-lisp #:asdf))

(in-package #:memoize-system)

(defsystem memoize
  :author "Tim Bradshaw"
  :licence "may be used for any purpose whatsoever by anyone, no warranty whatsoever."
  :version "1.6"
  :components ((:file "memoize")))