;; This software is Copyright (c) 2006, Steffen Siering.
;; <COMPANY> grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :cl-user)

;;;; * Package definition

(defpackage :cffi-util
  (:use :cl :cffi :mycl-util
	:trivial-gray-streams
	:flexi-streams)
  (:export
   ;;flag-system
   #:*current-flag-system*
   #:make-flag-system #:flag-system-parent
   #:find-flag-system
   #:delete-flag-system
   #:clear-flag-systems
   #:use-flagsystem
   #:def-flag-system
   #:with-flag-system
   #:direct-flag-value
   #:flag-value
   #:clear-flags
   #:concat-flags
   #:flags
   #:def-flag
   #:def-flag*
   #:has-flag
   #:clr-flag
   #:set-flag
   #:flag-list

   ;;helpers
   #:defcfun*
   #:defmethod*

   ;;cbuffer
   #:make-cbuffer
   #:make-cbuffer-from-pointer
   #:cbuffer-p
   #:cbuffer-data
   #:cbuffer-size
   #:cbuffer-length

   #:*use-cbuffer-pool*
   #:*cbuffer-pool-max-size*
   #:*cbuffer-pool-min-buffer-size*
   #:*cbuffer-pool-max-buffer-size*
   #:make-cbuffer-pool
   #:*cbuffer-pool*
   #:clear-cbuffer-pool
   #:alloc-cbuffer
   #:free-cbuffer
   
   #:cbuffer-resize
   #:cbuffer-eob-p
   #:cbuffer-ensure-size
   #:cbuffer-adjust-min-size
   #:with-cbuffer

   #:cbuffer-checkbounds
   #:cbuffer-byte
   #:make-cbuffer-input-stream
   #:make-cbuffer-output-stream
   #:stream-position
   #:stream-cbuffer

   ))
