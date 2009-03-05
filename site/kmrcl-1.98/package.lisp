;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Purpose:       Package definition for kmrcl package
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Apr 2000
;;;;
;;;; $Id$
;;;;
;;;; This file, part of KMRCL, is Copyright (c) 2002-2006 by Kevin M. Rosenberg
;;;;
;;;; KMRCL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:cl-user)

(defpackage #:kmrcl
  (:nicknames #:kl)
  (:use #:cl)
  (:export
   #:ensure-integer
   #:mklist
   #:filter
   #:map-and-remove-nils
   #:appendnew
   #:memo-proc
   #:memoize
   #:defun-memo
   #:_f
   #:compose
   #:until
   #:while
   #:for

   ;; strings.lisp
   #:string-trim-whitespace
   #:string-left-trim-whitespace
   #:string-right-trim-whitespace
   #:mapappend
   #:mapcar-append-string
   #:mapcar2-append-string
   #:position-char
   #:position-not-char
   #:delimited-string-to-list
   #:string-delimited-string-to-list
   #:list-to-delimited-string
   #:prefixed-fixnum-string
   #:prefixed-integer-string
   #:integer-string
   #:fast-string-search
   #:string-substitute
   #:string-to-list-skip-delimiter
   #:string-starts-with
   #:count-string-char
   #:count-string-char-if
   #:hexchar
   #:charhex
   #:encode-uri-string
   #:decode-uri-string
   #:uri-query-to-alist
   #:non-alphanumericp
   #:random-string
   #:first-char
   #:last-char
   #:ensure-string
   #:string-right-trim-one-char
   #:string-strip-ending
   #:string-maybe-shorten
   #:string-elide
   #:shrink-vector
   #:collapse-whitespace
   #:string->list
   #:trim-non-alphanumeric
   #:binary-sequence-to-hex-string
   #:remove-char-string

   ;; io.lisp
   #:indent-spaces
   #:indent-html-spaces
   #:print-n-chars
   #:print-n-strings
   #:print-list
   #:print-rows
   #:write-fixnum
   #:file-subst
   #:stream-subst
   #:null-output-stream
   #:directory-tree
   #:write-utime-hms
   #:write-utime-hm
   #:write-utime-ymdhms
   #:write-utime-ymdhm
   #:write-utime-hms-stream
   #:write-utime-hm-stream
   #:write-utime-ymdhms-stream
   #:write-utime-ymdhm-stream
   #:with-utime-decoding
   #:with-utime-decoding-utc-offset
   #:is-dst
   #:year
   #:month
   #:day-of-month
   #:hour
   #:minute
   #:second
   #:daylight-p
   #:zone
   #:day-of-month
   #:day-of-week
   #:+datetime-number-strings+
   #:utc-offset
   #:copy-binary-stream

   ;; impl.lisp
   #:probe-directory
   #:cwd
   #:quit
   #:command-line-arguments
   #:copy-file
   #:run-shell-command

   ;; lists.lisp
   #:remove-from-tree-if
   #:find-tree
   #:with-each-file-line
   #:with-each-stream-line
   #:remove-keyword
   #:remove-keywords
   #:append-sublists
   #:alist-elem-p
   #:alistp
   #:get-alist
   #:update-alist
   #:alist-plist
   #:plist-alist
   #:update-plist
   #:get-plist
   #:flatten
   #:unique-slot-values

   ;; seq.lisp
   #:nsubseq

   ;; math.lisp
   #:ensure-integer
   #:histogram
   #:fixnum-width
   #:scaled-epsilon
   #:sinc
   #:numbers-within-percentage

   ;; macros.lisp
   #:time-iterations
   #:time-seconds
   #:in
   #:mean
   #:with-gensyms
   #:let-if
   #:let-when
   #:aif
   #:awhen
   #:awhile
   #:aand
   #:acond
   #:alambda
   #:it
   #:mac
   #:mv-bind
   #:deflex
   #:def-cached-vector
   #:def-cached-instance
   #:with-ignore-errors
   #:ppmx
   #:defconstant*
   #:defvar-unbound

   ;; files.lisp
   #:print-file-contents
   #:read-stream-to-string
   #:read-file-to-string
   #:read-file-to-usb8-array
   #:read-stream-to-strings
   #:read-file-to-strings

   ;; strings.lisp
   #:string-append
   #:count-string-words
   #:substitute-string-for-char
   #:string-trim-last-character
   #:nstring-trim-last-character
   #:string-hash
   #:is-string-empty
   #:is-char-whitespace
   #:not-whitespace-char
   #:is-string-whitespace
   #:string-invert
   #:escape-xml-string
   #:make-usb8-array
   #:usb8-array-to-string
   #:string-to-usb8-array
   #:substitute-chars-strings
   #:add-sql-quotes
   #:escape-backslashes
   #:concat-separated-strings
   #:print-separated-strings
   #:lex-string
   #:split-alphanumeric-string

   ;; strmatch.lisp
   #:score-multiword-match
   #:multiword-match

   ;; symbols.lisp
   #:ensure-keyword
   #:ensure-keyword-upcase
   #:ensure-keyword-default-case
   #:concat-symbol
   #:concat-symbol-pkg
   #:show
   #:show-variables
   #:show-functions

   ;; From attrib-class.lisp
   #:attributes-class
   #:slot-attribute
   #:slot-attributes

   #:generalized-equal

   ;; From buffered input

   #:make-fields-buffer
   #:read-buffered-fields

   ;; From datetime.lisp
   #:pretty-date-ut
   #:pretty-date
   #:date-string
   #:print-float-units
   #:print-seconds
   #:posix-time-to-utime
   #:utime-to-posix-time

   ;; From random.lisp
   #:seed-random-generator
   #:random-choice

   ;; From repl.lisp
   #:make-repl
   #:init/repl

   ;; From web-utils
   #:*base-url*
   #:base-url!
   #:make-url
   #:*standard-html-header*
   #:*standard-xhtml-header*
   #:*standard-xml-header*
   #:user-agent-ie-p
   #:decode-uri-query-string
   #:split-uri-query-string

   ;; From xml-utils
   #:sgml-header-stream
   #:xml-tag-contents
   #:positions-xml-tag-contents
   #:cdata-string
   #:write-cdata

   ;; From console
   #:*console-msgs*
   #:cmsg
   #:cmsg-c
   #:cmsg-add
   #:cmsg-remove
   #:fixme

   ;; byte-stream
   #:make-binary-array-output-stream
   #:get-output-stream-data
   #:dump-output-stream-data
   #:make-byte-array-input-stream

   ;; sockets.lisp
   #:make-active-socket
   #:close-active-socket

   ;; listener.lisp
   #:init/listener
   #:stop-all/listener
   #:listener

   ;; fformat.lisp
   #:fformat

   ;; os.lisp
   #:command-output
   #:run-shell-command-output-stream
   #:delete-directory-and-files
   #:file-size
   #:getpid

   ;; color.lisp
   #:rgb->hsv
   #:rgb255->hsv255
   #:hsv->rgb
   #:hsv255->rgb255
   #:hsv-equal
   #:hsv255-equal
   #:hsv-similar
   #:hsv255-similar
   #:hue-difference
   #:hue-difference-fixnum

   ;; signals.lisp
   #:set-signal-handler
   #:remove-signal-handler
   ))
