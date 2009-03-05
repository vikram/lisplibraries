;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Purpose:       Package file for Lisp Markup Language 2
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  June 2003
;;;;
;;;; $Id$
;;;;
;;;; This file, part of LML2, is Copyright (c) 2000-2003 by Kevin Rosenberg.
;;;; Rights of modification and redistribution are in the LICENSE file.
;;;;
;;;; *************************************************************************

(in-package #:cl-user)

(defpackage #:lisp-markup-language-2
  (:use #:common-lisp #:kmrcl)
  (:nicknames #:lml2)
  (:export

   ;; data.lisp
   #:*html-stream*

   ;; base.lisp
   #:html-file-page
   #:dtd-prologue
   #:lml-format
   #:lml-print
   #:lml-princ
   #:lml-write-char
   #:lml-write-string
   #:lml-print-date
   #:alink
   #:alink-c

   ;; htmlgen.lisp
   #:html #:html-print #:html-print-subst #:html-print-list #:html-print-list-subst
   #:html-stream #:*html-stream*


   ;; files.lisp
   #:with-dir
   #:process-dir
   #:lml-load
   #:insert-file

   ;; stdsite.lisp
   #:print-std-page
   #:std-page
   #:std-body
   #:std-head
   #:titled-pre-section

   ;; downloads.lisp
   #:std-dl-page
   #:full-dl-page

   ;; utils.lisp
   #:lml-quit
   #:lml-cwd

   ;; apache-dir
   #:write-html-apache-directory
))
