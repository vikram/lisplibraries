;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          stdsite.lisp
;;;; Purpose:       Functions to create my standard style sites
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Aug 2002
;;;;
;;;; $Id$
;;;;
;;;; This file, part of LML2, is Copyright (c) 2000-2003 by Kevin Rosenberg.
;;;; Rights of modification and redistribution are in the LICENSE file.
;;;;
;;;; *************************************************************************


;;; A "standard site" is a format for a certain style of web page.
;;; It is based on the LML2 package.
;;; A stdsite page expects to include the following files:
;;;  header.lml_
;;;  banner.lml_
;;;  content.lml_
;;;  footer.lml_
;;; These files are optional
;;;  final.lml_
;;;  rightcol.lml_

(in-package #:lml2)

(defmacro std-head (title &body body)
  `(html
    (:head
     (:title (:princ ,title))
     (lml-load "header.lml_")
     ,@body)))


(defun std-footer (file)
  (html
   ((:div :class "disclaimsec")
    (let ((src-file (make-pathname
                     :defaults *sources-dir*
                     :type "lml"
                     :name (pathname-name file))))
      (when (probe-file src-file)
        (html
         ((:div :class "lastmod")
          (lml-format  "Last modified: ~A" (date-string (file-write-date src-file)))))))
    (lml-load "footer.lml_"))))


(defmacro std-body (file &body body)
  `(html
    (:body
     (lml-load "banner.lml_")
     ((:table :class "stdbodytable" :border "0" :cellpadding "3")
      (:tbody
       ((:tr :valign "top")
        ((:td :class "stdcontentcell")
         (lml-load "contents.lml_"))
        ((:td :valign "top")
         ,@body
         (std-footer ,file))
        ((:td :valign "top")
         (lml-load "rightcol.lml_" :optional t)))))
     (lml-load "final.lml_" :optional t))))


(defmacro print-std-page (file title format encoding &body body)
  `(progn
     (dtd-prologue ,format ,encoding)
     (html
      ((:html :xmlns "http://www.w3.org/1999/xhtml")
       (std-head ,title)
       (std-body ,file ,@body)))))

(defmacro std-page ((out-file title &key (format :xhtml10-strict) (encoding :utf-8))
                    &body body)
  `(let ((*indent* 0))
     (with-open-file (*html-stream* (lml-file-name ',out-file :output)
                      :direction :output
                      :if-exists :supersede)
       (print-std-page (lml-file-name ',out-file :source) ,title ,format ,encoding ,@body))))

(defmacro titled-pre-section (title &body body)
  `(progn
    (html
     (:h1 ,title)
     ((:pre "style" "padding-left:30pt;")
      ,@body))))
