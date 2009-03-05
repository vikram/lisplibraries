;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          web-utils.lisp
;;;; Purpose:       Basic web utility functions
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Apr 2000
;;;;
;;;; $Id$
;;;;
;;;; This file, part of KMRCL, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;;
;;;; KMRCL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:kmrcl)


;;; HTML/XML constants

(defvar *standard-xml-header*
  #.(format nil "<?xml version=\"1.0\" encoding=\"iso-8859-1\" standalone=\"yes\"?>~%"))

(defvar *standard-html-header* "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML//EN\">")

(defvar *standard-xhtml-header*
  #.(format nil "<?xml version=\"1.0\" encoding=\"iso-8859-1\" standalone=\"yes\"?>~%<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">"))


;;; User agent functions

(defun user-agent-ie-p (agent)
  "Takes a user-agent string and returns T for Internet Explorer."
  (or (string-starts-with "Microsoft" agent)
      (string-starts-with "Internet Explore" agent)
      (search "Safari" agent)
      (search "MSIE" agent)))

;;; URL Functions

(defvar *base-url* "")
(defun base-url! (url)
  (setq *base-url* url))

(defun make-url (page-name &key (base-dir *base-url*) (format :html) vars anchor)
  (let ((amp (case format
               (:html
                "&")
               ((:xml :ie-xml)
                "&amp;"))))
    (concatenate 'string
      base-dir page-name
      (if vars
          (let ((first-var (first vars)))
            (concatenate 'string
              "?"  (car first-var) "=" (cdr first-var)
              (mapcar-append-string
               #'(lambda (var)
                   (when (and (car var) (cdr var))
                     (concatenate 'string
                       amp (string-downcase (car var)) "=" (cdr var))))
               (rest vars))))
        "")
      (if anchor
          (concatenate 'string "#" anchor)
        ""))))

(defun decode-uri-query-string (s)
  "Decode a URI query string field"
  (declare (simple-string s)
           (optimize (speed 3) (safety 0) (space 0)))
  (do* ((old-len (length s))
        (new-len (- old-len (* 2 (the fixnum (count-string-char s #\%)))))
        (new (make-string new-len))
        (p-old 0)
        (p-new 0 (1+ p-new)))
       ((= p-new new-len) new)
    (declare (simple-string new)
             (fixnum p-old p-new old-len new-len))
         (let ((c (schar s p-old)))
           (when (char= c #\+)
             (setq c #\space))
           (case c
             (#\%
              (unless (>= old-len (+ p-old 3))
                (error "#\% not followed by enough characters"))
              (setf (schar new p-new)
                    (code-char
                     (parse-integer (subseq s (1+ p-old) (+ p-old 3))
                                    :radix 16)))
              (incf p-old 3))
             (t
              (setf (schar new p-new) c)
              (incf p-old))))))

(defun split-uri-query-string (s)
  (mapcar
   (lambda (pair)
     (let ((pos (position #\= pair)))
       (when pos
         (cons (subseq pair 0 pos)
               (when (> (length pair) pos)
                 (decode-uri-query-string (subseq pair (1+ pos))))))))
   (delimited-string-to-list s #\&)))
