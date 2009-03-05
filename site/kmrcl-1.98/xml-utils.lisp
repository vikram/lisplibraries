;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          xml-utils.lisp
;;;; Purpose:       XML utilities
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


;;; XML Extraction Functions

(defun find-start-tag (tag taglen xmlstr start end)
  "Searches for the start of a tag in an xmlstring. Returns STARTPOS ATTRIBUTE-LIST)"
  (declare (simple-string tag xmlstr)
           (fixnum taglen start end)
           (optimize (speed 3) (safety 0) (space 0)))
  (do* ((search-str (concatenate 'string "<" tag))
        (search-len (1+ taglen))
        (bracketpos (fast-string-search search-str xmlstr search-len start end)
                    (fast-string-search search-str xmlstr search-len start end)))
       ((null bracketpos) nil)
    (let* ((endtag (+ bracketpos 1 taglen))
           (char-after-tag (schar xmlstr endtag)))
      (when (or (char= #\> char-after-tag)
                (char= #\space char-after-tag))
        (if (char= #\> char-after-tag)
            (return-from find-start-tag (values (1+ endtag) nil))
            (let ((endbrack (position-char #\> xmlstr (1+ endtag) end)))
              (if endbrack
                  (return-from find-start-tag
                    (values (1+ endbrack)
                            (string-to-list-skip-delimiter
                             (subseq xmlstr endtag endbrack))))
                  (values nil nil)))))
      (setq start endtag))))


(defun find-end-tag (tag taglen xmlstr start end)
  (fast-string-search
   (concatenate 'string "</" tag ">") xmlstr
   (+ taglen 3) start end))

(defun positions-xml-tag-contents (tag xmlstr &optional (start-xmlstr 0)
                                       (end-xmlstr (length xmlstr)))
  "Returns three values: the start and end positions of contents between
 the xml tags and the position following the close of the end tag."
  (let* ((taglen (length tag)))
    (multiple-value-bind (start attributes)
        (find-start-tag tag taglen xmlstr start-xmlstr end-xmlstr)
      (unless start
        (return-from positions-xml-tag-contents (values nil nil nil nil)))
      (let ((end (find-end-tag tag taglen xmlstr start end-xmlstr)))
        (unless end
          (return-from positions-xml-tag-contents (values nil nil nil nil)))
        (values start end (+ end taglen 3) attributes)))))


(defun xml-tag-contents (tag xmlstr &optional (start-xmlstr 0)
                         (end-xmlstr (length xmlstr)))
  "Returns two values: the string between XML start and end tag
and position of character following end tag."
  (multiple-value-bind
      (startpos endpos nextpos attributes)
      (positions-xml-tag-contents tag xmlstr start-xmlstr end-xmlstr)
    (if (and startpos endpos)
        (values (subseq xmlstr startpos endpos) nextpos attributes)
      (values nil nil nil))))

(defun cdata-string (str)
  (concatenate 'string "<![CDATA[" str "]]>"))

(defun write-cdata (str s)
  (declare (simple-string str) (optimize (speed 3) (safety 0) (space 0)))
  (do ((len (length str))
       (i 0 (1+ i)))
      ((= i len) str)
    (declare (fixnum i len))
    (let ((c (schar str i)))
      (case c
        (#\< (write-string "&lt;" s))
        (#\& (write-string "&amp;" s))
        (t   (write-char c s))))))

(defun xml-declaration-stream (stream &key (version "1.0") standalone encoding)
  (format stream "<?xml version=\"~A\"~A~A ?>~%"
          version
          (if encoding
              (format nil " encoding=\"~A\"" encoding)
              ""
              )
          (if standalone
              (format nil " standalone=\"~A\"" standalone)
              "")))

(defun doctype-stream (stream top-element availability registered organization type
                       label language url entities)
  (format stream "<!DOCTYPE ~A ~A \"~A//~A//~A ~A//~A\"" top-element
          availability (if registered "+" "-") organization type label language)

  (when url
    (write-char #\space stream)
    (write-char #\" stream)
    (write-string url stream)
    (write-char #\" stream))

  (when entities
    (format stream " [~%~A~%]" entities))

  (write-char #\> stream)
  (write-char #\newline stream))

(defun doctype-format (stream format &key top-element (availability "PUBLIC")
                       (registered nil) organization (type "DTD") label
                       (language "EN") url entities)
  (case format
    ((:xhtml11 :xhtml)
     (doctype-stream stream "html" availability registered "W3C" type "XHTML 1.1" language
                     (if url url "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd")
                     entities))
    (:xhtml10-strict
     (doctype-stream stream "html" availability registered "W3C" type "XHTML 1.0 Strict" language
                     (if url url "http://www.w3.org/TR/xhtml10/DTD/xhtml10-strict.dtd")
                     entities))
    (:xhtml10-transitional
     (doctype-stream stream "html" availability registered "W3C" type "XHTML 1.0 Transitional" language
                     (if url url "http://www.w3.org/TR/xhtml10/DTD/xhtml10-transitional.dtd")
                     entities))
    (:xhtml-frameset
     (doctype-stream stream "html" availability registered "W3C" type "XHTML 1.0 Frameset" language
                     (if url url "http://www.w3.org/TR/xhtml10/DTD/xhtml10-frameset.dtd")
                     entities))
    (:html2
     (doctype-stream stream "HTML" availability registered "IETF" type "HTML" language url entities))
    (:html3
     (doctype-stream stream "HTML" availability registered "IETF" type "HTML 3.0" language url entities))
    (:html3.2
     (doctype-stream stream "HTML" availability registered "W3C" type "HTML 3.2 Final" language url entities))
    ((:html :html4)
     (doctype-stream stream "HTML" availability registered "W3C" type "HTML 4.01 Final" language url entities))
    ((:docbook :docbook42)
     (doctype-stream stream (if top-element top-element "book")
                     availability registered "OASIS" type "Docbook XML 4.2" language
                     (if url url "http://www.oasis-open.org/docbook/xml/4.2/docbookx.dtd")
                     entities))
    (t
     (unless top-element (warn "Missing top-element in doctype-format"))
     (unless organization (warn "Missing organization in doctype-format"))
     (unless label (warn "Missing label in doctype-format"))
     (doctype-stream stream top-element availability registered organization type label language url
                     entities))))


(defun sgml-header-stream (format stream &key entities (encoding "iso-8859-1") standalone (version "1.0")
                          top-element (availability "PUBLIC") registered organization (type "DTD")
                           label (language "EN") url)
  (when (in format :xhtml :xhtml11 :xhtml10-strict :xhtml10-transitional :xhtml10-frameset :xml :docbook)
    (xml-declaration-stream stream :version version :encoding encoding :standalone standalone))
  (unless (eq :xml format)
    (doctype-format stream format :top-element top-element
                    :availability availability :registered registered
                    :organization organization :type type :label label :language language
                    :url url :entities entities))
  stream)

