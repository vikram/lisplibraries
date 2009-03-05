;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/html-extract/specials.lisp,v 1.1.1.1 2005/09/22 22:09:22 edi Exp $

;;; Copyright (c) 2003, Dr. Edmund Weitz.  All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package #:html-extract)

(defvar *named-entities*
  '(("quot" . #.(code-char 34))
    ("amp" . #.(code-char 38))
    ("lt" . #.(code-char 60))
    ("gt" . #.(code-char 62))
    ("nbsp" . #.(code-char 160))
    ("iexcl" . #.(code-char 161))
    ("cent" . #.(code-char 162))
    ("pound" . #.(code-char 163))
    ("curren" . #.(code-char 164))
    ("yen" . #.(code-char 165))
    ("brvbar" . #.(code-char 166))
    ("sect" . #.(code-char 167))
    ("uml" . #.(code-char 168))
    ("copy" . #.(code-char 169))
    ("ordf" . #.(code-char 170))
    ("laquo" . #.(code-char 171))
    ("not" . #.(code-char 172))
    ("shy" . #.(code-char 173))
    ("reg" . #.(code-char 174))
    ("macr" . #.(code-char 175))
    ("deg" . #.(code-char 176))
    ("plusmn" . #.(code-char 177))
    ("sup2" . #.(code-char 178))
    ("sup3" . #.(code-char 179))
    ("acute" . #.(code-char 180))
    ("micro" . #.(code-char 181))
    ("para" . #.(code-char 182))
    ("middot" . #.(code-char 183))
    ("cedil" . #.(code-char 184))
    ("sup1" . #.(code-char 185))
    ("ordm" . #.(code-char 186))
    ("raquo" . #.(code-char 187))
    ("frac14" . #.(code-char 188))
    ("frac12" . #.(code-char 189))
    ("frac34" . #.(code-char 190))
    ("iquest" . #.(code-char 191))
    ("Agrave" . #.(code-char 192))
    ("Aacute" . #.(code-char 193))
    ("Acirc" . #.(code-char 194))
    ("Atilde" . #.(code-char 195))
    ("Auml" . #.(code-char 196))
    ("Aring" . #.(code-char 197))
    ("AElig" . #.(code-char 198))
    ("Ccedil" . #.(code-char 199))
    ("Egrave" . #.(code-char 200))
    ("Eacute" . #.(code-char 201))
    ("Ecirc" . #.(code-char 202))
    ("Euml" . #.(code-char 203))
    ("Igrave" . #.(code-char 204))
    ("Iacute" . #.(code-char 205))
    ("Icirc" . #.(code-char 206))
    ("Iuml" . #.(code-char 207))
    ("ETH" . #.(code-char 208))
    ("Ntilde" . #.(code-char 209))
    ("Ograve" . #.(code-char 210))
    ("Oacute" . #.(code-char 211))
    ("Ocirc" . #.(code-char 212))
    ("Otilde" . #.(code-char 213))
    ("Ouml" . #.(code-char 214))
    ("times" . #.(code-char 215))
    ("Oslash" . #.(code-char 216))
    ("Ugrave" . #.(code-char 217))
    ("Uacute" . #.(code-char 218))
    ("Ucirc" . #.(code-char 219))
    ("Uuml" . #.(code-char 220))
    ("Yacute" . #.(code-char 221))
    ("THORN" . #.(code-char 222))
    ("szlig" . #.(code-char 223))
    ("agrave" . #.(code-char 224))
    ("aacute" . #.(code-char 225))
    ("acirc" . #.(code-char 226))
    ("atilde" . #.(code-char 227))
    ("auml" . #.(code-char 228))
    ("aring" . #.(code-char 229))
    ("aelig" . #.(code-char 230))
    ("ccedil" . #.(code-char 231))
    ("egrave" . #.(code-char 232))
    ("eacute" . #.(code-char 233))
    ("ecirc" . #.(code-char 234))
    ("euml" . #.(code-char 235))
    ("igrave" . #.(code-char 236))
    ("iacute" . #.(code-char 237))
    ("icirc" . #.(code-char 238))
    ("iuml" . #.(code-char 239))
    ("eth" . #.(code-char 240))
    ("ntilde" . #.(code-char 241))
    ("ograve" . #.(code-char 242))
    ("oacute" . #.(code-char 243))
    ("ocirc" . #.(code-char 244))
    ("otilde" . #.(code-char 245))
    ("ouml" . #.(code-char 246))
    ("divide" . #.(code-char 247))
    ("oslash" . #.(code-char 248))
    ("ugrave" . #.(code-char 249))
    ("uacute" . #.(code-char 250))
    ("ucirc" . #.(code-char 251))
    ("uuml" . #.(code-char 252))
    ("yacute" . #.(code-char 253))
    ("thorn" . #.(code-char 254))
    ("yuml" . #.(code-char 255)))
  "Named HTML entities \(only those with char-codes below 255)
arranged as an alist.")

(defvar *space-tags*
  '("address" "applet" "blockquote" "body" "br" "button" "caption" "dd" "dir" "div" "dl" "dt" "fieldset" "h1" "h2" "h3" "h4" "h5" "h6" "hr" "iframe" "img" "input" "isindex" "label" "legend" "li" "menu" "noframes" "noscript" "object" "ol" "option" "p" "pre" "q" "select" "table" "tbody" "td" "textarea" "tfoot" "th" "thead" "title" "tr" "tt" "ul")
  "HTML tags which introduce a space around them.")

(defvar *find-string-hash*
  (make-hash-table :test #'equal)
  "Hash tables used internally by READ-UNTIL to cache offset arrays.")