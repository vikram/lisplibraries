;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          data.lisp
;;;; Purpose:       Lisp Markup Language functions
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Aug 2002
;;;;
;;;; $Id$
;;;;
;;;; This file, part of LML2, is Copyright (c) 2000-2003 by Kevin Rosenberg.
;;;; Rights of modification and redistribution are in the LICENSE file.
;;;;
;;;; *************************************************************************

(in-package #:lml2)

(defvar *html-stream* *standard-output*)

(defvar *print-spaces* nil)
(defvar *indent* 0)

(defvar +xml-prologue-string+
  "<?xml version=\"1.0\" encoding=\"iso-8859-1\" standalone=\"yes\"?>")

(defvar +xml-prologue-begin+
  "<?xml version=\"1.0\" encoding=\"")

(defvar +xml-prologue-end+
  "\" standalone=\"yes\"?>")

(defvar +html4-dtd-string+
  "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML//EN\">")

(defvar +xhtml11-dtd-string+
  "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\"")

(defvar +xhtml10-strict-dtd-string+
  "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml10/DTD/xhtml10-strict.dtd\"")

(defvar +xhtml10-transitional-dtd-string+
  "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml10/DTD/xhtml10-transitional.dtd\"")

(defvar +xhtml10-frameset-dtd-string+
  "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Frameset//EN\" \"http://www.w3.org/TR/xhtml10/DTD/xhtml10-frameset.dtd\"")
