;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/html-extract/test.lisp,v 1.1.1.1 2005/09/22 22:09:22 edi Exp $

;;; Copyright (c) 2003, Dr. Edmund Weitz. All rights reserved.

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

(defun extract (html)
  (with-input-from-string (*standard-input* html)
    (with-output-to-string (*standard-output*)
      (html-extract))))

;; some simple test cases - there should be more... :)
(defparameter *test-cases*
  '(("<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 3.2 Final//EN\"> 
<HTML>
<BODY BGCOLOR=white>
This is the <A NAME=foo HREF=\"first.html\">first link</A>, and here's the <A HREF=\"mailto:bill@microsoft.com\" TITLE='bar'>second one</A>.
</BODY>
</HTML>"
     " 


This is the first link, and here's the second one.

")
    ("Just some plain text")
    ("Hier ein <a name=frotzel>Link</a>." "Hier ein Link.")
    ;; error in comment declaration
    ("<A HREF='/frob'>outer link</A> <!-- comment with embedded <A HREF='/frob'>link</A>-- --another comment-- error <a href=\"/foo/frob/bar.html\">...</a>"
     "outer link error ...")
    ;; wrong comment declaration
    ("<!--------->" "->")
    ("<% <a href=foo.html><img title='howdy' src=foo.gif border=0/></a>"
     "<% 
")
    ("<Form Name=Name-of-the-Form Action='/?session=foo345'><input type=text name=foo><br><input src='frob.gif?session=foo345' value='Press me'></form>" "")
    ("Tat&uuml Tata" "Tatü Tata")
    ("Hello&#45World" "Hello-World")
    ("&OumL;sterreich" "&OumLsterreich")
    ("<foo <b>bar</b>" "bar")
    ("<foo <b>bar</b>>" "bar>")
    ("<foo bar=\"baz\" <b>bar</b>" "bar")
    ("</foo <b>bar</b>" "bar")
    ("<foo>bar" "bar")
    ("<foo bar=\"baz\">bar" "bar")
    ("<u foo=quux>I</u>nternational <b>B</b>ureau <i>M</i>achines" "International Bureau Machines")
    ("I<br>B<p>M" "I
B
M")))

(defun test ()
  (loop for (input output) in *test-cases*
        for expected-output = (or output input)
        for i from 1 do
        (format t "~&Test #~A...~%" i)
        (force-output)
        unless (string= expected-output
                        (extract input)) do
        (format t "~&Test #~A failed~%" i))
  (values))