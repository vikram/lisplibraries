;; -*- lisp -*-

(in-package :it.bese.yaclml)

;;;; * YACLML shortcuts

(defvar *html-prologue* "<!DOCTYPE html
PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"
\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">")

(deftag <:comment (&body body)
  (emit-princ "<!--")
  (emit-body body)
  (emit-princ "-->"))

(deftag-macro <:href (url &allow-other-attributes others &body body)
  (if body
      `(<:a :href ,url ,@others ,@body)
      `(<:a :href ,url ,@others ,url)))

(deftag-macro <:stylesheet (sheet &allow-other-attributes others)
  `(<:link :rel "stylesheet" :href ,sheet ,@others))

(deftag-macro <:text (&allow-other-attributes others)
  `(<:input :type "text" ,@others))

(deftag-macro <:submit (&allow-other-attributes others)
  `(<:input :type "submit" ,@others))

(deftag-macro <:image (&allow-other-attributes others)
  `(<:input :type "image" ,@others))

(deftag-macro <:checkbox (&allow-other-attributes others)
  `(<:input :type "checkbox" ,@others))

(deftag-macro <:file (&allow-other-attributes others)
  `(<:input :type "file" ,@others))

(deftag <:&nbsp () (emit-princ "&nbsp;"))

(def-html-tag <:marquee
  width
  height
  direction
  behaviour
  scrolldelay
  scrollamount
  loop
  bgcolor
  hspace
  vspace)

(def-html-tag <:applet code archive width height)

(deftag <:param (&attribute name value)
  (emit-empty-tag "param" `(("name" . ,name)
                            ("value" . ,value))))

;; Copyright (c) 2002-2005, Edward Marco Baringer
;; All rights reserved. 
;; 
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;; 
;;  - Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 
;;  - Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;;  - Neither the name of Edward Marco Baringer, nor BESE, nor the names
;;    of its contributors may be used to endorse or promote products
;;    derived from this software without specific prior written permission.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
