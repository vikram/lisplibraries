;; -*- lisp -*-

(in-package :common-lisp-user)

(defpackage :it.bese.yaclml
  (:nicknames :yaclml)
  (:documentation "Yet Another Common Lisp Markup Language")
  (:use :common-lisp :it.bese.arnesi :iterate)
  (:export ;; code generation
           #:emit-princ
	   #:emit-html
	   #:emit-code
	   #:emit-princ-attributes
	   #:emit-open-tag
	   #:emit-close-tag
	   #:emit-empty-tag
	   #:emit-body
	   #:emit-xml-tag
           #:wrap-in-tag
           ;; defining tags
           #:deftag
           #:deftag-macro
           ;; using yaclml
           #:with-yaclml-stream
           #:enable-yaclml-syntax
           #:disable-yaclml-syntx
	   #:*yaclml-stream*
	   #:*yaclml-indent*
           ;; tal
           #:compile-tal-string
           #:compile-tal-file
           #:def-attribute-handler
           #:def-tag-handler
           #:read-tal-expression-from-string
	   #:tal-generator
	   #:file-system-generator
	   #:load-tal
	   #:template-truename
           #:root-directories
           #:*uri-to-package*
           #:transform-lxml-form
           #:transform-lxml-tree

           #:push-binding
           #:make-standard-environment
           #:extend-environment
           #:add-binding
           #:lookup-tal-variable
           #:tal-env))

(defpackage :it.bese.yaclml.tags
  (:nicknames :<)
  (:documentation "YACLML programmatic HTML generation.")
  (:use)
  (:export ;; HTML4
           #:a #:abbr #:acronym #:address #:area #:b #:base #:bdo #:big
           #:blockquote #:body #:br #:button #:caption #:cite #:code #:col
           #:colgroup #:dd #:del #:dfn #:div #:dl #:dt #:em #:fieldset #:form
           #:frame #:frameset #:h1 #:h2 #:h3 #:h4 #:h5 #:h6 #:head #:hr #:html
           #:i #:iframe #:img #:input #:ins #:kbd #:label #:legend #:li #:link
           #:map #:meta #:noframes #:noscript #:object #:ol #:optgroup #:option
           #:p #:param #:pre #:q #:samp #:script #:select #:small #:span
           #:strong #:style #:sub #:sup #:table #:tbody #:td #:textarea #:tfoot           
           #:th #:thead #:title #:tr #:tt #:ul #:var
           ;; Not really HTML4, but close enough
           #:applet #:param #:marquee
           ;; YACLML extended HTML
           #:href #:stylesheet #:text #:submit #:image #:checkbox #:file
           #:as-is #:as-html #:call-with-yaclml-stream #:comment #:progn
	   #:&nbsp))

(defpackage :it.bese.yaclml.tal
  (:use)
  (:documentation "An HTML template authoring library.")
  (:nicknames :tal)
  (:export #:tal 
           #:content
           #:replace
           #:when
           #:dolist
           #:include
           #:in-package))

(defpackage :it.bese.yaclml.tal.include-params
  (:use))

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
