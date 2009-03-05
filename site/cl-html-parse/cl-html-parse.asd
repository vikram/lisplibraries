;;;-*- Mode: Lisp; Package: COMMON-LISP-USER -*-

#| simple-header

Copyright (c) 2001-2003 Gary Warren King (gwking@cs.umass.edu) 

Permission is hereby granted, free of charge, to any person obtaining a 
copy of this software and associated documentation files (the "Software"),
to deal in the Software without restriction, including without limitation
the rights to use, copy, modify, merge, publish, distribute, sublicense, 
and/or sell copies of the Software, and to permit persons to whom the 
Software is furnished to do so, subject to the following conditions: 

The above copyright notice and this permission notice shall be included in 
all copies or substantial portions of the Software. 

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL 
THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER 
DEALINGS IN THE SOFTWARE. 

|#


;; copyright (c) 1986-2000 Franz Inc, Berkeley, CA 
;;
;; This code is free software; you can redistribute it and/or
;; modify it under the terms of the version 2.1 of
;; the GNU Lesser General Public License as published by 
;; the Free Software Foundation, as clarified by the AllegroServe
;; prequel found in license-allegroserve.txt.
;;
;; This code is distributed in the hope that it will be useful,
;; but without any warranty; without even the implied warranty of
;; merchantability or fitness for a particular purpose.  See the GNU
;; Lesser General Public License for more details.
;;
;; Version 2.1 of the GNU Lesser General Public License is in the file 
;; license-lgpl.txt that was distributed with this file.
;; If it is not present, you can access it from
;; http://www.gnu.org/copyleft/lesser.txt (until superseded by a newer
;; version) or write to the Free Software Foundation, Inc., 59 Temple Place, 
;; Suite 330, Boston, MA  02111-1307  USA
;;


(defpackage :asdf-cl-http-parse (:use #:asdf #:cl))
(in-package :asdf-cl-http-parse)

(defsystem cl-html-parse
  :author "Gary Warren King <gwking@metabang.com>"
  :version "1.0"
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "MIT Style license for the packaging, AllegroServe license for the code."
  :description "HTML Parser"
  :long-description "Franz's HTML Parser packaged up separately for ASDF."
  :components ((:static-file "COPYING")
               (:module "dev"
                        :components ((:static-file "README")
                                     
                                     (:file "package")
                                     #-allegro
                                     (:file "if-star" 
                                            :depends-on ("package"))
                                     (:file "cl-html-parse" 
                                            :depends-on ("package" 
                                                         #-allegro "if-star"))))))


