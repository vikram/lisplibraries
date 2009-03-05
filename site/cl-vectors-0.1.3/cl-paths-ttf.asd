;;;; -*- Lisp -*- mode

;;;; cl-vectors -- Rasterizer and paths manipulation library
;;;; Copyright (C) 2007  Frédéric Jolliton <frederic@jolliton.com>
;;;; 
;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), known as the LLGPL.
;;;; 
;;;; This library is distributed in the hope that it will be useful, but
;;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the Lisp
;;;; Lesser GNU Public License for more details.

(defpackage #:cl-paths-ttf-system
  (:use #:cl #:asdf))

(in-package #:cl-paths-ttf-system)

(defsystem #:cl-paths-ttf
  :description "cl-paths-ttf: vectorial paths manipulation"
  :version "0.1.3"
  :author "Frederic Jolliton <frederic@jolliton.com>"
  :license "LLGPL (http://opensource.franz.com/preamble.html)"
  :depends-on ("cl-paths" "zpb-ttf")
  :components ((:file "paths-ttf")))