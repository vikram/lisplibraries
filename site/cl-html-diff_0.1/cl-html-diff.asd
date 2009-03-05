;;; ------------------------------------------------- -*- Mode: LISP -*-
;;; CL-HTML-DIFF -- A Lisp library for generating human readable diffs
;;; of HTML documents, using HTML.
;;;
;;; Copyright 2005 
;;; John Wiseman (jjwiseman@yahoo.com)
;;; $Id: cl-html-diff.asd,v 1.3 2005/02/24 20:44:28 wiseman Exp $
;;;
;;; Licensed under the MIT license--see the accompanying LICENSE.txt
;;; file.
;;;
;;; This is the ASDF system definition.

(in-package :asdf)

(defsystem :cl-html-diff
    :name "CL-HTML-DIFF"
    :author "John Wiseman <jjwiseman@yahoo.com>"
    :version "0.1"
    :maintainer "John Wiseman <jjwiseman@yahoo.com>"
    :licence "MIT"
    :depends-on (:cl-difflib)
    :components ((:file "html-diff")
		 (:static-file "LICENSE.txt")))
