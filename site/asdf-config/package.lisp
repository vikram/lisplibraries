(defpackage :asdf-config
  (:use #:cl #:asdf)
  (:export #:defsystem-config
	   #:system-config
	   #:configure-op
	   #:initialize-op
	   #:set-parameter
	   #:set-parameters
	   #:get-lisp-source-filenames
	   #:get-all-sources))

(in-package #:asdf-config)
