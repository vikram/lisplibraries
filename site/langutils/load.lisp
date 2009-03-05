;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: cl-user -*-
;;;; *************************************************************************
;;;;
;;;; Filename:      load.lisp
;;;; Purpose:       Loader for the langutils standalone distribution
;;;; Depends:       Assumes that asdf is installed and that it can search
;;;;                the subdirectories below for .asd files.

(in-package :cl-user)


;;
;; Setup Step 1.
;;
;; Copy asdf.lisp to the root of the distribution and load it here 
;; if your setup does not already load it...
;;
#-:asdf (load "libs/asdf")

;;
;; Setup Step 2.
;;
;; Setup a logical pathname translation starting at a root 
;; directory such that the /data directory of this distribution
;; is a subdirectory of it
;;

(defparameter *langutils-directory-root* "/Users/eslick/Work/think/trunk/distributions/langutils-staging/")
(defun make-langutils-path (path) (merge-pathnames path *langutils-directory-root*))

(setf (logical-pathname-translations "think")
      `(("**;*" ,(make-langutils-path 
		  (make-pathname :directory '(:relative :wild-inferiors) :name :wild)))
        ("**;*.*" ,(make-langutils-path 
		    (make-langutils-path 
		     (make-pathname :directory '(:relative :wild-inferiors) :name :wild :type :wild))))))

;;
;; Setup Step 3.
;;
;; OPTIONAL - search for the subdirs of this root tree inside asdf
;; Uncomment #-nil statements if you want to copy these subdirs into 
;; your own asdf tree.  This allows us to find all the .asd files 
;; automatically
;;

;;#-nil
(defparameter *langutils-subdirs*
    '("langutils;"
      "langutils;libs;"))

;;#-nil 
(defun search-langutils-sources (sys)
  "Search under all the subdirectories in the langutils distribution"
  (let ((name (asdf::coerce-name sys)))
    (dolist (location *langutils-subdirs*)
      (let* ((dir (translate-logical-pathname (concatenate 'string "think:" location)))
	     (files (directory (merge-pathnames
				(make-pathname :name name
					       :type "asd"
					       :version :newest
					       :directory '(:relative :wild))
				dir))))
	(dolist (file files)
	  (when (probe-file file)
	    (return-from search-langutils-sources file)))))))

;;#-nil
(pushnew #'search-langutils-sources
	 asdf:*system-definition-search-functions*)

;;
;; Second, load our custom asdf extension
;;
(load "libs/asdf-config")

;;
;; Finally, load the system and its dependencies through ASDF
;;
;;(asdf:operate 'asdf-config:initialize-op :langutils)


