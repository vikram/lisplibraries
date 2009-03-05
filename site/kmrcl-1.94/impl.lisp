;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          impl.lisp
;;;; Purpose:       Implementation Dependent routines for kmrcl
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Sep 2003
;;;;
;;;; $Id: impl.lisp 11421 2006-12-31 18:42:10Z kevin $
;;;;
;;;; This file, part of KMRCL, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;;
;;;; KMRCL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:kmrcl)

(defun canonicalize-directory-name (filename)
  (flet ((un-unspecific (value)
	   (if (eq value :unspecific) nil value)))
    (let* ((path (pathname filename))
	   (name (un-unspecific (pathname-name path)))
	   (type (un-unspecific (pathname-type path)))
	   (new-dir
	    (cond ((and name type) (list (concatenate 'string name "." type)))
		  (name (list name))
		  (type (list type))
		  (t nil))))
      (if new-dir
	  (make-pathname
	   :directory (append (un-unspecific (pathname-directory path))
			      new-dir)
		    :name nil :type nil :version nil :defaults path)
	  path))))


(defun probe-directory (filename &key (error-if-does-not-exist nil))
  (let* ((path (canonicalize-directory-name filename))
	 (probe
	  #+allegro (excl:probe-directory path)
	  #+clisp (values
		   (ignore-errors
		     (#+lisp=cl ext:probe-directory
				#-lisp=cl lisp:probe-directory
				path)))
	  #+(or cmu scl) (when (eq :directory
				   (unix:unix-file-kind (namestring path)))
			   path)
	  #+lispworks (when (lw:file-directory-p path)
			path)
	  #+sbcl (when (eq :directory
			   (sb-unix:unix-file-kind (namestring path)))
		   path)
	  #-(or allegro clisp cmu lispworks sbcl scl)
	  (probe-file path)))
    (if probe
	probe
	(when error-if-does-not-exist
	  (error "Directory ~A does not exist." filename)))))

(defun cwd (&optional dir)
  "Change directory and set default pathname"
  (cond
   ((not (null dir))
    (when (and (typep dir 'logical-pathname)
	       (translate-logical-pathname dir))
      (setq dir (translate-logical-pathname dir)))
    (when (stringp dir)
      (setq dir (parse-namestring dir)))
    #+allegro (excl:chdir dir)
    #+clisp (#+lisp=cl ext:cd #-lisp=cl lisp:cd dir)
    #+(or cmu scl) (setf (ext:default-directory) dir)
    #+cormanlisp (ccl:set-current-directory dir)
    #+(and mcl (not openmcl)) (ccl:set-mac-default-directory dir)
    #+openmcl (ccl:cwd dir)
    #+gcl (si:chdir dir)
    #+lispworks (hcl:change-directory dir)
    (setq cl:*default-pathname-defaults* dir))
   (t
    (let ((dir
	   #+allegro (excl:current-directory)
	   #+clisp (#+lisp=cl ext:default-directory #-lisp=cl lisp:default-directory)
	   #+(or cmu scl) (ext:default-directory)
	   #+sbcl (sb-unix:posix-getcwd/)
	   #+cormanlisp (ccl:get-current-directory)
	   #+lispworks (hcl:get-working-directory)
	   #+mcl (ccl:mac-default-directory)
	   #-(or allegro clisp cmu scl cormanlisp mcl sbcl lispworks) (truename ".")))
      (when (stringp dir)
	(setq dir (parse-namestring dir)))
      dir))))



(defun quit (&optional (code 0))
  "Function to exit the Lisp implementation. Copied from CLOCC's QUIT function."
    #+allegro (excl:exit code :quiet t)
    #+clisp (#+lisp=cl ext:quit #-lisp=cl lisp:quit code)
    #+(or cmu scl) (ext:quit code)
    #+cormanlisp (win32:exitprocess code)
    #+gcl (lisp:bye code)
    #+lispworks (lw:quit :status code)
    #+lucid (lcl:quit code)
    #+sbcl (sb-ext:quit :unix-status (typecase code (number code) (null 0) (t 1)))
    #+mcl (ccl:quit code)
    #-(or allegro clisp cmu scl cormanlisp gcl lispworks lucid sbcl mcl)
    (error 'not-implemented :proc (list 'quit code)))


(defun command-line-arguments ()
  #+allegro (system:command-line-arguments)
  #+sbcl sb-ext:*posix-argv*
  )

(defun copy-file (from to &key link overwrite preserve-symbolic-links
		  (preserve-time t) remove-destination force verbose)
  #+allegro (sys:copy-file from to :link link :overwrite overwrite
			   :preserve-symbolic-links preserve-symbolic-links
			   :preserve-time preserve-time
			   :remove-destination remove-destination
			   :force force :verbose verbose)
  #-allegro
  (declare (ignore verbose preserve-symbolic-links overwrite))
  (cond
    ((and (typep from 'stream) (typep to 'stream))
     (copy-binary-stream from to))
    ((not (probe-file from))
     (error "File ~A does not exist." from))
    ((eq link :hard)
     (run-shell-command "ln -f ~A ~A" (namestring from) (namestring to)))
    (link
     (multiple-value-bind (stdout stderr status)
	 (command-output "ln -f ~A ~A" (namestring from) (namestring to))
       (declare (ignore stdout stderr))
       ;; try symbolic if command failed
       (unless (zerop status)
	 (run-shell-command "ln -sf ~A ~A" (namestring from) (namestring to)))))
    (t
     (when (and (or force remove-destination) (probe-file to))
       (delete-file to))
     (let* ((options (if preserve-time
			 "-p"
			 ""))
	    (cmd (format nil "cp ~A ~A ~A" options (namestring from) (namestring to))))
       (run-shell-command cmd)))))
