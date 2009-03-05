;; This software is Copyright (c) 2006, Steffen Siering.
;; <COMPANY> grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :cffi-util)

;;;; * Flag Systems

;; macros/functions for dealing with flags in cstyle using integers...
;; flag-systems help to avoid name clashes... blahblahblah

(defvar *flag-systems* (make-hash-table))
(defvar *current-flag-system* nil)

(def-construct flag-system
    parent
  (flags (make-hash-table)))

(defun find-flag-system (name)
  (gethash name *flag-systems*))

(defun (setf find-flag-system) (system name)
  (setf (gethash name *flag-systems*) system))

(defun delete-flag-system (name)
  (remhash name *flag-systems*))

(defun clear-flag-systems ()
  (clrhash *flag-systems*))

(defun use-flagsystem (name)
  (setf *current-flag-system*
	(find-flag-system name)))

(defun def-flag-system (name &optional parent)
  "flag systems may be children of other flag systems"
  (if (not (keywordp name))
    (error "flag-system name must be a keyword, but found: ~A" name)
    (if-bind old-system (find-flag-system name)
	     (update-flag-system old-system parent)
	     (setf (find-flag-system name)
		   (make-flag-system parent)))))

(defun update-flag-system (fs new-parent)
  (setf (flag-system-parent fs) new-parent)
  fs)

(defmacro with-flag-system (flag-system &body body)
  `(let ((*current-flag-system* ,flag-system))
    (declare (special *current-flag-system*))
    ,@body))

;;flags must be numbers

(defun direct-flag-value (flag &optional (fs *current-flag-system*))
  (gethash flag (flag-system-flags fs)))

(defun flag-value (flag &optional (fs *current-flag-system*))
  (if (not fs)
      0
      (multiple-value-bind (value present)
	  (gethash flag (flag-system-flags fs))
	(if present
	    value
	    (flag-value flag (find-flag-system (flag-system-parent fs)))))))

(defun (setf flag-value) (value flag &optional (fs *current-flag-system*))
  (setf (gethash flag (flag-system-flags fs)) value))

(defun clear-flags (&optional (fs *current-flag-system*))
  (clrhash (flag-system-flags fs)))

(defun concat-flags (flag-list &optional (fs *current-flag-system*))
  "creates an integer using logior using a list of flags:
   e.g. (concat-flags '(:append :create <more-symbols>))"
  (reduce (lambda (cur flag)
	    (logior cur
		    (if-bind val (flag-value flag fs)
			     val
			     (error "unknown flag: ~A" flag))))
	  flag-list
	  :initial-value 0))

(defmacro flags (&rest args)
  "creates code using a flag-system and a list of args.
   All functions using this macros must be recompiled if
   they're using flags being redefined in a flag-system
   after compilation.
   Uses *current-flag-system*... use with-flag-system 
   if you need to use another flag-system within this macro"
  `(logior ,@(mapcar (lambda (expr)
		       (if-bind val (flag-value (first expr))
				`(if ,(second expr) ,val 0)
				(error "unknown flag: ~A" (first expr))))
		     (group args 2))))

(defun def-flag (flag value &optional (fs *current-flag-system*))
  (setf (flag-value flag fs) value))

(defmacro def-flag* (flag-key const-name value
		  &optional (fs *current-flag-system*))
  "same as def-flag, but creates a constant"
  (setf (flag-value flag-key fs) value)
  (eval
    `(defconstant ,const-name ,value)))

(defun has-flag (value flag &optional (flag-system *current-flag-system*))
  (let ((flag-value (flag-value flag flag-system)))
   (= flag-value (logand value flag-value))))

(defun clr-flag (value flag &optional (flag-system *current-flag-system*))
  (cond ((and (listp flag) (not (null flag))) ;; list of flags?
	 (clr-flag (clr-flag value (cdr flag) flag-system)
		   (car flag)
		   flag-system))
	 ((not (null flag)) ;;one flag itself
	  (logand value (lognot (flag-value flag flag-system))))
	 (t value)))

(defun set-flag (value flag &optional (flag-system *current-flag-system*))
  (cond ((and (listp flag) (not (null flag)))
	 (set-flag (set-flag value (cdr flag) flag-system)
		   (car flag)
		   flag-system))
	((not (null flag))
	 (logior value (flag-value flag flag-system)))
	(t value)))
  
(defun flag-list (value &key
		  (flag-system *current-flag-system*)
		  flags)
  "returns a list of flags, which byte pattern is found in value"
  (let ((ret (list)))
    (if flags
	(mapc (lambda (key)
		(when (has-flag value key flag-system)
		  (push key ret)))
	      flags)
	(maphash (lambda (key val)
		   (when (= val (logand val value))
		     (push key ret)))
		 (flag-system-flags flag-system)))
    ret))
