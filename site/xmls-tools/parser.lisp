;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; parser.lisp: Code for writing parsers based on XMLS
;;;; Copyright (C) 2004 Robert Marlow <bobstopper@bobturf.org>
;;;;
;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Library General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 2 of the License, or (at your option) any later version.
;;;;
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Library General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Library General Public
;;;; License along with this library; if not, write to the
;;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;;; Boston, MA  02111-1307, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :xmls-tools)


(defun make-xml-parser (&optional (default-handler #'ignore-element))
  (cons (make-hash-table :test #'equal)
	default-handler))


(defun ignore-element (tree &rest args)
  (declare (ignore tree args))
  nil)


(defmacro def-element-handler (element-name
			       (xml-tree-name &rest args)
			       (xml-parser)
			       &body handler-code)
  `(setf (gethash ,element-name (car ,xml-parser))
    (let ((xml-parser ,xml-parser))
      (declare (ignorable xml-parser))
      (lambda (,xml-tree-name ,@args state)
	(declare (ignorable ,xml-tree-name state ,@args))
	,@handler-code))))


(defmacro with-state ((state-key state-value) &body body)
  `(let ((state (acons ,state-key ,state-value state)))
    ,@body))


(defmacro with-parser (xml-parser &body body)
  `(let ((state nil)
	 (xml-parser ,xml-parser))
    ,@body))
    

(defmacro process-xml (tree &rest args)
  `(if (null ,tree)
    nil
    (funcall (gethash (node-name ,tree) (car xml-parser)
	      (cdr xml-parser))
     ,tree ,@args state)))


(defun %compress-whitespace (string)
  (let ((out-stream (make-string-output-stream))
	(in-whitespace-p nil))
    (do ((i 0 (1+ i)))
	((>= i (length string))
	 (get-output-stream-string out-stream))
      (let ((c (char string i)))
	(cond
	  ((member c xmls::*whitespace*)
	   (unless in-whitespace-p
	     (princ #\space out-stream)
	     (setf in-whitespace-p t)))
	  (t
	   (princ c out-stream)
	   (when in-whitespace-p
	     (setf in-whitespace-p nil))))))))


(defmacro compress-whitespace (string)
  `(if (cdr (assoc :compress-whitespace state))
    (%compress-whitespace ,string)
    ,string))
    

(defmacro with-indent (stream &body body)
  `(let* ((old-stream ,stream)
	  (,stream (make-string-output-stream)))
    ,@body
    ;; print line by line with single-space indents on front
    (princ #\space old-stream)
    (do ((string (get-output-stream-string ,stream))
	 (i 0 (1+ i)))
	((>= i (length string)))
      (let ((c (char string i)))
	(princ c old-stream)
	(when (and (char= c #\newline)
		   (< i (1- (length string))))
	  (princ #\space old-stream))))))

