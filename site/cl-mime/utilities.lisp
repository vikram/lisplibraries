;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; utilities.lisp: General non-mime specific utilities
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



(in-package :mime)


;;; This macro does very little other than tidy up the do-loops I tend to do
;;; when reading files line-by-line. 
(defmacro read-lines ((line-var stream) (exit-clause &body exit-body) &body body)
  "Reads lines into LINE-VAR from STREAM until either EOF is
reached or EXIT-CLAUSE is true where upon EXIT-BODY is executed.
Executes BODY for every line in the file"
  `(do ((,line-var (read-line ,stream nil 'eof)
		   (read-line ,stream nil 'eof)))
       ((or (eql ,line-var 'eof)
	    ,exit-clause)
	,@exit-body)
     ,@body))


;;; This makes it simple to convert the contents of a stream to a string
(defmacro read-stream-to-string (stream line-var &optional exit-clause)
  "Reads STREAM until EOF and returns a string containing the contents"
  (let ((string-stream (gensym)))
    `(with-output-to-string
       (,string-stream)
       (read-lines (,line-var ,stream)
		   (,exit-clause t)
		   (princ ,line-var ,string-stream)
		   (terpri ,string-stream)))))


;;; These macros stolen from KMRCL
(defmacro aif (test then &optional else)
  `(let ((it ,test))
    (if it ,then ,else)))


(defun ensure-keyword (name)
  "Returns keyword for a name"
  (etypecase name
    (keyword name)
    (string (nth-value 0 (intern (string-upcase name) :keyword)))
    (symbol (nth-value 0 (intern (symbol-name name) :keyword)))))

