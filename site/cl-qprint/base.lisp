;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; base.lisp: The Program
;;;; Copyright (C) 2004 Robert Marlow <rob@bobturf.org>
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


(defpackage cl-qprint
  (:use :cl)
  (:nicknames :qprint)
  (:export :encode
	   :decode))


(in-package :cl-qprint)

(defun decode (input)
  "INPUT must be a string or a stream. Reads quoted-printable encoding
from INPUT and produces the equivalent decoded string"
  (let ((out-stream (make-string-output-stream))
	(in-stream
	 (typecase input
	   (string (make-string-input-stream input))
	   (stream input))))
    (do ((char (read-char in-stream nil 'eof)
	       (read-char in-stream nil 'eof)))
	((eql char 'eof)
	 (get-output-stream-string out-stream))
      (princ (if (char= char #\=)
		 (let ((char2 (read-char in-stream)))
		   ;; Check for and convert all newlines (LF or CRLF)
		   ;; to nothing. The = indicates a soft line break.
		   (if (member char2 '(#\return #\linefeed)
			       :test #'char=)
		       (let ((char3 (read-char in-stream nil 'eof)))
			 (cond
			   ((eql char3 'eof) "")
			   ((and (char= char3 #\linefeed)
				 (char= char2 #\return)) "")
			   (t char3)))
		       ;; If not a newline the = indicates encoding
		       (code-char (parse-integer
				   (format nil "~C~C"
					   char2
					   (read-char in-stream nil 'eof))
				   :radix 16))))
		 char)
	     out-stream))))


(defun cr-lf (stream)
  "Prints a CRLF sequence to STREAM. RFC 2045 mandates CRLF for newlines"
  (princ #\return stream)
  (princ #\linefeed stream))


(defun encode (input)
  "INPUT must be either a string or a stream. Reads from INPUT and produces
a quoted-printable encoded string"
  (let ((out-stream (make-string-output-stream))
	(in-stream
	 (typecase input
	   (string (make-string-input-stream input))
	   (stream input)))
	(last-line-break 0)
	(ws nil))
    
    (do ((c (read-char in-stream nil 'eof)
	    (read-char in-stream nil 'eof))
	 (position 0 (file-position out-stream)))
	((eql c 'eof)
	 (get-output-stream-string out-stream))

      ;; Put in a soft line break if the line's gotten too long
      (when (>= (- position last-line-break) 74)
	(princ #\= out-stream)
	(cr-lf out-stream)
	(setf last-line-break position))
      
      ;; ws on the end of a line must be encoded
      (when ws
	(if (char= c #\newline)
	    (format out-stream "=~2,'0X" (char-code ws))
	    (princ ws out-stream)))
      
      (cond

	;; Ensure newlines are CR-LF
	((char= c #\newline)
	 (cr-lf out-stream)
	 (setf last-line-break position))

	;; Keep track of whitespace in case of following newlines
	((member c '(#\space #\tab) :test #'char=)
	 (setf ws c))
	
	;; Encode non-printable characters and =
	((or (char< c #\!)
	     (char> c #\~)
	     (char= c #\=))
	 (format out-stream "=~2,'0X" (char-code c)))

	;; Else just print the character.
	(t (princ c out-stream)))

      ;; Keep track of whitespace in case we hit a newline
      (unless (member c '(#\space #\tab) :test #'char=)
	(setf ws nil)))))

      
