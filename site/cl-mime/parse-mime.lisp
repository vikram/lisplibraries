;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; parse-mime.lisp: Tools for parsing a mime string/stream 
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

(defgeneric parse-mime (mime &optional headers)
  (:documentation
   "Parse a string or stream containing a MIME message and return a mine
object representing it or nil if the message is not MIME compatible"))


(defmethod parse-mime ((mime string) &optional headers)
  (parse-mime (make-string-input-stream mime) headers))


(defmethod parse-mime ((mime stream) &optional headers)
  (catch 'bad-mime
    (let* ((headers (or headers (parse-headers mime)))
	   (content-type-header (assoc :content-type headers))
	   (content-disposition-header (assoc :content-disposition headers))
	   (content-type-subtype (split "/" (header-value
					     content-type-header)))
	   (content-type (first content-type-subtype))
	   (content-subtype (second content-type-subtype))
	   (content-parm (header-parms content-type-header))
	   (content-disposition (header-value content-disposition-header))
	   (content-disposition-parm (when content-disposition-header
				       (header-parms
					content-disposition-header)))
	   (boundary nil)
	   (mime-version (or (header-value (assoc :mime-version headers)) "1.0"))
	   (mime-type (cond
		       ((equal content-type "text") 'text-mime)
		       ((equal content-type "multipart") 'multipart-mime)
		       (t 'mime))))

      (if (equal mime-version "1.0")
	
	  (let* ((encoding (intern (or (string-upcase 
					(cdr (assoc :content-transfer-encoding
						    headers)))
				       "7BIT")
				   :keyword))
		 (mime-obj-gen
		  (list
		   mime-type
		   :type content-type
		   :subtype content-subtype
		   :encoding encoding
		   :content-encoding encoding
		   :description (cdr (assoc :content-description
					    headers))
		   :id (remove #\< (remove #\> (cdr (assoc :content-id headers))))
		   :disposition content-disposition
		   :disposition-parameters content-disposition-parm)))
	      
	    (case mime-type
	      ((text-mime)
	       (setq mime-obj-gen
		     (append mime-obj-gen
			     (list
			      :charset (cdr (assoc :charset content-parm))
			      :parameters (delete (assoc :charset content-parm)
						  content-parm)))))
	      ((multipart-mime)
	       (setq boundary (second (assoc :boundary content-parm)))
	       (setq mime-obj-gen 
		     (append mime-obj-gen
			     (list
			      :boundary boundary
			      :parameters (delete (assoc :boundary content-parm)
						  content-parm)
			      :prologue (get-prologue mime boundary)))))
						    
	      (t (setq mime-obj-gen
		       (append mime-obj-gen (list :parameters content-parm)))))

	    (setq mime-obj-gen
		  (append mime-obj-gen
			  (list :content (parse-body mime
						     (ensure-keyword mime-type)
						     boundary))))

	    (case mime-type
	      ((multipart-mime)
	       (setq mime-obj-gen
		     (append mime-obj-gen
			     (list :epilogue (get-epilogue mime))))))

	    (apply #'make-instance mime-obj-gen))

	;; If we decide this isn't MIME 1.0 compatible, we just return nil.
	nil))))

(defun parse-headers (stream)
  "Parses headers from a stream and converts them into keyword/value pairs"
  (let ((headers nil)
	(previous-line nil))
	
    (read-lines (line stream)
		((equal line "")
		 (if previous-line
		     (push (create-header previous-line) headers))
		 headers)
				 
		;; Headers beginning with whitespace are continuations
		;; from the header on the previous line. Headers not
		;; beginning with complete lines are starts of new headers
		(unless 
		    (register-groups-bind
		     (next-line)
		     ("^\\s+(.+)" line)
		     
		     (setq previous-line
			   (format nil "~A ~A" previous-line next-line)))
		  (if previous-line
		      (push (create-header previous-line) headers))
		  (setq previous-line line)))))


(defun header-value (header)
  "Takes a header cons and returns the value component"
  (register-groups-bind (value) ("^([^;\\s]*)" (cdr header)) value))


(defun header-parms (header)
  "Takes a header cons and returns all parameters contained within"
  (extract-parms
   (regex-replace-all "\\(.*?\\)"
		      (or (register-groups-bind
			   (params)
			   ("^[^;\\s]*(;.*)$" (cdr header))
			   
			   params)
			  (return-from header-parms nil))
		      "")))


(defun header-comments (header)
  "Returns all comments from the keyword/value header pair in HEADER"
  (extract-header-comments (cdr header)))


(defun extract-header-comments (header-value-string &optional comment-list)
  "Takes a header string and optional list of already extracted comments and
returns all comments contained within that string"
  (if (register-groups-bind
       (comment rest)
       ("\\((.*?)\\)(.*)" header-value-string)
       
       (setq header-value-string rest)
       (setq comment-list (cons comment comment-list)))

      (extract-header-comments header-value-string comment-list)
    (reverse comment-list)))

			       
(defun create-header (header-string)
  "Takes a header string and returns a keyword/value header pair"
  (register-groups-bind
   (header-name header-value)
   ("^([^:\\s]+):\\s*(.*)$" header-string)
   
   (cons (ensure-keyword header-name)
	 header-value)))


(defun extract-parms (parm-string &optional parms)
  "Takes a string of parameters and returns a list of keyword/value
parameter pairs"
  (if (register-groups-bind
       (parm-name parm-value rest)
       (";\\s*(.*?)=\"?([^;\"\\s]*)\"?[\\s]*(;?.*)" parm-string)
       
       (setq parm-string rest)
       (setq parms (cons (list (ensure-keyword parm-name) parm-value)
			 parms)))
  
      (extract-parms parm-string parms)
    parms))


(defgeneric parse-body (body mime-type &optional boundary)
  (:documentation
   "Parses a mime body within the context of the mime type expected.
Assumes the stream's position is already at the body. If it's not,
you should call parse headers first or read through to the first null
line."))


(defmethod parse-body ((body string) (mime-type (eql :mime))
		       &optional boundary)
  (declare (ignore boundary))
  body)


(defmethod parse-body ((body stream) (mime-type (eql :mime))
		       &optional boundary)
  (declare (ignore boundary))
  (read-stream-to-string body line))


(defmethod parse-body ((body string) (mime-type (eql :text-mime))
		       &optional boundary)
  (declare (ignore boundary))
  body)


(defmethod parse-body ((body stream) (mime-type (eql :text-mime))
		       &optional boundary)
  (declare (ignore boundary))
  (read-stream-to-string body line))


(defmethod parse-body ((body string) (mime-type (eql :multipart-mime))
		       &optional boundary)
  (parse-body (make-string-input-stream body) mime-type boundary))


(defmethod parse-body ((body stream) (mime-type (eql :multipart-mime))
		       &optional boundary)
  (multiple-value-bind (part end-type) (read-until-boundary body boundary)
    (cons (parse-mime part)
	  (case end-type
	    ((end-part) (parse-body body mime-type boundary))
	    ((end-mime) nil)
	    ((eof) (throw 'bad-mime nil))
	    (t (throw 'bad-mime "Unexpected Parse Return Value"))))))
    

(defgeneric get-prologue (body boundary)
  (:documentation "Grab the prologue from a Multipart MIME message"))


(defmethod get-prologue ((body string) boundary)
  (get-prologue (make-string-input-stream body) boundary))


(defmethod get-prologue ((body stream) boundary)
  (multiple-value-bind (text end-type) (read-until-boundary body boundary)
    (case end-type
      ((end-part) text)
      ((end-mime eof) (throw 'bad-mime nil))
      (t (throw 'bad-mime "Unexpected Parse Return Value")))))


(defgeneric get-epilogue (body)
  (:documentation "Grab the prologue from a Multipart MIME message"))


(defmethod get-epilogue ((body string))
  (read-stream-to-string (make-string-input-stream body) line))


(defmethod get-epilogue ((body stream))
  (read-stream-to-string body line))
      

(defun read-until-boundary (stream boundary)
  "Reads a MIME body from STREAM until it reaches a boundary defined by
BOUNDARY"
  (let ((end-type 'eof)
	(actual-boundary (concatenate 'string "--" boundary)))
    (values
     (read-stream-to-string
      stream line
      
      (or (and (equal (delete #\return line) actual-boundary)
	       (setq end-type 'end-part))
	  (and (equal (delete #\return line) (concatenate 'string
							  actual-boundary
							  "--"))
	       (setq end-type 'end-mime))))
     end-type)))
 

(defparameter *mime-types-file* 
  (make-pathname :directory '(:absolute "etc")
		 :name "mime"
		 :type "types"))


(defun lookup-mime (pathname &optional mime-types-file)
  "Takes a PATHNAME argument and uses MIME-TYPES-FILE (or the system 
default) to determine the mime type of PATHNAME. Returns two values:
the content type and the the content subtype"
  (let ((extension (pathname-type pathname)))
    (with-open-file
	(mime (or mime-types-file *mime-types-file*) :direction :input)
      (read-lines
	  (line mime)
	  ((register-groups-bind
	       (extensions)
	       ("^[^#\\s]+\\s+([^#]+)" line)
	     (find extension (split "\\s+" extensions)
		   :test #'string-equal))
	   (if (eq line 'eof)
	       (values "application" "octet-stream")
	       (register-groups-bind
		   (content-type content-subtype)
		   ("^([^\/]+)\/([^\\s]+)" line)
		 (values (or content-type "application")
			 (or content-subtype "octet-stream")))))))))
