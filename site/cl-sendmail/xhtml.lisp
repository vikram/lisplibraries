;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; xhtml.lisp: Generate HTML emails with text alternatives and images
;;;; Copyright (C) 2006 Robert Marlow <bobstopper@bobturf.org>
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


(in-package :sendmail)


(defun build-xhtml-email (mail-output-stream)
  "The CONTENT slot of MAIL-OUTPUT-STREAM is expected to be an XML 
structure comparable to what XMLS would output after parsing XML. The
content is parsed for any images in file locations which are loaded,
encoded and turned into MIME attachments. The XML is modified to have 
the image source point to the content ID of the new attachment. The XML
itself is converted into a text equivalent which is put in a text/plain 
MIME part while the XML itself is output as XHTML in a text/html MIME 
part.

Note: the process of attaching images is destructive to the XML structure.
If you want to keep your XML structure intact make a copy first."
  (if (content mail-output-stream)
      (with-parser *xhtml-email-parser*
	(with-state (:images nil)
	  (process-xml (content mail-output-stream))
	  (let* ((images (cdr (assoc :images state)))
		 (text-mime (make-instance
			     'text-mime
			     :subtype "plain"
			     :encoding :quoted-printable
			     :content (xhtml-to-text
				       (content mail-output-stream))))
		 (html-mime (make-instance
			     'text-mime
			     :subtype "html"
			     :encoding :quoted-printable
			     :content (format nil
					      "<?xml version=\"1.0\" encoding=\"us-ascii\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
  \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">~%~A"
					      (toxml (content mail-output-stream))))))

	    (change-class mail-output-stream
			  'multipart-mail-output-stream
			  :type "multipart"
			  :subtype (if images
				       "related"
				       "alternative")
			  :content
			  (if images
			      `(,(make-instance
				  'multipart-mime
				  :subtype "alternative"
				  :content `(,text-mime
					     ,html-mime))
				,@images)
			      `(,text-mime ,html-mime))))))))


(defparameter *xhtml-email-parser* (make-xml-parser))


(defmacro descend ()
  `(dolist (child (node-children tree))
    (when (listp child)
      (process-xml child))))

(def-element-handler "html" (tree) (*xhtml-email-parser*)
  (descend))

(def-element-handler "body" (tree) (*xhtml-email-parser*)
  (descend))

(def-element-handler "p" (tree) (*xhtml-email-parser*)
  (descend))

(def-element-handler "a" (tree) (*xhtml-email-parser*)
  (descend))

(def-element-handler "div" (tree) (*xhtml-email-parser*)
  (descend))

(def-element-handler "div" (tree) (*xhtml-email-parser*)
  (descend))

(def-element-handler "span" (tree) (*xhtml-email-parser*)
  (descend))

(def-element-handler "blockquote" (tree) (*xhtml-email-parser*)
  (descend))

(def-element-handler "ul" (tree) (*xhtml-email-parser*)
  (descend))

(def-element-handler "ol" (tree) (*xhtml-email-parser*)
  (descend))

(def-element-handler "li" (tree) (*xhtml-email-parser*)
  (descend))

(def-element-handler "table" (tree) (*xhtml-email-parser*)
  (descend))

(def-element-handler "tr" (tree) (*xhtml-email-parser*)
  (descend))

(def-element-handler "td" (tree) (*xhtml-email-parser*)
  (descend))

(def-element-handler "th" (tree) (*xhtml-email-parser*)
  (descend))
  

(def-element-handler "img" (tree) (*xhtml-email-parser*)
  (let* ((src (assoc "src" (node-attrs tree) :test #'equal))
	 (filename (url-filename (second src))))

    (when (string-equal (get-protocol (second src)) "file")
      (multiple-value-bind
	    (type subtype)
	  (lookup-mime filename)
	(let ((mime (make-instance 'mime
				   :encoding :base64
				   :content-encoding :base64
				   :type type
				   :subtype subtype
				   :id (make-content-id)
				   :parameters `((:name ,filename))
				   :content (retrieve-file (second src)))))
	  (push mime (cdr (assoc :images xmls-tools::state)))
	  
	  (setf (second src) (format nil "cid:~A" (content-id mime))))))))


(defun retrieve-file (url)
  "Reads the file located at URL and converts it to base64"
  (let ((protocol (get-protocol url)))
    (cond
      ((string-equal protocol "file")
       (usb8-array-to-base64-string (read-file (url-path url))
				    :columns 75))
      (t (error "Unsupported Protocol ~A" protocol)))))


(defun get-protocol (url &optional (string ""))
  "Obtains the protocol part of URL"
  (if (= (length url) 0)
      nil
      (if (char= (char url 0) #\:)
	  string
	  (get-protocol (subseq url 1)
			(concatenate 'string string
				     (string (char url 0)))))))


(defun url-path (url &optional path-start)
  "Obtains the path part of URL"
  (cond
    ((= (length url) 0)
     nil)
    ((char= (char url 0) #\/)
     (url-path (subseq url 1) t))
    (path-start
     (concatenate 'string "/" url))
    (t
     (url-path (subseq url 1)))))


(defun url-filename (url &optional (string ""))
  "Obtains the filename part of URL"
  (if (= (length url) 0)
      string
      (url-filename (subseq url 1)
		    (if (char= (char url 0) #\/)
			""
			(concatenate 'string 
				     string
				     (string (char url 0)))))))
