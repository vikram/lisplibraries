;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; xhtml2text.lisp: Example program - converts xhtml to text
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


(defparameter *xhtml-to-text-parser* (make-xml-parser))


(defmacro handle-maybe-text-children (tree out-stream)
  `(dolist (child (node-children ,tree))
    (typecase child
      (string (princ (compress-whitespace child) ,out-stream))
      (list (process-xml child ,out-stream)))))


(defmacro handle-non-text-children (tree out-stream)
  `(dolist (child (node-children ,tree))
    (when (listp child)
      (process-xml child ,out-stream))))


(def-element-handler "html" (tree out-stream) (*xhtml-to-text-parser*)
  (handle-non-text-children tree out-stream))


(def-element-handler "body" (tree out-stream) (*xhtml-to-text-parser*)
  (handle-non-text-children tree out-stream))


(def-element-handler "div" (tree out-stream) (*xhtml-to-text-parser*)
  (handle-non-text-children tree out-stream))


(def-element-handler "scan" (tree out-stream) (*xhtml-to-text-parser*)
  (handle-maybe-text-children tree out-stream))


(def-element-handler "p" (tree out-stream) (*xhtml-to-text-parser*)
  (fresh-line out-stream)
  (handle-maybe-text-children tree out-stream)
  (fresh-line out-stream)
  (terpri out-stream))


(def-element-handler "a" (tree out-stream) (*xhtml-to-text-parser*)
  (handle-maybe-text-children tree out-stream))


(def-element-handler "h1" (tree out-stream) (*xhtml-to-text-parser*)
  (let ((new-stream (make-string-output-stream)))
    (handle-maybe-text-children tree new-stream)
    (let ((text (get-output-stream-string new-stream)))
      (format out-stream "~A~&~A~%~%"
	      text
	      (make-string (length text) :initial-element #\*)))))


(def-element-handler "h2" (tree out-stream) (*xhtml-to-text-parser*)
  (let ((new-stream (make-string-output-stream)))
    (handle-maybe-text-children tree new-stream)
    (let ((text (get-output-stream-string new-stream)))
      (format out-stream "~A~&~A~%~%"
	      text
	      (make-string (length text) :initial-element #\=)))))


(def-element-handler "h3" (tree out-stream) (*xhtml-to-text-parser*)
  (let ((new-stream (make-string-output-stream)))
    (handle-maybe-text-children tree new-stream)
    (let ((text (get-output-stream-string new-stream)))
      (format out-stream "~A~&~A~%~%"
	      text
	      (make-string (length text) :initial-element #\-)))))


(def-element-handler "h4" (tree out-stream) (*xhtml-to-text-parser*)
  (let ((new-stream (make-string-output-stream)))
    (handle-maybe-text-children tree new-stream)
    (let ((text (get-output-stream-string new-stream)))
      (format out-stream "~A~&~A~%~%"
	      text
	      (make-string (length text) :initial-element #\`)))))


(def-element-handler "h5" (tree out-stream) (*xhtml-to-text-parser*)
  (let ((new-stream (make-string-output-stream)))
    (handle-maybe-text-children tree new-stream)
    (let ((text (get-output-stream-string new-stream)))
      (format out-stream "~A~&~A~%~%"
	      text
	      (make-string (length text) :initial-element #\')))))


(def-element-handler "h6" (tree out-stream) (*xhtml-to-text-parser*)
  (princ "**" out-stream)
  (handle-maybe-text-children tree out-stream)
  (format out-stream "**~%~%"))


(def-element-handler "span" (tree out-stream) (*xhtml-to-text-parser*)
  (handle-maybe-text-children tree out-stream))


(def-element-handler "hr" (tree out-stream) (*xhtml-to-text-parser*)
  (format out-stream "~&     ~A~%"
	  (make-string 70 :initial-element #\*)))


(def-element-handler "ul" (tree out-stream) (*xhtml-to-text-parser*)
  (fresh-line out-stream)
  (with-indent out-stream
    (with-state (:ul t)
      (handle-non-text-children tree out-stream)))
  (fresh-line out-stream)
  (terpri out-stream))


(def-element-handler "ol" (tree out-stream) (*xhtml-to-text-parser*)
  (fresh-line out-stream)
  (with-indent out-stream
    (with-state (:ol 0)
      (handle-non-text-children tree out-stream)))
  (fresh-line out-stream)
  (terpri out-stream))


(def-element-handler "li" (tree out-stream) (*xhtml-to-text-parser*)
  (let ((list-state (assoc '(:ol :ul) state
			   :test (lambda (item-list state-item)
				   (find state-item item-list)))))
    (format out-stream "~A "
	    (case (car list-state)
	      ((:ul) #\*)
	      ((:ol) (format nil "~A." (incf (cdr list-state))))))
    (handle-maybe-text-children tree out-stream)
    (terpri out-stream)))


(def-element-handler "img" (tree out-stream) (*xhtml-to-text-parser*)
  (format out-stream "[~A]"
	  (compress-whitespace (or (get-attr-value "alt" tree) ""))))


(def-element-handler "br" (tree out-stream) (*xhtml-to-text-parser*)
  (fresh-line out-stream))


(defun text-lines (string)
  (let ((lines-list '())
	(string-buffer (make-string (length string)))
	(width 0))
    (flet ((push-line (j)
	     (let ((line (subseq string-buffer 0 j)))
	     (push line lines-list)
	     (setf string-buffer (subseq string-buffer j))
	     (when (> (length line) width)
	       (setf width (length line))))))
    (do ((i 0 (1+ i))
	 (j 0 (1+ j)))
	((>= i (length string))
	 (push-line j)
	 (let ((lines (apply #'vector (nreverse lines-list))))
	   (values lines width (length lines))))
      (case (char string i)
	((#\newline)
	 (push-line j)
	 (setf j -1))
	((#\return) (decf j))
	(otherwise
	 (setf (char string-buffer j) (char string i))))))))


(def-element-handler "table" (tree out-stream) (*xhtml-to-text-parser*)
  (with-state (:table nil)
    (with-state (:column 0)
      (with-state (:row 0)

	;; Process children
	(handle-non-text-children tree out-stream)
  
	;; Handle cells
	(let* ((columns (cdr (assoc :column state)))
	       (rows (cdr (assoc :row state)))
	       (row-heights (make-array columns
					:element-type 'fixnum
					:initial-element 0))
	       (col-widths (make-array rows
				       :element-type 'fixnum
				       :initial-element 0))
	       (table (make-array `(,columns ,rows))))
	  ;; Split cells by lines and record maximum dimensions
	  (mapc (lambda (cell)
		  (multiple-value-bind
			(lines width height)
		      (text-lines (third cell))
		    ;; We increment width to give padding between cells
		    (when (> (1+ width) (aref col-widths (second cell)))
		      (setf (aref col-widths (second cell)) (1+ width)))
		    (when (> height (aref row-heights (first cell)))
		      (setf (aref row-heights (first cell)) height))
		    (setf (aref table (first cell) (second cell))
			  lines)))
		(cdr (assoc :table state)))

	  ;; print the cells paying attention to cell dimensions
	  (dotimes (i rows)
	    (dotimes (j (aref row-heights i))
	      (dotimes (k columns)
		(let* ((cell (aref table i k))
		       (line (if (< j (length cell))
				 (aref cell j)
				 "")))
		  (princ line out-stream)
		  (princ (make-string (- (aref col-widths k)
					 (length line))
				      :initial-element #\space)
			 out-stream)))
	      (fresh-line out-stream)))))))
  
  (fresh-line out-stream)
  (terpri out-stream))


;;; need rowspan / colspan stuff too
	      
(def-element-handler "tr" (tree out-stream) (*xhtml-to-text-parser*)
  (fresh-line out-stream)
  (setf (cdr (assoc :column state)) 0)
  (handle-non-text-children tree out-stream)
  (incf (cdr (assoc :row state))))


(def-element-handler "td" (tree out-stream) (*xhtml-to-text-parser*)
  (let ((cell-stream (make-string-output-stream))
	(column (assoc :column state))
	(row (assoc :row state))
	(table (assoc :table state)))

    (handle-maybe-text-children tree cell-stream)
    
    (push (list (cdr row) (cdr column)
		(get-output-stream-string cell-stream))
	  (cdr table))
    (incf (cdr column))))


(def-element-handler "th" (tree out-stream) (*xhtml-to-text-parser*)
  (let ((cell-stream (make-string-output-stream))
	(column (assoc :column state))
	(row (assoc :row state))
	(table (assoc :table state)))

    (handle-maybe-text-children tree cell-stream)
    
    (push (list (cdr row) (cdr column)
		(get-output-stream-string cell-stream))
	  (cdr table))
    (incf (cdr column))))


(def-element-handler "blockquote" (tree out-stream) (*xhtml-to-text-parser*)
  (fresh-line out-stream)
  (with-indent out-stream
    (handle-maybe-text-children tree out-stream)
    (fresh-line out-stream)
    (terpri out-stream)))


(def-element-handler "dl" (tree out-stream) (*xhtml-to-text-parser*)
  (fresh-line out-stream)
  (handle-maybe-text-children tree out-stream)
  (fresh-line out-stream)
  (terpri out-stream))


(def-element-handler "dt" (tree out-stream) (*xhtml-to-text-parser*)
  (fresh-line out-stream)
  (handle-maybe-text-children tree out-stream))


(def-element-handler "dd" (tree out-stream) (*xhtml-to-text-parser*)
  (fresh-line out-stream)
  (with-indent out-stream
    (handle-maybe-text-children tree out-stream)))


(def-element-handler "pre" (tree out-stream) (*xhtml-to-text-parser*)
  (with-state (:compress-whitespace nil)
    (handle-maybe-text-children tree out-stream)))


(defun xhtml-to-text (xml)
  (let ((out-stream (make-string-output-stream))
	(*strip-comments* nil))
    (with-parser *xhtml-to-text-parser*
      (with-state (:compress-whitespace t)
	(process-xml
	 (typecase xml
	   (string (parse xml :compress-whitespace nil))
	   (list xml))
	 out-stream)))
    (get-output-stream-string out-stream)))