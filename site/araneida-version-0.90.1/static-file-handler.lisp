(in-package :araneida)

;;; XXX Should abstract this somehow so that it can be done on things
;;; other than filename suffix

(defvar *content-types*
  '(("html" "text/html")
    ("gif" "image/gif")
    ("jpg" "image/jpeg")
    ("png" "image/png")
    ("css" "text/css")
    ("class" "application/octet-stream")
    ("doc" "application/octet-stream")
    ("zip" "application/octet-stream")
    ("gz" "application/octet-stream")
    ("ASF" "video/x-ms-asf")
    ("tar" "application/octet-stream")
    ("avi" "video/x-msvideo")
    ("txt" "text/plain")))

(defun read-mime-types (filename)
  "Read a standard-format mime.types file and return an alist suitable for
assigning to *content-types*"
  (labels ((chop-comment (string)
                         (subseq string 0 (position #\# string)))
           (collect-extns (type extns)
                          (loop for e in (split extns)
				if (> (length e) 0)
                                collect (list e type))))
    (with-open-file (in filename :direction :input)
      (let ((eof (gensym)))
        (loop for line = (read-line in nil eof)
              until (eq line eof)
              for (type extns) =  (remove-if-empty
				   (split (chop-comment line) 2))
              append (collect-extns type extns))))))

(defun copy-stream (from to)
  "Copy into TO from FROM until end of the input file.  Do not
translate or otherwise maul anything."
; We used to catch sequence type mismatches, but given bivalent streams these days....
  (let ((buf (make-array 4096 :element-type (stream-element-type from))))
    (do ((pos (read-sequence buf from) (read-sequence buf from)))
        ((= 0 pos) nil)      
      (write-sequence buf to :end pos))))

;; a host lisp compatibility file can override this to set the
;; appropriate external format for reading in files to send with
;; send-file
(defvar *open-external-format-arguments* nil)

(defun send-file (r file-name &key content-type)
  (let ((stream (request-stream r))
	(content-type 
	 (or content-type
	     (cadr (assoc (or (pathname-type file-name) "txt") *content-types*
			  :test #'string=))))
	(in (apply #'open file-name :direction :input
		   *open-external-format-arguments*)))
    (unwind-protect
	 (progn
	   (request-send-headers r :content-type content-type
				 :conditional t
				 :content-length (file-length in)
				 :last-modified (file-write-date in))
	   (copy-stream in stream))
      (close in))))

(defclass static-file-handler (handler)
  ((pathname :initarg :pathname :accessor static-file-pathname
	     :documentation "Root pathname for URI components to merge against.  Requests may not be made outside this hierarchy")
   (default-name :initarg :default-name :accessor static-file-default-name
		 :initform "index.html")))

(defmethod handle-request-response
    ((handler static-file-handler) method request)
  ;; chop arg-string into /-delimited components.
  ;; remove .. components along with the component preceding them
  (let* ((path (cons :relative
		     (loop for p on
			   (nreverse (split
				      (request-unhandled-part request)
				      nil "/"))
			   if (string-equal (car p) "..")
			   do (setf p (cdr p))
			   else collect (car p) into v
			   finally (return (nreverse v)))))
	 (name (let ((n  (car (last path))))
		 (if (> (length n) 0) n nil)))
	 (path (butlast path))
	 (dot-pos (and name (position #\. name :from-end t)))
	 (extension (and dot-pos (subseq name (1+ dot-pos))))
	 (name (urlstring-unescape (if dot-pos  (subseq name 0  dot-pos) name)))
	 (file (make-pathname :name name :directory path :type extension))
	 (fnam (merge-pathnames file (static-file-pathname handler))))
    (when (and (pathname-name fnam)
	       (probe-file fnam)
	       (not (pathname-name (truename fnam))))
      (request-redirect request
			(concatenate 'string (request-urlstring request) "/"))
      (return-from handle-request-response t))
    (when (not (pathname-name fnam))
      (setf fnam (merge-pathnames fnam (static-file-default-name handler))))
    (with-file-error-handlers
        (progn
	  (send-file request fnam)
	  t)
      (format nil "Can't read ~S: ~A~%" fnam c))))
