;;; -*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: kmrcl -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          byte-stream.lisp
;;;; Purpose:       Byte array input/output streams
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  June 2003
;;;;
;;;; $Id: byte-stream.lisp 10390 2005-04-06 17:40:37Z kevin $
;;;;
;;;; Works for CMUCL, SBCL, and AllergoCL only
;;;;
;;;; This file, part of KMRCL, is Copyright (c) 2003 by Kevin M. Rosenberg
;;;; and by onShore Development, Inc.
;;;;
;;;; KMRCL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:kmrcl)

;; Intial CMUCL version by OnShored. Ported to SBCL by Kevin Rosenberg

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (sb-ext:without-package-locks
	    (sb-pcl::structure-class-p
	     (find-class (intern "FILE-STREAM" "SB-IMPL"))))
    (push :old-sb-file-stream cl:*features*)))

#+(or cmu sbcl)
(progn
(defstruct (byte-array-output-stream
             (:include #+cmu system:lisp-stream
		       #+(and sbcl old-sb-file-stream) sb-impl::file-stream
		       #+(and sbcl (not old-sb-file-stream)) sb-sys:fd-stream
                       (bout #'byte-array-bout)
                       (misc #'byte-array-out-misc))
             (:print-function %print-byte-array-output-stream)
             (:constructor make-byte-array-output-stream ()))
  ;; The buffer we throw stuff in.
  (buffer (make-array 128 :element-type '(unsigned-byte 8)))
  ;; Index of the next location to use.
  (index 0 :type fixnum))

(defun %print-byte-array-output-stream (s stream d)
  (declare (ignore s d))
  (write-string "#<Byte-Array-Output Stream>" stream))

(setf (documentation 'make-binary-output-stream 'function)
  "Returns an Output stream which will accumulate all output given it for
   the benefit of the function Get-Output-Stream-Data.")

(defun byte-array-bout (stream byte)
  (let ((current (byte-array-output-stream-index stream))
	(workspace (byte-array-output-stream-buffer stream)))
    (if (= current (length workspace))
	(let ((new-workspace (make-array (* current 2) :element-type '(unsigned-byte 8))))
	  (replace new-workspace workspace)
	  (setf (aref new-workspace current) byte)
	  (setf (byte-array-output-stream-buffer stream) new-workspace))
	(setf (aref workspace current) byte))
    (setf (byte-array-output-stream-index stream) (1+ current))))

(defun byte-array-out-misc (stream operation &optional arg1 arg2)
  (declare (ignore arg2))
  (case operation
    (:file-position
     (if (null arg1)
	 (byte-array-output-stream-index stream)))
    (:element-type '(unsigned-byte 8))))

(defun get-output-stream-data (stream)
  "Returns an array of all data sent to a stream made by
Make-Byte-Array-Output-Stream since the last call to this function and
clears buffer."
  (declare (type byte-array-output-stream stream))
    (prog1 
	(dump-output-stream-data stream)
      (setf (byte-array-output-stream-index stream) 0)))

(defun dump-output-stream-data (stream)
  "Returns an array of all data sent to a stream made by
Make-Byte-Array-Output-Stream since the last call to this function."
  (declare (type byte-array-output-stream stream))
  (let* ((length (byte-array-output-stream-index stream))
	 (result (make-array length :element-type '(unsigned-byte 8))))
    (replace result (byte-array-output-stream-buffer stream))
    result))

) ; progn


#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-ext:without-package-locks
      (defvar *system-copy-fn* (if (fboundp (intern "COPY-SYSTEM-AREA" "SB-KERNEL"))
				   (intern "COPY-SYSTEM-AREA" "SB-KERNEL")
				   (intern "COPY-SYSTEM-UB8-AREA" "SB-KERNEL")))
    (defconstant +system-copy-multiplier+ (if (fboundp (intern "COPY-FROM-SYSTEM-AREA" "SB-KERNEL"))
					      sb-vm:n-byte-bits
					 1))))
  
#+(or cmu sbcl)
(progn
  (defstruct (byte-array-input-stream
	     (:include #+cmu system:lisp-stream
		       ;;#+sbcl sb-impl::file-stream
		       #+(and sbcl old-sb-file-stream) sb-impl::file-stream
		       #+(and sbcl (not old-sb-file-stream)) sb-sys:fd-stream
		       (in #'byte-array-inch)
		       (bin #'byte-array-binch)
		       (n-bin #'byte-array-stream-read-n-bytes)
		       (misc #'byte-array-in-misc))
	     (:print-function %print-byte-array-input-stream)
					;(:constructor nil)
	     (:constructor internal-make-byte-array-input-stream
			   (byte-array current end)))
  (byte-array nil :type vector)
  (current nil)
  (end nil))

  
(defun %print-byte-array-input-stream (s stream d)
  (declare (ignore s d))
  (write-string "#<Byte-Array-Input Stream>" stream))

(defun byte-array-inch (stream eof-errorp eof-value)
  (let ((byte-array (byte-array-input-stream-byte-array stream))
	(index (byte-array-input-stream-current stream)))
    (cond ((= index (byte-array-input-stream-end stream))
	   #+cmu
	   (eof-or-lose stream eof-errorp eof-value)
	   #+sbcl
	   (sb-impl::eof-or-lose stream eof-errorp eof-value)
	   )
	  (t
	   (setf (byte-array-input-stream-current stream) (1+ index))
	   (aref byte-array index)))))

(defun byte-array-binch (stream eof-errorp eof-value)
  (let ((byte-array (byte-array-input-stream-byte-array stream))
	(index (byte-array-input-stream-current stream)))
    (cond ((= index (byte-array-input-stream-end stream))
	   #+cmu
	   (eof-or-lose stream eof-errorp eof-value)
	   #+sbcl
	   (sb-impl::eof-or-lose stream eof-errorp eof-value)
	   )
	  (t
	   (setf (byte-array-input-stream-current stream) (1+ index))
	   (aref byte-array index)))))

(defun byte-array-stream-read-n-bytes (stream buffer start requested eof-errorp)
  (declare (type byte-array-input-stream stream))
  (let* ((byte-array (byte-array-input-stream-byte-array stream))
	 (index (byte-array-input-stream-current stream))
	 (available (- (byte-array-input-stream-end stream) index))
	 (copy (min available requested)))
    (when (plusp copy)
      (setf (byte-array-input-stream-current stream)
	(+ index copy))
      #+cmu
      (system:without-gcing
       (system::system-area-copy (system:vector-sap byte-array)
			 (* index vm:byte-bits)
			 (if (typep buffer 'system::system-area-pointer)
			     buffer
			     (system:vector-sap buffer))
			 (* start vm:byte-bits)
			 (* copy vm:byte-bits)))
      #+sbcl
      (sb-sys:without-gcing
       (funcall *system-copy-fn* (sb-sys:vector-sap byte-array)
			 (* index +system-copy-multiplier+)
			 (if (typep buffer 'sb-sys::system-area-pointer)
			     buffer
			     (sb-sys:vector-sap buffer))
			 (* start +system-copy-multiplier+)
			 (* copy +system-copy-multiplier+))))
    (if (and (> requested copy) eof-errorp)
	(error 'end-of-file :stream stream)
	copy)))

(defun byte-array-in-misc (stream operation &optional arg1 arg2)
  (declare (ignore arg2))
  (case operation
    (:file-position
     (if arg1
	 (setf (byte-array-input-stream-current stream) arg1)
	 (byte-array-input-stream-current stream)))
    (:file-length (length (byte-array-input-stream-byte-array stream)))
    (:unread (decf (byte-array-input-stream-current stream)))
    (:listen (or (/= (the fixnum (byte-array-input-stream-current stream))
		     (the fixnum (byte-array-input-stream-end stream)))
		 :eof))
    (:element-type 'base-char)))
  
(defun make-byte-array-input-stream (buffer &optional (start 0) (end (length buffer)))
  "Returns an input stream which will supply the bytes of BUFFER between
  Start and End in order."
  (internal-make-byte-array-input-stream buffer start end))

) ;; progn

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setq cl:*features* (delete :old-sb-file-stream cl:*features*)))

;;; Simple streams implementation by Kevin Rosenberg

#+allegro
(progn

  (defclass extendable-buffer-output-stream (excl:buffer-output-simple-stream)
    ()
    )

  (defun make-byte-array-output-stream ()
    "Returns an Output stream which will accumulate all output given it for
   the benefit of the function Get-Output-Stream-Data."
    (make-instance 'extendable-buffer-output-stream
      :buffer (make-array 128 :element-type '(unsigned-byte 8))
      :external-form :octets))

  (defun get-output-stream-data (stream)
    "Returns an array of all data sent to a stream made by
Make-Byte-Array-Output-Stream since the last call to this function
and clears buffer."
    (prog1 
	(dump-output-stream-data stream)
      (file-position stream 0)))
  
  (defun dump-output-stream-data (stream)
    "Returns an array of all data sent to a stream made by
Make-Byte-Array-Output-Stream since the last call to this function."
    (force-output stream)
    (let* ((length (file-position stream))
	   (result (make-array length :element-type '(unsigned-byte 8))))
      (replace result (slot-value stream 'excl::buffer))
      result))
  
  (defmethod excl:device-extend ((stream extendable-buffer-output-stream)
				 need action)
    (declare (ignore action))
    (let* ((len (file-position stream))
	   (new-len (max (+ len need) (* 2 len)))
	   (old-buf (slot-value stream 'excl::buffer))
	   (new-buf (make-array new-len :element-type '(unsigned-byte 8))))
      (declare (fixnum len)
	       (optimize (speed 3) (safety 0)))
      (dotimes (i len)
	(setf (aref new-buf i) (aref old-buf i)))
      (setf (slot-value stream 'excl::buffer) new-buf)
      (setf (slot-value stream 'excl::buffer-ptr) new-len)
      )
    t)
  
)

#+allegro
(progn
  (defun make-byte-array-input-stream (buffer &optional (start 0)
							(end (length buffer)))
    (excl:make-buffer-input-stream buffer start end :octets))
  ) ;; progn


