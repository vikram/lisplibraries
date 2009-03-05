;;; Copyright (C) 2001, 2003  Eric Marsden
;;; Copyright (C) 2005  David Lichteblau
;;; "the conditions and ENSURE-SSL-FUNCALL are by Jochen Schmidt."
;;;
;;; See LICENSE for details.

(declaim
 (optimize (speed 3) (space 1) (safety 1) (debug 0) (compilation-speed 0)))

(in-package :cl+ssl)

(defconstant +initial-buffer-size+ 2048)

(defclass ssl-stream
    (fundamental-binary-input-stream
     fundamental-binary-output-stream 
     trivial-gray-stream-mixin)
  ((ssl-stream-socket
    :initarg :socket
    :accessor ssl-stream-socket)
   (handle
    :initform nil
    :accessor ssl-stream-handle)
   (output-buffer
    :initform (cffi-sys::make-shareable-byte-vector +initial-buffer-size+)
    :accessor ssl-stream-output-buffer)
   (output-pointer
    :initform 0
    :accessor ssl-stream-output-pointer)
   (input-buffer
    :initform (cffi-sys::make-shareable-byte-vector +initial-buffer-size+)
    :accessor ssl-stream-input-buffer)
   (peeked-byte
    :initform nil
    :accessor ssl-stream-peeked-byte)))

(defmethod print-object ((object ssl-stream) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "for ~A" (ssl-stream-socket object))))

(defclass ssl-server-stream (ssl-stream) 
  ((certificate
    :initarg :certificate
    :accessor ssl-stream-certificate)
   (key
    :initarg :key
    :accessor ssl-stream-key)))

(defmethod stream-element-type ((stream ssl-stream))
  '(unsigned-byte 8))

(defmethod close ((stream ssl-stream) &key abort)
  (declare (ignore abort))
  (force-output stream)
  (ssl-free (ssl-stream-handle stream))
  (setf (ssl-stream-handle stream) nil)
  (close (ssl-stream-socket stream)))

(defmethod open-stream-p ((stream ssl-stream))
  (and (ssl-stream-handle stream) t))

(defmethod stream-listen ((stream ssl-stream))
  (or (ssl-stream-peeked-byte stream)
      (setf (ssl-stream-peeked-byte stream)
            (let* ((*blockp* nil)
                   (b (stream-read-byte stream)))
              (if (eql b :eof) nil b)))))

(defmethod stream-read-byte ((stream ssl-stream))
  (or (ssl-stream-peeked-byte stream)
      (let ((buf (ssl-stream-input-buffer stream)))
        (handler-case
            (cffi-sys::with-pointer-to-vector-data (ptr buf)
              (ensure-ssl-funcall (ssl-stream-socket stream)
                                  (ssl-stream-handle stream)
                                  #'ssl-read
                                  5.5
                                  (ssl-stream-handle stream)
                                  ptr
                                  1)
              (elt buf 0))
          (ssl-error-zero-return ()     ;SSL_read returns 0 on end-of-file
            :eof)))))

(defmethod stream-read-sequence ((stream ssl-stream) thing start end &key)
  (check-type thing (simple-array (unsigned-byte 8) (*)))
  (when (and (< start end) (ssl-stream-peeked-byte stream))
    (setf (elt thing start) (ssl-stream-peeked-byte stream))
    (setf (ssl-stream-peeked-byte stream) nil)
    (incf start))
  (let ((buf (ssl-stream-input-buffer stream)))
    (loop
        for length = (min (- end start) (length buf))
        while (plusp length)
        do
          (handler-case
              (cffi-sys::with-pointer-to-vector-data (ptr buf)
                (ensure-ssl-funcall (ssl-stream-socket stream)
                                    (ssl-stream-handle stream)
                                    #'ssl-read
                                    5.5
                                    (ssl-stream-handle stream)
                                    ptr
                                    length)
                (replace thing buf :start1 start :end1 (+ start length))
                (incf start length))
            (ssl-error-zero-return ()   ;SSL_read returns 0 on end-of-file
              (return))))
    start))

(defmethod stream-write-byte ((stream ssl-stream) b)
  (let ((buf (ssl-stream-output-buffer stream)))
    (when (eql (length buf) (ssl-stream-output-pointer stream))
      (force-output stream))
    (setf (elt buf (ssl-stream-output-pointer stream)) b)
    (incf (ssl-stream-output-pointer stream)))
  b)

(defmethod stream-write-sequence ((stream ssl-stream) thing start end &key)
  (check-type thing (simple-array (unsigned-byte 8) (*)))
  (let ((buf (ssl-stream-output-buffer stream)))
    (when (> (+ (- end start) (ssl-stream-output-pointer stream)) (length buf))
      ;; not enough space left?  flush buffer.
      (force-output stream)
      ;; still doesn't fit?
      (while (> (- end start) (length buf))
        (replace buf thing :start2 start)
        (incf start (length buf))
        (setf (ssl-stream-output-pointer stream) (length buf))
        (force-output stream)))
    (replace buf thing
             :start1 (ssl-stream-output-pointer stream)
             :start2 start
             :end2 end)
    (incf (ssl-stream-output-pointer stream) (- end start)))
  thing)

(defmethod stream-finish-output ((stream ssl-stream))
  (stream-force-output stream))

(defmethod stream-force-output ((stream ssl-stream))
  (let ((buf (ssl-stream-output-buffer stream))
        (fill-ptr (ssl-stream-output-pointer stream))
        (handle (ssl-stream-handle stream))
	(socket (ssl-stream-socket stream)))
    (when (plusp fill-ptr)
      (cffi-sys::with-pointer-to-vector-data (ptr buf)
        (ensure-ssl-funcall socket handle #'ssl-write 0.5 handle ptr fill-ptr))
      (setf (ssl-stream-output-pointer stream) 0))))


;;; interface functions
;;;
(defun make-ssl-client-stream
    (socket &key (method 'ssl-v23-method) external-format)
  "Returns an SSL stream for the client socket descriptor SOCKET."
  (ensure-initialized method)
  (let ((stream (make-instance 'ssl-stream :socket socket))
        (handle (ssl-new *ssl-global-context*)))
    (setf (ssl-stream-handle stream) handle)
    (ssl-set-bio handle (bio-new-lisp) (bio-new-lisp))
    (ssl-set-connect-state handle)
    (ensure-ssl-funcall socket handle #'ssl-connect 0.25 handle)
    (if external-format
        (flexi-streams:make-flexi-stream stream
                                         :external-format external-format)
        stream)))

(defun make-ssl-server-stream
    (socket &key certificate key (method 'ssl-v23-method) external-format)
  "Returns an SSL stream for the server socket descriptor SOCKET.
CERTIFICATE is the path to a file containing the PEM-encoded certificate for
 your server. KEY is the path to the PEM-encoded key for the server, which
must not be associated with a passphrase."
  (ensure-initialized method)
  (let ((stream (make-instance 'ssl-server-stream
		 :socket socket
		 :certificate certificate
		 :key key))
        (handle (ssl-new *ssl-global-context*))
	(bio (bio-new-lisp)))
    (setf (ssl-stream-handle stream) handle)
    (ssl-set-bio handle bio bio)
    (ssl-set-accept-state handle)
    (when (zerop (ssl-set-cipher-list handle "ALL"))
      (error 'ssl-error-initialize :reason "Can't set SSL cipher list"))
    (when key
      (unless (eql 1 (ssl-use-rsa-privatekey-file handle
						  key
						  +ssl-filetype-pem+))
        (error 'ssl-error-initialize :reason "Can't load RSA private key ~A")))
    (when certificate
      (unless (eql 1 (ssl-use-certificate-file handle
					       certificate
					       +ssl-filetype-pem+))
        (error 'ssl-error-initialize
	       :reason "Can't load certificate ~A" certificate)))
    (ensure-ssl-funcall socket handle #'ssl-accept 0.25 handle)
    (if external-format
        (flexi-streams:make-flexi-stream stream
                                         :external-format external-format)
        stream)))
