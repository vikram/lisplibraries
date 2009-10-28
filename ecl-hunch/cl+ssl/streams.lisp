;;; Copyright (C) 2001, 2003  Eric Marsden
;;; Copyright (C) 2005  David Lichteblau
;;; Copyright (C) 2007  Pixel // pinterface
;;; "the conditions and ENSURE-SSL-FUNCALL are by Jochen Schmidt."
;;;
;;; See LICENSE for details.

(eval-when (:compile-toplevel)
  (declaim
   (optimize (speed 3) (space 1) (safety 1) (debug 0) (compilation-speed 0))))

(in-package :cl+ssl)

(defclass ssl-stream
    (fundamental-binary-input-stream
     fundamental-binary-output-stream 
     trivial-gray-stream-mixin)
  ((ssl-stream-socket
    :initarg :socket
    :accessor ssl-stream-socket)
   (close-callback
    :initarg :close-callback
    :accessor ssl-close-callback)
   (handle
    :initform nil
    :accessor ssl-stream-handle)
   (deadline
    :initform nil
    :initarg :deadline
    :accessor ssl-stream-deadline)
   (output-buffer
    :initform (make-buffer +initial-buffer-size+)
    :accessor ssl-stream-output-buffer)
   (output-pointer
    :initform 0
    :accessor ssl-stream-output-pointer)
   (input-buffer
    :initform (make-buffer +initial-buffer-size+)
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
  (cond
    ((ssl-stream-handle stream)
     (unless abort
       (force-output stream))
     (ssl-free (ssl-stream-handle stream))
     (setf (ssl-stream-handle stream) nil)
     (when (streamp (ssl-stream-socket stream))
       (close (ssl-stream-socket stream)))
     (when (functionp (ssl-close-callback stream))
       (funcall (ssl-close-callback stream)))
     t)
    (t
     nil)))

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
            (with-pointer-to-vector-data (ptr buf)
              (ensure-ssl-funcall stream
                                  (ssl-stream-handle stream)
                                  #'ssl-read
                                  (ssl-stream-handle stream)
                                  ptr
                                  1)
              (buffer-elt buf 0))
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
        for length = (min (- end start) (buffer-length buf))
        while (plusp length)
        do
          (handler-case
              (with-pointer-to-vector-data (ptr buf)
                (ensure-ssl-funcall stream
                                    (ssl-stream-handle stream)
                                    #'ssl-read
                                    (ssl-stream-handle stream)
                                    ptr
                                    length)
                (v/b-replace thing buf :start1 start :end1 (+ start length))
                (incf start length))
            (ssl-error-zero-return ()   ;SSL_read returns 0 on end-of-file
              (return))))
    ;; fixme: kein out-of-file wenn (zerop start)?
    start))

(defmethod stream-write-byte ((stream ssl-stream) b)
  (let ((buf (ssl-stream-output-buffer stream)))
    (when (eql (buffer-length buf) (ssl-stream-output-pointer stream))
      (force-output stream))
    (setf (buffer-elt buf (ssl-stream-output-pointer stream)) b)
    (incf (ssl-stream-output-pointer stream)))
  b)

(defmethod stream-write-sequence ((stream ssl-stream) thing start end &key)
  (check-type thing (simple-array (unsigned-byte 8) (*)))
  (let ((buf (ssl-stream-output-buffer stream)))
    (when (> (+ (- end start) (ssl-stream-output-pointer stream)) (buffer-length buf))
      ;; not enough space left?  flush buffer.
      (force-output stream)
      ;; still doesn't fit?
      (while (> (- end start) (buffer-length buf))
        (b/v-replace buf thing :start2 start)
        (incf start (buffer-length buf))
        (setf (ssl-stream-output-pointer stream) (buffer-length buf))
        (force-output stream)))
    (b/v-replace buf thing
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
        (handle (ssl-stream-handle stream)))
    (when (plusp fill-ptr)
      (unless handle
	(error "output operation on closed SSL stream"))
      (with-pointer-to-vector-data (ptr buf)
        (ensure-ssl-funcall stream handle #'ssl-write handle ptr fill-ptr))
      (setf (ssl-stream-output-pointer stream) 0))))

#+clozure-common-lisp
(defun install-nonblock-flag (fd)
  (ccl::fd-set-flags fd (logior (ccl::fd-get-flags fd) 
                     #.(read-from-string "#$O_NONBLOCK"))))
                     ;; read-from-string is necessary because
                     ;; CLISP and perhaps other Lisps are confused
                     ;; by #$, signaling"undefined dispatch character $", 
                     ;; even though the defun in conditionalized by 
                     ;; #+clozure-common-lisp
                    
#+(and sbcl (not win32))
(defun install-nonblock-flag (fd)
  (sb-posix:fcntl fd
		  sb-posix::f-setfl
		  (logior (sb-posix:fcntl fd sb-posix::f-getfl)
			  sb-posix::o-nonblock)))

#-(or clozure-common-lisp (and sbcl (not win32)))
(defun install-nonblock-flag (fd)
  (declare (ignore fd)))


;;; interface functions
;;;

(defun install-handle-and-bio (stream handle socket unwrap-stream-p)
  (setf (ssl-stream-handle stream) handle)
  (when unwrap-stream-p
    (let ((fd (stream-fd socket)))
      (when fd
	(setf socket fd))))
  (etypecase socket
    (integer
     (install-nonblock-flag socket)
     (ssl-set-fd handle socket))
    (stream
     (ssl-set-bio handle (bio-new-lisp) (bio-new-lisp))))
  (ssl-ctx-ctrl handle
		+SSL_CTRL_MODE+
		+SSL_MODE_ACCEPT_MOVING_WRITE_BUFFER+
		0)
  socket)

(defun install-key-and-cert (handle key certificate)
  (when key
    (unless (eql 1 (ssl-use-rsa-privatekey-file handle
						key
						+ssl-filetype-pem+))
      (error 'ssl-error-initialize :reason (format nil "Can't load RSA private key file ~A" key))))
  (when certificate
    (unless (eql 1 (ssl-use-certificate-file handle
					     certificate
					     +ssl-filetype-pem+))
      (error 'ssl-error-initialize
	     :reason (format nil "Can't load certificate ~A" certificate)))))

(defun handle-external-format (stream ef)
  (if ef
      (flexi-streams:make-flexi-stream stream :external-format ef)
      stream))

;; fixme: free the context when errors happen in this function
(defun make-ssl-client-stream
    (socket &key certificate key password (method 'ssl-v23-method) external-format
                 close-callback (unwrap-stream-p t))
  "Returns an SSL stream for the client socket descriptor SOCKET.
CERTIFICATE is the path to a file containing the PEM-encoded certificate for
 your client. KEY is the path to the PEM-encoded key for the client, which
may be associated with the passphrase PASSWORD."
  (ensure-initialized :method method)
  (let ((stream (make-instance 'ssl-stream
			       :socket socket
			       :close-callback close-callback))
        (handle (ssl-new *ssl-global-context*)))
    (setf socket (install-handle-and-bio stream handle socket unwrap-stream-p))
    (ssl-set-connect-state handle)
    (with-pem-password (password)
      (install-key-and-cert handle key certificate))
    (ensure-ssl-funcall stream handle #'ssl-connect handle)
    (handle-external-format stream external-format)))

;; fixme: free the context when errors happen in this function
(defun make-ssl-server-stream
    (socket &key certificate key password (method 'ssl-v23-method) external-format
                 close-callback (unwrap-stream-p t))
  "Returns an SSL stream for the server socket descriptor SOCKET.
CERTIFICATE is the path to a file containing the PEM-encoded certificate for
 your server. KEY is the path to the PEM-encoded key for the server, which
may be associated with the passphrase PASSWORD."
  (ensure-initialized :method method)
  (let ((stream (make-instance 'ssl-server-stream
		 :socket socket
		 :close-callback close-callback
		 :certificate certificate
		 :key key))
        (handle (ssl-new *ssl-global-context*)))
    (setf socket (install-handle-and-bio stream handle socket unwrap-stream-p))
    (ssl-set-accept-state handle)
    (when (zerop (ssl-set-cipher-list handle "ALL"))
      (error 'ssl-error-initialize :reason "Can't set SSL cipher list"))
    (with-pem-password (password)
      (install-key-and-cert handle key certificate))
    (ensure-ssl-funcall stream handle #'ssl-accept handle)
    (handle-external-format stream external-format)))

#+openmcl
(defmethod stream-deadline ((stream ccl::basic-stream))
  (ccl::ioblock-deadline (ccl::stream-ioblock stream t)))
#+openmcl
(defmethod stream-deadline ((stream t))
  nil)


(defgeneric stream-fd (stream))
(defmethod stream-fd (stream) stream)

#+sbcl
(defmethod stream-fd ((stream sb-sys:fd-stream))
  (sb-sys:fd-stream-fd stream))

#+cmu
(defmethod stream-fd ((stream system:fd-stream))
  (system:fd-stream-fd stream))

#+openmcl
(defmethod stream-fd ((stream ccl::basic-stream))
  (ccl::ioblock-device (ccl::stream-ioblock stream t)))

#+clisp
(defmethod stream-fd ((stream stream))
  ;; sockets appear to be direct instances of STREAM
  (ignore-errors (socket:stream-handles stream)))
