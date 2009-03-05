(in-package :gzip-stream)
(declaim (optimize speed (safety 1) (debug 1)))

(declaim (type fixnum +buffer-size+))
(defconstant +buffer-size+ 8192)

;; gzip stream
(defclass gzip-output-stream (fundamental-binary-output-stream)
  ((underlying-file :initarg :understream :accessor under-file)
   (input-buffer :initform (make-array +buffer-size+ :element-type '(unsigned-byte 8)))
   (input-pos :initform 0 :accessor input-pos :type fixnum)
   (deflate-stream :accessor deflate-stream)
   (size :initform 0 :type fixnum)
   (crc-high :initform #xFFFF)
   (crc-low :initform #xFFFF)
   (compress-buffer :initarg :buffer :accessor compress-buffer)))

; from salza's examples/gzip.lisp
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +gzip-signature+
    (if (boundp '+gzip-signature+)
        (symbol-value '+gzip-signature+)
        #(#x1F #x8B))))

(defconstant +gzip-deflate-compression+ 8)
(defconstant +gzip-flags+ 0)

(defconstant +gzip-fast-compression+ 4)
(defconstant +gzip-unix-os+ 3)

(defun write-gzip-header (stream)
  (write-sequence +gzip-signature+ stream)
  (write-byte +gzip-deflate-compression+ stream)
  (write-byte +gzip-flags+ stream)
  ;; mtime
  (write-sequence #(0 0 0 0) stream)
  (write-byte +gzip-fast-compression+ stream)
  (write-byte +gzip-unix-os+ stream))

(defmethod initialize-instance :after ((stream gzip-output-stream) &rest initargs &key)
  (declare (ignore initargs))
  (let* ((compress-buffer (make-array +buffer-size+ :element-type '(unsigned-byte 8)))
         (callback (lambda (deflate-stream)
                     (write-sequence compress-buffer
                                     (under-file stream)
                                     :start 0
                                     :end (salza-deflate:deflate-stream-pos deflate-stream))
                     (setf (salza-deflate:deflate-stream-pos deflate-stream) 0))))
    (setf (deflate-stream stream) 
          (salza-deflate:make-deflate-stream compress-buffer
                                             :callback callback))
    (write-gzip-header (slot-value stream 'underlying-file))
    (salza-deflate:start-deflate-stream (deflate-stream stream))))

(defun gzip-stream-output (stream bytes-written)
  (declare (type fixnum bytes-written))
  (with-slots (input-buffer size crc-high crc-low deflate-stream) stream
    (incf (the fixnum size) bytes-written)
    (salza-deflate:deflate-write-sequence
     input-buffer deflate-stream :end bytes-written)
    (multiple-value-bind (tmp-crc-high tmp-crc-low)
        (salza-deflate:crc32 crc-high crc-low input-buffer bytes-written)
      (setf crc-high tmp-crc-high
            crc-low tmp-crc-low))
    (setf (input-pos stream) 0)))

(defmethod stream-write-byte ((stream gzip-output-stream) byte)
  (declare (type integer byte))
  (with-slots (input-buffer) stream
    (setf (aref (the salza-types:octet-vector input-buffer)
                (the fixnum (input-pos stream)))
          byte)
    (incf (the fixnum (input-pos stream)))
    (when (>= (the fixnum (input-pos stream)) +buffer-size+)
      (gzip-stream-output stream +buffer-size+))))

(defmethod stream-force-output ((stream gzip-output-stream))
  (unless (zerop (the fixnum (input-pos stream)))
    (gzip-stream-output stream (input-pos stream)))
  (call-next-method)
  (force-output (under-file stream))
  (values))

(defmethod stream-finish-output ((stream gzip-output-stream))
  (flet ((write-uint32 (value output)
           (declare (type (unsigned-byte 32) value))
           (write-byte (ldb (byte 8 0) value) output)
           (write-byte (ldb (byte 8 8) value) output)
           (write-byte (ldb (byte 8 16) value) output)
           (write-byte (ldb (byte 8 24) value) output)))
    (with-slots (input-buffer underlying-file crc-high crc-low size finished) stream
      (unless (zerop (the fixnum (input-pos stream)))
        (gzip-stream-output stream (input-pos stream)))
      (salza-deflate:finish-deflate-stream (slot-value stream 'deflate-stream ))
      (setf crc-high (logxor crc-high #xFFFF)
            crc-low (logxor crc-low #xFFFF))
      (write-uint32 (logior (ash crc-high 16) crc-low) underlying-file)
      (write-uint32 size underlying-file)
      (call-next-method)
      (finish-output underlying-file)))
  (values))
  
(defmethod stream-clear-output ((stream gzip-output-stream))
  (setf (input-pos stream) 0)
  (call-next-method)
  (values))

;; I think this is broken
(defmethod stream-write-sequence 
           #+lispworks ((stream gzip-output-stream) sequence start end)
           #-lispworks ((stream gzip-output-stream) sequence &optional (start 0) (end (length sequence)))
  (declare (optimize speed (safety 1) (debug 0))
           (type fixnum start end))
  (assert (< start end))
  (let ((size-to-write (- end start))
        (written 0)
        (input-buffer (slot-value stream 'input-buffer)))
    (declare (type fixnum written size-to-write))
    (loop while (< written size-to-write) do
          (let ((to-write (min (- +buffer-size+ (input-pos stream))
                               end)))
            (salza-deflate::octet-replace input-buffer
                                          sequence
                                          (input-pos stream) (1- +buffer-size+)
                                          start to-write)
            (incf (the fixnum (input-pos stream)) to-write)
            (incf written to-write)
            (gzip-stream-output stream (input-pos stream))))))

(defmethod close ((stream gzip-output-stream) &key abort)
  (unless abort
    (finish-output stream))
  (close (under-file stream) :abort abort))

(defmethod stream-element-type ((stream gzip-output-stream))
  (stream-element-type (under-file stream)))

(defmethod stream-line-column  ((stream gzip-output-stream))
  (declare (ignore stream))
  nil)

(defmethod stream-write-char ((stream gzip-output-stream) char)
  (stream-write-char (under-file stream) char))

(defmethod stream-write-string ((stream gzip-output-stream) string &optional start end)
  (stream-write-string (under-file stream) string start end))




;;; Input
(defclass gzip-input-stream (fundamental-binary-input-stream)
  ((underfile :accessor underfile-of :initarg :understream)
   (read-buffer :accessor read-buffer-of :initform
                (flexi-streams:make-in-memory-input-stream ""))
   (data-buffer :accessor data-buffer-of :initform (make-array (* 32 1024)))
   (bit-reader :accessor bit-reader-of :initform nil)
   (last-end :initform 0)))

(defmethod initialize-instance :after ((obj gzip-input-stream) &rest args)
  (skip-gzip-header (underfile-of obj))
  (setf (bit-reader-of obj)
        (new-bit-reader (underfile-of obj))))

(defmethod stream-element-type ((stream gzip-input-stream))
  '(unsigned-byte 8))

(defmethod close ((stream gzip-input-stream) &key abort)
  (close (underfile-of stream) :abort abort))

(defmethod input-stream-p ((stream gzip-input-stream))
  t)

(defmethod output-stream-p ((stream gzip-input-stream))
  nil)

(defun fill-buffer (stream)
  (with-slots (underfile read-buffer bit-reader data-buffer last-end)
      stream
    (setf read-buffer
          (flexi-streams:make-in-memory-input-stream 
           (flexi-streams:with-output-to-sequence (tmp)
             (setf last-end
                   (process-deflate-block bit-reader tmp
                                          data-buffer last-end)))))))

(defmethod stream-read-byte ((stream gzip-input-stream)) 
  (with-slots (read-buffer last-end) stream
    (let ((next-byte (read-byte read-buffer nil nil)))
      (if (null next-byte)
          (if last-end
              (progn (fill-buffer stream) (stream-read-byte stream))
              :eof)
          next-byte))))
                      
(defmethod stream-listen ((stream gzip-input-stream))
  (listen (underfile-of stream)))

(defmethod stream-clear-input ((stream gzip-input-stream))
  (clear-input (underfile-of stream)))



;; Utils Functions
(defmacro with-open-gzip-file ((var path &rest open-args &key (direction :input) &allow-other-keys)
                               &body body)
  (let ((abort (gensym "abort")))
    `(let ((,var (make-instance (ecase ,direction
                                  (:input 'gzip-input-stream)
                                  (:output 'gzip-output-stream))
                                :understream (open ,path ,@open-args :element-type 'salza::octet)))
           (,abort t))
       (unwind-protect 
           (prog1 (progn ,@body)
             (setf ,abort nil))
         (close ,var :abort ,abort)))))



(defun gzip (in-file out-file)
  (with-open-file (in-stream in-file :element-type '(unsigned-byte 8))
    (with-open-gzip-file (out-stream out-file :direction :output 
                                     :element-type '(unsigned-byte 8) :if-exists :supersede)
      (let ((buffer (make-array +buffer-size+ :element-type '(unsigned-byte 8))))
        (loop for x = (read-sequence buffer in-stream)
              until (zerop x) do
              (write-sequence buffer out-stream :start 0 :end x)))))
  (truename out-file))


(defun gunzip (in-file out-file)
  (with-open-gzip-file (ins in-file)
    (with-open-file (outs out-file :direction :output 
                          :element-type '(unsigned-byte 8) :if-exists :supersede)
      (let ((buffer (make-array +buffer-size+ :element-type '(unsigned-byte 8))))
        (loop for x = (read-sequence buffer ins)
              until (zerop x) do
              (write-sequence buffer outs :start 0 :end x)))))
  (truename out-file))
      
      
#|
;;; zip stream
(defparameter foo (make-hash-table))
(dotimes (x 1000) (setf (gethash x foo) (princ-to-string x)))
(time (cl-store:store foo "~/foo.out"))


(with-open-gzip-file (s "E:/test/foo.gz")
  (cl-store:restore s))

(with-open-gzip-file (s "E:/test/foo.gz" :direction :output
                        :if-exists :supersede)
  (cl-store:store "Hello" s))

(cl-store:store "Hello" "E:/test/foo.no-comp")

(time (gzip "~/foo.out" "~/gzipped-diff.gz"))

(with-open-gzip-file (s "E:/test/foo.gz" :direction :output :if-exists :supersede)
  (time (cl-store:store foo s)))

(gunzip "E:/test/foo.gz" "E:/test/foo.nogz")
(cl-store:restore "E:/test/foo.nogz")

|#
