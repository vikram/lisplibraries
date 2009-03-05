;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LW Style Buffer Protocol for other Lisps     ;;;
;;; So far only 8bit byte and character IO works ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :gray-stream)

(declaim (optimize (speed 3) (safety 0)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (progn
    (defconstant +default-input-buffer-size+ 2048)
    (defconstant +default-output-buffer-size+ 2048)
    (defconstant +buffer-delta+ 8)
    (defconstant +max-ascii-code+ 127)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct buffer-state 
    (input-buffer (make-array +default-input-buffer-size+ :element-type '(unsigned-byte 8)) :type (simple-array (unsigned-byte 8)))
    (input-index +default-input-buffer-size+ :type fixnum)
    (input-limit +default-input-buffer-size+ :type fixnum)
    (prev-index nil :type (or fixnum null)) ; previous character position
    (output-buffer (make-array +default-output-buffer-size+ :element-type '(unsigned-byte 8)) :type (simple-array (unsigned-byte 8)))
    (output-index 0 :type fixnum)
    (output-limit +default-output-buffer-size+ :type fixnum)))

;; Can be used to implement resourcing of buffers later
(defun %allocate-buffer-state (&optional (input-limit +default-input-buffer-size+) (output-limit +default-output-buffer-size+))
  (declare (ignore input-limit output-limit))
  (make-buffer-state))

(defun %deallocate-buffer-state (state)
  (declare (ignore state)))

;; Can be used to implement unbuffered encapsulating streams later
(defclass native-lisp-stream-mixin ()
  ((lisp-stream :initarg :lisp-stream
		:reader native-lisp-stream))
  (:documentation "Stream mixin that encapsulates a native stream."))

(defclass buffered-stream-mixin (native-lisp-stream-mixin)
  ((buffer-state :initform (%allocate-buffer-state)))
  (:documentation "Stream mixin that provides buffering for a native lisp stream."))

;; fundamental-bivalent-xxx-streams can be used to implement buffered
;; and unbuffered bivalent streams.  At the moment, we only implement
;; buffered ones.
(defclass fundamental-bivalent-input-stream
    (fundamental-character-input-stream fundamental-binary-input-stream)
  ())

(defclass fundamental-bivalent-output-stream
    (fundamental-character-output-stream fundamental-binary-output-stream)
  ())

(defclass buffered-bivalent-input-stream
    (buffered-stream-mixin fundamental-bivalent-input-stream)
  ())

(defclass buffered-bivalent-output-stream
    (buffered-stream-mixin fundamental-bivalent-output-stream)
  ())

#+sb-unicode
(progn
  (defclass external-format-mixin ()
    ((external-format :initform :latin1
                      :reader external-format
                      :type symbol)
     ;; Cached values for external format
     (ef-var-length-p :initform nil
                      :reader stream-var-length-p)
     (ef-ascii-compatible-p :initform t
                            :reader stream-ef-ascii-compatible-p)
     (code->ef-mapper :initform #'identity
                      :reader stream-code->ef-mapper
                      :type (function (fixnum) (unsigned-byte 8)))
     (ef->code-mapper :initform #'identity
                      :reader stream-ef->code-mapper
                      :type (function ((unsigned-byte 8)) fixnum))
     (ef-char-bytes :initform nil
                    :reader stream-ef-char-bytes
                    :type (function ((array (unsigned-byte 8))
                                     fixnum (or fixnum null))
                                    fixnum))
     (ef-code-length :initform nil
                     :reader stream-ef-code-length
                     :type (function ((unsigned-byte 8)) fixnum))
     (ef-get-char :initform nil
                  :reader stream-ef-get-char
                  :type (function ((array (unsigned-byte 8))
                                   fixnum
                                   fixnum)
                                  character))
     (ef-char-len :initform nil
                  :reader stream-ef-char-len
                  :type (function (fixnum) fixnum))
     (char->ef :initform nil
               :reader stream-char->ef
               :type (function (fixnum
                                (array (unsigned-byte 8))
                                fixnum
                                fixnum))))
    (:documentation "Stream mixin that provides external format handling."))

  (defmethod initialize-instance :after ((stream external-format-mixin)
                                         &key external-format
                                         &allow-other-keys)
    (when external-format
      (setf (external-format stream) external-format)))

  (defgeneric (setf external-format) (ef stream))
  (defmethod (setf external-format) (ef (stream external-format-mixin))
    (if (eq ef (slot-value stream 'external-format))
        ef
        (with-slots (external-format
                     ef-var-length-p
                     ef-ascii-compatible-p
                     code->ef-mapper
                     ef->code-mapper
                     ef-char-bytes
                     ef-code-length
                     ef-get-char
                     ef-char-len
                     char->ef)
            stream
          (let ((ef-entry (get-ef ef)))
            (when (and ef-entry (ascii-compatible-ef-p ef-entry))
              (setf external-format ef)
              (setf ef-var-length-p (variable-length-ef-p ef-entry))
              (setf ef-ascii-compatible-p (ascii-compatible-ef-p ef-entry))
              (setf code->ef-mapper (code->ef-mapper ef-entry))
              (setf ef->code-mapper (ef->code-mapper ef-entry))
              (setf ef-char-bytes (bytes-per-char-ef ef-entry))
              (setf ef-code-length (ef-code-length ef-entry))
              (setf ef-get-char (simple-get-char-ef ef-entry))
              (setf ef-char-len (char-len-as-ef ef-entry))
              (setf char->ef (char->ef ef-entry))
              ef))))))

(defclass buffered-bivalent-stream
    (#+sb-unicode
     external-format-mixin
     buffered-bivalent-input-stream
     buffered-bivalent-output-stream)
  ())

(defmacro with-stream-output-buffer ((buffer index limit) stream &body forms)
  (let ((state (gensym "BUFFER-STATE-")))
  `(let ((,state (slot-value ,stream 'buffer-state)))
     (symbol-macrolet ((,buffer ,(list 'buffer-state-output-buffer state))
                       (,index ,(list 'buffer-state-output-index state))
                       (,limit ,(list 'buffer-state-output-limit state)))
       ,@forms))))

;;; Encapsulated native streams

(defmethod close ((stream native-lisp-stream-mixin) &key abort)
  (close (native-lisp-stream stream) :abort abort))

(defmethod stream-listen ((stream native-lisp-stream-mixin))
  (listen (native-lisp-stream stream)))

(defmethod open-stream-p ((stream native-lisp-stream-mixin))
  (common-lisp::open-stream-p (native-lisp-stream stream)))

(defmethod stream-clear-output ((stream native-lisp-stream-mixin))
  (clear-output (native-lisp-stream stream)))

;;; Input streams

(defmacro with-stream-input-buffer ((buffer index limit) stream &body forms)
  (let ((state (gensym "BUFFER-STATE-")))
  `(let ((,state (slot-value ,stream 'buffer-state)))
     (symbol-macrolet ((,buffer ,(list 'buffer-state-input-buffer state))
                       (,index ,(list 'buffer-state-input-index state))
                       (,limit ,(list 'buffer-state-input-limit state)))
       ,@forms))))

(defmacro with-stream-input-buffer-prev
    ((buffer index limit prev-index) stream &body forms)
  (let ((state (gensym "BUFFER-STATE-")))
    `(let ((,state (slot-value ,stream 'buffer-state)))
      (symbol-macrolet ((,buffer ,(list 'buffer-state-input-buffer state))
                        (,index ,(list 'buffer-state-input-index state))
                        (,limit ,(list 'buffer-state-input-limit state))
                        (,prev-index ,(list 'buffer-state-prev-index state)))
          ,@forms))))

(defgeneric stream-fill-buffer (stream))
(defmethod stream-fill-buffer ((stream buffered-stream-mixin))
  ;; Implement b/nb semantics: block until at least one byte is read,
  ;; but not until the whole buffer is filled.  This means it takes at
  ;; most n calls to this function to fill a buffer of length n, even
  ;; with a slow connection.
  ;; WARNING It may block if the whole code sequence is not available.
  (with-stream-input-buffer-prev (buffer index limit prev-index) stream
    (with-slots (ef-var-length-p ef-code-length) stream
      (let* ((the-stream (native-lisp-stream stream))
             (read-bytes
              #+sb-unicode
               (if ef-var-length-p
                   (do ((byte)
                        (ulen 0)
                        (n-read 0 (1+ n-read)))
                       ;; No error handling. Just ensure that
                       ;; valid code sequences are completely read.
                       ((or (and (= ulen 0)
                                 (or (and (> n-read 0)
                                          (not (listen the-stream)))
                                     (>= n-read
                                         #. (- +default-input-buffer-size+
                                               +buffer-delta+))))
                            (>= n-read #. +default-input-buffer-size+)
                            (null (setf byte (read-byte the-stream nil nil))))
                        n-read)
                     (declare (type (or (unsigned-byte 8) null) byte)
                              (type fixnum ulen n-read))
                     (setf (aref buffer n-read) byte)
                     (setf ulen
                           (if (> byte #. +max-ascii-code+)
                               (let ((len (funcall ef-code-length byte)))
                                 (declare (type fixnum len))
                                 (if (> len 1)
                                     (1- len)
                                     (max (1- ulen) 0)))
                               0)))
                   (do ((byte)
                        (n-read 0 (1+ n-read)))
                       ((or (and (> n-read 0) (not (listen the-stream)))
                            (>= n-read #. +default-input-buffer-size+)
                            (null (setf byte (read-byte the-stream nil nil))))
                        n-read)
                     (declare (type (or (unsigned-byte 8) null) byte)
                              (type fixnum n-read))
                     (setf (aref buffer n-read) byte)))
               #-sb-unicode
               (do ((byte)
                    (n-read 0 (1+ n-read)))
                   ((or (and (> n-read 0) (not (listen the-stream)))
                        (>= n-read #. +default-input-buffer-size+)
                        (null (setf byte (read-byte the-stream nil nil))))
                    n-read)
                 (declare (type (or (unsigned-byte 8) null) byte)
                          (type fixnum n-read))
                 (setf (aref buffer n-read) byte))))
        (declare (type fixnum read-bytes))
        (if (zerop read-bytes)
            nil
            (setf index 0
                  prev-index nil
                  limit read-bytes))))))

(defmethod stream-read-byte ((stream buffered-bivalent-input-stream))
  (with-stream-input-buffer-prev (buffer index limit prev-index) stream
     (unless (< index limit)
       (when (null (stream-fill-buffer stream))
	 (return-from stream-read-byte :eof)))
     (prog1 (aref buffer index)
       (incf index)
       (setf prev-index nil))))

#+sb-unicode
(defmethod stream-read-char ((stream buffered-bivalent-input-stream))
  (with-stream-input-buffer-prev (buffer index limit prev-index) stream
    (with-slots (ef-var-length-p ef->code-mapper ef-char-bytes ef-get-char)
        stream
      (if (and (>= index limit)
               (null (stream-fill-buffer stream)))
          :eof
          (let ((byte (aref buffer index)))
            (setf prev-index index)
            (cond ((<= byte #. +max-ascii-code+)
                   (incf index)
                   (code-char byte))
                  ((not ef-var-length-p)
                   (incf index)
                   (code-char (funcall ef->code-mapper byte)))
                  (t
                   (let* ((len (funcall ef-char-bytes buffer index limit))
                          (char (funcall ef-get-char buffer index len)))
                     (declare (type fixnum len)
                              (type character char))
                     (setf index (min (+ index len) limit))
                     char))))))))

#-sb-unicode
(defmethod stream-read-char ((stream buffered-bivalent-input-stream))
  (with-stream-input-buffer-prev (buffer index limit prev-index) stream
    (if (and (>= index limit)
             (null (stream-fill-buffer stream)))
        :eof
        (let ((char (code-char (aref buffer index))))
          (setf prev-index index)
          (incf index)
          char))))

(defmethod stream-listen ((stream buffered-bivalent-input-stream))
  (with-stream-input-buffer (buffer index limit) stream
    (or (< index limit)
        (listen (native-lisp-stream stream)))))

(defmethod stream-read-char-no-hang ((stream buffered-bivalent-input-stream))
  (if (stream-listen stream)
      (stream-read-char stream)
      nil))

(defmethod stream-unread-char ((stream buffered-bivalent-input-stream) char)
  (with-stream-input-buffer-prev (buffer index limit prev-index) stream
    (if prev-index
        (setf index prev-index
              prev-index nil)
        (error "Cannot unread char ~A" char))))

(defmethod stream-peek-char ((stream buffered-bivalent-input-stream))
  (let ((char (stream-read-char stream)))
    (unless (eq char :eof)
      (stream-unread-char stream char))
    char))

(defmethod stream-read-line ((stream buffered-bivalent-input-stream))
  (let ((res (make-array 80 :element-type 'character :fill-pointer 0)))
    (loop
     (let ((ch (stream-read-char stream)))
       (cond ((eq ch :eof)
	      (return (values (copy-seq res) t)))
	     ((char= ch #\Linefeed)
              (return (values (copy-seq res) nil)))
             (t
              (vector-push-extend ch res)))))))

;; specialised read-elements
(defmacro define-read-elements (name sequence-type &optional reader-fun)
  ;; If SEQUENCE is a list (READER-FUN not provided), determine
  ;; the reader function from the type of the first element of the list.
  (let* ((reader-sym (unless reader-fun (gensym "READER-")))
         (reading-form (if reader-fun
                           `(,reader-fun socket-stream)
                           `(funcall ,reader-sym socket-stream)))
         (reader-let (unless reader-fun
                       `((,reader-sym (if (subtypep (type-of (first sequence))
                                                    'character)
                                          #'stream-read-char
                                          #'stream-read-byte))))))
    `(defun ,name (socket-stream sequence start end)
       (declare (type ,sequence-type sequence)
                (type fixnum start)
                (type (or fixnum null) end))
       (let* (,@reader-let
              (len (length sequence))
              (n (- (min (or end len) len) start)))
         (declare (type fixnum len n))
         (loop :for i fixnum :upfrom start
               :repeat n
               :for e = ,reading-form
               :when (eq e :eof) :do (return-from ,name i)
               :do (setf (elt sequence i) e))
         (the fixnum (+ start n))))))

(define-read-elements read-elements-string string stream-read-char)
(define-read-elements read-elements-array-ub8
                      (array (unsigned-byte 8))
                      stream-read-byte)
(define-read-elements read-elements-list sequence)
(define-read-elements read-elements-generic sequence stream-read-byte)

(defmethod stream-read-sequence ((stream buffered-bivalent-input-stream)
                                 sequence &optional start end)
  (typecase sequence
    (string
     (read-elements-string stream sequence start end))
    ((array (unsigned-byte 8))
     (read-elements-array-ub8 stream sequence start end))
    (list
     (read-elements-list stream sequence start end))
    (otherwise
     (read-elements-generic stream sequence start end))))

(defmethod stream-clear-input ((stream buffered-bivalent-input-stream))
  (with-stream-input-buffer-prev (buffer index limit prev-index) stream
    (setf index limit
          prev-index nil)
    (clear-input (native-lisp-stream stream)))
  nil)

(defmethod stream-element-type ((stream fundamental-bivalent-input-stream))
  '(or character (unsigned-byte 8)))

;;; Output streams

(defgeneric stream-write-buffer (stream buffer start end))
(defmethod stream-write-buffer ((stream buffered-stream-mixin)
                                buffer start end)
  (let ((lisp-stream (native-lisp-stream stream)))
    (write-sequence buffer lisp-stream :start start :end end)))

(defgeneric stream-flush-buffer (stream))
(defmethod stream-flush-buffer ((stream buffered-stream-mixin))
  (with-stream-output-buffer (buffer index limit) stream
    (when (plusp index)
      (stream-write-buffer stream buffer 0 index)
      (setf index 0))))

(defmacro write-byte-mac (stream buffer index limit byte)
  `(progn
     (unless (< ,index ,limit)
       (stream-flush-buffer ,stream))
     (setf (aref ,buffer ,index) ,byte)
     (incf ,index)))

(defmethod stream-write-byte ((stream buffered-bivalent-output-stream) byte)
  (with-stream-output-buffer (buffer index limit) stream
    (write-byte-mac stream buffer index limit byte)
  byte))

#+sb-unicode
(defun write-char-var (stream char)
  (declare (type character char))
  (with-stream-output-buffer (buffer index limit) stream
    (with-slots (ef-char-len char->ef) stream
      (let ((code (char-code char)))
        (declare (type fixnum code))
        (if (<= code #. +max-ascii-code+)
            (write-byte-mac stream buffer index limit code)
            (let ((len (funcall ef-char-len code)))
              (declare (type fixnum len))
              (unless (< (+ index len) limit)
                (stream-flush-buffer stream))
              (funcall char->ef code buffer index len)
              (incf index len)))))))

#+sb-unicode
(defun write-char-fix (stream char)
  (declare (type character char))
  (with-stream-output-buffer (buffer index limit) stream
    (with-slots (code->ef-mapper) stream
      (let ((code (char-code char)))
        (declare (type fixnum code))
        (if (<= code #. +max-ascii-code+)
            (write-byte-mac stream buffer index limit code)
            (let ((byte (funcall code->ef-mapper code)))
              (declare (type (unsigned-byte 8) byte))
              (write-byte-mac stream buffer index limit byte)))))))

#+sb-unicode
(defun write-chars (stream sequence start end)
  (declare (type fixnum start)
           (type (or fixnum null) end)
           (type (array character) sequence))
  (with-slots (ef-var-length-p) stream
    (let* ((len (length sequence))
           (end (if end (min end len) len)))
      (declare (type fixnum len end))
      (if ef-var-length-p
          (loop :for j fixnum :from start :below end
                :do (write-char-var stream (aref sequence j)))
          (loop :for j fixnum :from start :below end
                :do (write-char-fix stream (aref sequence j)))))))

#-sb-unicode  
(defun write-chars (stream sequence start end)
  (declare (type fixnum start)
           (type (or fixnum null) end)
           (type (array character) sequence))
  (with-stream-output-buffer (buffer index limit) stream
    (let* ((len (length sequence))
           (end (if end (min end len) len)))
      (declare (type fixnum len end))
      (loop :for j fixnum :from start :below end
            :do (write-byte-mac stream buffer index limit
                                (char-code (aref sequence j)))))))

(defun write-bytes (stream sequence start end)
  (declare (type fixnum start)
           (type (or fixnum null) end)
           (type (array (unsigned-byte 8)) sequence))
  (with-stream-output-buffer (buffer index limit) stream
    (let* ((len (length sequence))
           (end (if end (min end len) len)))
      (declare (type fixnum len end))
      (assert (<= 0 start end len))
      (loop :for j fixnum :from start :below end
            :do (write-byte-mac stream buffer index limit
                                (aref sequence j))))))

(defun write-list (stream sequence start end)
  (declare (type list sequence)
           (type fixnum start)
           (type (or fixnum null) end))
  (let ((len (when end (- end start))))
    (declare (type fixnum len))
    (loop :for j fixnum :from 0
          :for e :in sequence
          :while (or (not len) (< j len))
          :do (etypecase e
                (character
                 (locally (declare (type character e))
                   (stream-write-char stream e)))
                ((unsigned-byte 8)
                 (locally (declare (type (unsigned-byte 8) e))
                   (stream-write-byte stream e)))))))

#+sb-unicode
(defmethod stream-write-char ((stream buffered-bivalent-output-stream) char)
  (with-slots (ef-var-length-p) stream
    (declare (type character char))
    (if ef-var-length-p
        (write-char-var stream char)
        (write-char-fix stream char)))
  char)

#-sb-unicode
(defmethod stream-write-char ((stream buffered-bivalent-output-stream) char)
  (with-stream-output-buffer (buffer index limit) stream
    (write-byte-mac stream buffer index limit (char-code char))
  byte))

(defmethod stream-write-string ((stream buffered-bivalent-output-stream)
                                string &optional (start 0) end)
  (write-chars stream string start end)
  string)

(defmethod stream-write-sequence ((stream buffered-stream-mixin) sequence
                                  &optional (start 0) end)
  (typecase sequence
    (string
     (write-chars stream sequence start end))
    ((array (unsigned-byte 8))
     (write-bytes stream sequence start end))
    (list
     (write-list stream sequence start end)))
  sequence)

(defmethod stream-element-type ((stream fundamental-bivalent-output-stream))
  '(or character (unsigned-byte 8)))

(defmethod stream-line-column ((stream fundamental-bivalent-output-stream))
  nil)

(defmethod stream-finish-output ((stream buffered-bivalent-output-stream))
  (stream-flush-buffer stream)
  (finish-output (native-lisp-stream stream)))

(defmethod stream-force-output ((stream buffered-bivalent-output-stream))
  (stream-flush-buffer stream)
  (force-output (native-lisp-stream stream)))

(defmethod stream-clear-output ((stream buffered-bivalent-output-stream))
  (with-stream-output-buffer (buffer index limit) stream
    (setf index limit))
  (clear-output (native-lisp-stream stream)))  ; Clear native stream also

(defmethod close ((stream buffered-bivalent-output-stream) &key abort)
  (if abort
      (stream-clear-output stream)
      (stream-finish-output stream))
  (close (native-lisp-stream stream) :abort abort))
