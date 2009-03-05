;;;
;;; cl-zlib.cl
;;; 
;;; Version 1.2, 7/29/2002
;;;
;;; Copyright (c) 2002 Alberto Riva <Alberto.Riva@TCH.Harvard.edu>
;;;
;;; This code implements a foreign function interface to the zlib
;;; library (http://www.zlib.org/). Two different sets of functions
;;; are provided, for in-memory compression/decompression and
;;; file-based compression/decompression. The first set includes the
;;; functions ZCOMPRESS, ZUNCOMPRESS, COMPRESS-STRING and
;;; UNCOMPRESS-STRING. The second set includes GZ-OPEN, GZ-CLOSE, GZ-READ, 
;;; GZ-WRITE, GZ-READ-LINE, GZ-WRITE-LINE and WITH-OUTPUT-TO-GZ.
;;;
;;; Use (MAN '<symbol>) for documentation on this library. (MAN) with no
;;; arguments prints documentation for all exported symbols. Examples and
;;; tests are at the end of the file.

;;; Changelog:

;;; 7/12/2002 - Fixed GZ-READ-LINE
;;; 7/29/2002 - Fixed GZ-READ-LINE-INT (thanks to Larry Hunter).

(in-package :zlib)

(eval-when (:compile-toplevel)
  (proclaim '(optimize (safety 3))))

;;;;
;;;; Documentation
;;;;
(defconstant +z-ok+ 0)
(defconstant +z-errno+        -1)
(defconstant +z-stream-error+ -2)
(defconstant +z-data-error+   -3)
(defconstant +z-mem-error+    -4)
(defconstant +z-buf-error+    -5)
(defconstant +z-version-error+ -6)

(defparameter *uncompress-buffer-scale-factor* 4
  "Buffer increased to this value times then UNCOMPRESS-STRING
need more space for operation.")

(defparameter *read-buffer-length* 4096
  "Length of buffer for reading (GZ-READ-LINE).
Doesn't affect the actual length of returning string.")

(defvar *zlib-symbols*
    '(*zlib-path* zcompress zuncompress compress-string uncompress-string
      gz-open gz-close gz-read gz-write gz-read-line gz-write-line
      with-gz))

#+allegro
(defun man (&rest symbols)
  (unless symbols
    (setq symbols *zlib-symbols*))
  (mapc #'(lambda (sym)
            (format t "~a " sym)
            (cond ((fboundp sym)
                   (format t "~a [~a]~%~%" 
                           (excl::arglist sym)
                           (if (macro-function sym)
                               "Macro" "Function"))
                   (format t "~a~%~%" (documentation sym 'function)))
                  ((boundp sym)
                   (format t "[Variable]~%Value: ~a~%~%~a~%~%" 
                           (symbol-value sym)
                           (documentation sym 'variable)))))
        symbols)
  (values))

;;;;
;;;; Configuration
;;;;

;; This code is based on the appropriate CL-PDF part
(eval-when (:compile-toplevel :load-toplevel :execute)

  (defvar *zlib-search-paths*
    `(,(directory-namestring (or *load-truename* (truename "./")))
       #+lispworks
       ,(directory-namestring (lw:lisp-image-name))
       "/usr/local/lib/"
       "/usr/lib/"
       "/windows/system32/"
       "/winnt/system32/")
    "The paths where to search the zlib shared library")

  (defvar *zlib-names* '("libz" "zlib")
    "Various library names depending from the system.")

  (defun find-zlib-path ()
    (dolist (name *zlib-names*)
      (let ((path (uffi:find-foreign-library
                   name
                   *zlib-search-paths*
                   :drive-letters '("C" "D" "E")
                   :types '("so" "a" "dll" "dylib"))))
        (when path
          (return path)))))
        
  (defvar *zlib-loaded* nil)

  (unless *zlib-loaded*
    (let ((zlib-path (find-zlib-path)))
      (cond (zlib-path
              (format t "~&;;; Loading ~s" zlib-path)
              (load-foreign-library zlib-path
                                    :module "zlib"
                                    :supporting-libraries '("c"))
              (setq *zlib-loaded* t))
            (t (error "Couldn't find ZLIB library with basename in ~{~A~^,~} in
~{~A~^,~}" *zlib-names* *zlib-search-paths*))))))

;;;;
;;;; In-memory compression
;;;;

(uffi:def-function ("compress" %gz-compress)
    ((dest (* :unsigned-char))
     (destlen (* :long))
     (source (* :unsigned-char))
     (sourcelen :long))
  :returning :int
  :module "zlib")

(uffi:def-function ("uncompress" %gz-uncompress)
    ((dest (* :unsigned-char))
     (destlen (* :long))
     (source (* :unsigned-char))
     (sourcelen :long))
  :returning :int
  :module "zlib")

(uffi:def-function ("compress" %gz-compress-string)
    ((dest (* :unsigned-char))
     (destlen (* :long))
     (source :cstring)
     (sourcelen :long))
  :returning :int
  :module "zlib")

(uffi:def-function ("uncompress" %gz-uncompress-string)
    ((dest (* :unsigned-char))
     (destlen (* :long))
     (source :cstring)
     (sourcelen :long))
  :returning :int
  :module "zlib")

(defun zcompress (source sourcelen dest)
  "Compress the first SOURCELEN bytes of SOURCE into DEST. DEST should
be an array of (unsigned-byte 8), and should be large enough to hold the
compressed contents. Returns the compressed length.

Note that the size of the DEST array should be at least 0.1% more than 
sourcelen plus 12 bytes, but the actual number of array elements filled 
in by the compression algorithm will usually be smaller (depending on how
'predictable' the input data is)." 
  (let ((destlen (uffi:allocate-foreign-object :long)))
    (setf (uffi:deref-pointer destlen :long) (length dest))
    (let ((res (%gz-compress dest destlen source sourcelen)))
      (unwind-protect
           (if (zerop res)
               (uffi:deref-pointer destlen :long)
               (error "zlib error, code ~d" res))
        (uffi:free-foreign-object destlen)))))

(defun zuncompress (source sourcelen dest)
  "Uncompress the first SOURCELEN bytes of SOURCE into DEST. DEST
should be long enough to hold the uncompressed contents. Returns the
uncompressed length."
  (let ((destlen (uffi:allocate-foreign-object :long)))
    (setf (uffi:deref-pointer destlen :long) (length dest))
    (let ((res (%gz-uncompress dest destlen source sourcelen)))
      (unwind-protect
           (if (zerop res)
               (uffi:deref-pointer destlen :long)
               (error "zlib error, code ~d" res))
        (uffi:free-foreign-object destlen)))))

;; This code is based on the appropriate CL-PDF part
(defun compress-string (source)
  "Returns two values: array of bytes containing the compressed data
 and the number of compressed bytes"
  (let* ((sourcelen (length source))
	 (destsize (+ 12 (ceiling (* sourcelen 1.01))))
	 (dest (uffi:allocate-foreign-string destsize :unsigned t))
	 (destsize-ptr (uffi:allocate-foreign-object :long)))
    (setf (uffi:deref-pointer destsize-ptr :long) destsize)
    (uffi:with-cstring (source-cstr source)
      (let ((result (%gz-compress-string dest destsize-ptr source-cstr sourcelen))
	    (destlen (uffi:deref-pointer destsize-ptr :long)))
	(unwind-protect
	     (if (zerop result)
		 (values (uffi:convert-from-foreign-string 
			  dest
			  :length destlen
			  :null-terminated-p nil)
			 destlen)
		 (error "zlib error, code ~D" result))
	  (progn
	    (uffi:free-foreign-object destsize-ptr)
	    (uffi:free-foreign-object dest)))))))

(defun uncompress-string (source &optional destsize)
  "Uncompress the contents of the SOURCE byte array to a string. DESTSIZE 
is the length of the resulting string (it should be recorded during 
compression)."
  (when (string= source "")
    (return-from uncompress-string ""))

  (unless destsize
    (setf destsize (* *uncompress-buffer-scale-factor* (length source))))

  (assert (not (zerop destsize)))

  (let ((dest)
        (destsize-ptr (uffi:allocate-foreign-object :long))
        (srclen (length source)))
    (labels ((uncompress (src destsize)
               (setf dest (uffi:allocate-foreign-string destsize :unsigned t))
               (setf (uffi:deref-pointer destsize-ptr :long) destsize)
               (uffi:with-cstring (src-cstr src)
                 (let ((res (%gz-uncompress-string dest destsize-ptr src-cstr srclen)))
                   (cond ((= +z-ok+ res)
                          (uffi:convert-from-foreign-string 
                           dest
                           :length (uffi:deref-pointer destsize-ptr :long)
                           :null-terminated-p nil))
                         ((= +z-buf-error+ res)
                          (uffi:free-foreign-object dest)
                          (uncompress src (* *uncomress-buffer-scale-factor* destsize)))
                         (t
                          (error "zlib error, code ~d" res)))))))
      (unwind-protect
           (uncompress source destsize)
        (progn
          (uffi:free-foreign-object destsize-ptr)
          (when dest (uffi:free-foreign-object dest)))))))

;;;;
;;;; Reading and writing compressed files
;;;;

(uffi:def-function ("gzopen" %gz-open)
    ((path :cstring)
     (mode :cstring))
  :returning :int
  :module "zlib")

(defun gz-open (pathname &optional (direction :input))
  "Open a compressed stream to the given PATHNAME. The optional 
argument DIRECTION should be one of :input (the default) or 
:output. If an existing file is opened for output, it is truncated 
to zero length."
  (when (and (eq direction :input)
             (not (probe-file pathname)))
    (error "file ~a doesn't exists" pathname))
  (uffi:with-cstring (mode
                      (ecase direction
                        (:input "rb")
                        (:output "wb")))
    (uffi:with-cstring (path (namestring pathname))

      (%gz-open path mode))))

(uffi:def-function ("gzclose" gz-close)
    ((file :int))
  :returning :int
  :module "zlib")

(setf (documentation 'gz-close 'function) 
      "Close the compressed stream FILE-DESCRIPTOR.")

(uffi:def-function ("gzread" %gz-read)
    ((file :int)
     (buf (* :unsigned-char))
     (len :int))
  :returning :int
  :module "zlib")

(defun gz-read (file-descriptor len)
  "Read LEN uncompressed bytes from FILE-DESCRIPTOR of compressed file
opened with GZ-OPEN. If the input file was not in gzip format, GZ-READ
copies the given number of bytes. GZ-READ returns the string and number
of bytes actually read or FALSE in case of error."
  (let* ((buf (uffi:allocate-foreign-string len :unsigned t))
         (res (%gz-read file-descriptor buf len)))
    (unwind-protect
         (cond ((= +z-errno+ res) (values "" nil))
               ((= 0 res) (values "" 0))
               (t (values
                   (uffi:convert-from-foreign-string
                    buf
                    :length res
                    :null-terminated-p nil)
                   res)))
      (uffi:free-foreign-object buf))))

(uffi:def-function ("gzwrite" %gz-write)
    ((file :int)
     (buf (* :unsigned-char))
     (len :int))
  :returning :int
  :module "zlib")

(defun gz-write (file-descriptor str &optional (len (length str)))
  "Write STR into the FILE-DESCRIPTOR of compressed file
opened with GZ-OPEN.
Returns the number of uncompressed bytes actually written or FALSE
in case of error."
  (uffi:with-cstring (cstr str)
    (let ((res (%gz-write file-descriptor cstr (min len (length str)))))
      (if (zerop res) nil res))))

(uffi:def-function ("gzgets" %gz-read-line)
    ((file :int)
     (buf (* :unsigned-char))
     (len :int))
  :returning :int
  :module "zlib")

(defun gz-read-line (file-descriptor &optional (eof-error-p t) eof)
  "Read a line of text from the FILE-DESCRIPTOR of compressed file
opened with GZ-OPEN.
Returns the uncompressed string or
EOF and FALSE in case of error."
  (let* ((line-ends '(#\Return #\Newline))
         (destlen *read-buffer-length*)
         (dest (uffi:allocate-foreign-string destlen :unsigned t)))

    (labels ((buf-to-str (buf buflen)
               (uffi:convert-from-foreign-string
                buf
                :length buflen
                :null-terminated-p t))
             (concat-result (acc)
               (string-right-trim
                line-ends
                (apply #'concatenate 'string (nreverse acc))))
             (collect-line (acc)
               (if (zerop (%gz-read-line file-descriptor dest destlen))
                   (cond (acc ; subsequent call
                          (concat-result acc))
                         (eof-error-p (error 'end-of-file))
                         (t eof))
                   (let* ((str (buf-to-str dest destlen))
                          (strlen (length str))
                          (newacc (cons str acc)))
                     (if (and (= strlen (- destlen 1))
                              (not (member (char str (- strlen 1)) line-ends)))
                         (collect-line newacc) ; line greater than buffer
                         (concat-result newacc))))))
      (unwind-protect
           (progn
             (assert (> destlen 1))
             (collect-line '()))
        (uffi:free-foreign-object dest)))))

(uffi:def-function ("gzputs" %gz-write-line)
    ((file :int)
     (str :cstring))
  :returning :int    ; number of chars written
  :module "zlib")

(defun gz-write-line (file-descriptor str)
  "Write the string STR to the FILE-DESCRIPTOR of compressed file
opened with GZ-OPEN.
Returns the number of characters written, or FALSE in case of error."
  (uffi:with-cstring (cstr str)
    (let ((res (%gz-write-line file-descriptor cstr)))
      (if (= +z-errno+ res) nil res))))


;;;;
;;;; High-level macro
;;;;

(defmacro with-gz ((fd filename &key (direction :input)) &body forms)
  "Open FILENAME using gz-open with the specified DIRECTION, assign
the open file to FD and exectute FORMS."
  (let ((fname (gensym)))
    `(let* ((,fname ,filename)
	    (,fd (zlib:gz-open ,fname ,direction)))
       (if (zerop ,fd)
	   (error "Could not open compressed file ~a." ,fname)
           (unwind-protect
                (progn ,@forms)
             (zlib:gz-close ,fd))))))

