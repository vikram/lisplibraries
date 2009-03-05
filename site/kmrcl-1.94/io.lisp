;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          io.lisp
;;;; Purpose:       Input/Output functions for KMRCL package
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Apr 2000
;;;;
;;;; $Id: io.lisp 11317 2006-11-26 19:04:23Z kevin $
;;;;
;;;; This file, part of KMRCL, is Copyright (c) 2002-2003 by Kevin M. Rosenberg
;;;;
;;;; KMRCL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:kmrcl)

(defun print-file-contents (file &optional (strm *standard-output*))
  "Opens a reads a file. Returns the contents as a single string"
  (when (probe-file file)
    (let ((eof (cons 'eof nil)))
      (with-open-file (in file :direction :input)
        (do ((line (read-line in nil eof) 
                   (read-line in nil eof)))
            ((eq line eof))
          (write-string line strm)
          (write-char #\newline strm))))))

(defun read-stream-to-string (in)
  (with-output-to-string (out)
    (let ((eof (gensym)))		    
      (do ((line (read-line in nil eof) 
		 (read-line in nil eof)))
	  ((eq line eof))
	(format out "~A~%" line)))))
	
(defun read-file-to-string (file)
  "Opens a reads a file. Returns the contents as a single string"
  (with-open-file (in file :direction :input)
    (read-stream-to-string in)))

(defun read-file-to-usb8-array (file)
  "Opens a reads a file. Returns the contents as single unsigned-byte array"
  (with-open-file (in file :direction :input :element-type '(unsigned-byte 8))
    (let* ((file-len (file-length in))
	   (usb8 (make-array file-len :element-type '(unsigned-byte 8)))
	   (pos (read-sequence usb8 in)))
      (unless (= file-len pos)
	(error "Length read (~D) doesn't match file length (~D)~%" pos file-len))
      usb8)))
      

(defun read-stream-to-strings (in)
  (let ((lines '())
	(eof (gensym)))		    
    (do ((line (read-line in nil eof) 
	       (read-line in nil eof)))
	((eq line eof))
      (push line lines))
    (nreverse lines)))
    
(defun read-file-to-strings (file)
  "Opens a reads a file. Returns the contents as a list of strings"
  (with-open-file (in file :direction :input)
    (read-stream-to-strings in)))

(defun file-subst (old new file1 file2)
  (with-open-file (in file1 :direction :input)
    (with-open-file (out file2 :direction :output
			 :if-exists :supersede)
      (stream-subst old new in out))))

(defun print-n-chars (char n stream)
  (declare (fixnum n) (optimize (speed 3) (safety 0) (space 0)))
  (dotimes (i n)
    (declare (fixnum i))
    (write-char char stream)))

(defun print-n-strings (str n stream)
  (declare (fixnum n) (optimize (speed 3) (safety 0) (space 0)))
  (dotimes (i n)
    (declare (fixnum i))
    (write-string str stream)))

(defun indent-spaces (n &optional (stream *standard-output*))
  "Indent n*2 spaces to output stream"
  (print-n-chars #\space (+ n n) stream))


(defun indent-html-spaces (n &optional (stream *standard-output*))
  "Indent n*2 html spaces to output stream"
  (print-n-strings "&nbsp;" (+ n n) stream))
     

(defun print-list (l &optional (output *standard-output*))
  "Print a list to a stream"
  (format output "~{~A~%~}" l))

(defun print-rows (rows &optional (ostrm *standard-output*))
  "Print a list of list rows to a stream"  
  (dolist (r rows) (format ostrm "~{~A~^ ~}~%" r)))


;; Buffered stream substitute

(defstruct buf
  vec (start -1) (used -1) (new -1) (end -1))

(defun bref (buf n)
  (svref (buf-vec buf)
         (mod n (length (buf-vec buf)))))

(defun (setf bref) (val buf n)
  (setf (svref (buf-vec buf)
               (mod n (length (buf-vec buf))))
        val))

(defun new-buf (len)
  (make-buf :vec (make-array len)))

(defun buf-insert (x b)
  (setf (bref b (incf (buf-end b))) x))

(defun buf-pop (b)
  (prog1 
    (bref b (incf (buf-start b)))
    (setf (buf-used b) (buf-start b)
          (buf-new  b) (buf-end   b))))

(defun buf-next (b)
  (when (< (buf-used b) (buf-new b))
    (bref b (incf (buf-used b)))))

(defun buf-reset (b)
  (setf (buf-used b) (buf-start b)
        (buf-new  b) (buf-end   b)))

(defun buf-clear (b)
  (setf (buf-start b) -1 (buf-used  b) -1
        (buf-new   b) -1 (buf-end   b) -1))

(defun buf-flush (b str)
  (do ((i (1+ (buf-used b)) (1+ i)))
      ((> i (buf-end b)))
    (princ (bref b i) str)))


(defun stream-subst (old new in out)
  (declare (string old new))
  (let* ((pos 0)
         (len (length old))
         (buf (new-buf len))
         (from-buf nil))
    (declare (fixnum pos len))
    (do ((c (read-char in nil :eof)
            (or (setf from-buf (buf-next buf))
                (read-char in nil :eof))))
        ((eql c :eof))
      (declare (character c))
      (cond ((char= c (char old pos))
             (incf pos)
             (cond ((= pos len)            ; 3
                    (princ new out)
                    (setf pos 0)
                    (buf-clear buf))
                   ((not from-buf)         ; 2
                    (buf-insert c buf))))
            ((zerop pos)                   ; 1
             (princ c out)
             (when from-buf
               (buf-pop buf)
               (buf-reset buf)))
            (t                             ; 4
             (unless from-buf
               (buf-insert c buf))
             (princ (buf-pop buf) out)
             (buf-reset buf)
             (setf pos 0))))
    (buf-flush buf out)))

(declaim (inline write-fixnum))
(defun write-fixnum (n s)
  #+allegro (excl::print-fixnum s 10 n)
  #-allegro (write-string (write-to-string n) s))




(defun null-output-stream ()
  (when (probe-file #p"/dev/null")
    (open #p"/dev/null" :direction :output :if-exists :overwrite))  
  )


(defun directory-tree (filename)
  "Returns a tree of pathnames for sub-directories of a directory"
  (let* ((root (canonicalize-directory-name filename))
	 (subdirs (loop for path in (directory
				     (make-pathname :name :wild
						    :type :wild
						    :defaults root))
			when (probe-directory path)
			collect (canonicalize-directory-name path))))
    (when (find nil subdirs)
      (error "~A" subdirs))
    (when (null root)
      (error "~A" root))
    (if subdirs
	(cons root (mapcar #'directory-tree subdirs))
	(if (probe-directory root)
	    (list root)
	    (error "root not directory ~A" root)))))


(defmacro with-utime-decoding ((utime &optional zone) &body body)
  "UTIME is a universal-time, ZONE is a number of hours offset from UTC, or NIL to use local time.  Execute BODY in an environment where SECOND MINUTE HOUR DAY-OF-MONTH MONTH YEAR DAY-OF-WEEK DAYLIGHT-P ZONE are bound to the decoded components of the universal time"
  `(multiple-value-bind
       (second minute hour day-of-month month year day-of-week daylight-p zone)
       (decode-universal-time ,utime ,@(if zone (list zone)))
     (declare (ignorable second minute hour day-of-month month year day-of-week daylight-p zone))
     ,@body))

(defvar +datetime-number-strings+ 
  (make-array 61 :adjustable nil :element-type 'string :fill-pointer nil
	      :initial-contents 
	      '("00" "01" "02" "03" "04" "05" "06" "07" "08" "09" "10" "11"
		"12" "13" "14" "15" "16" "17" "18" "19" "20" "21" "22" "23"
		"24" "25" "26" "27" "28" "29" "30" "31"	"32" "33" "34" "35"
		"36" "37" "38" "39" "40" "41" "42" "43" "44" "45" "46" "47"
		"48" "49" "50" "51" "52" "53" "54" "55" "56" "57" "58" "59"
		"60")))

(defun is-dst (utime)
  (with-utime-decoding (utime)
    daylight-p))


(defmacro with-utime-decoding-utc-offset ((utime utc-offset) &body body)
  (with-gensyms (zone)
    `(let* ((,zone (cond
		    ((eq :utc ,utc-offset) 
		     0)
		    ((null utc-offset)
		     nil)
		    (t
		     (if (is-dst ,utime)
			 (1- (- ,utc-offset))
		       (- ,utc-offset))))))
       (if ,zone
	   (with-utime-decoding (,utime ,zone)
	     ,@body)
	 (with-utime-decoding (,utime)
	   ,@body)))))


(defun write-utime-hms (utime &key utc-offset stream)
  (if stream
      (write-utime-hms-stream utime stream utc-offset)
    (with-output-to-string (s)
      (write-utime-hms-stream utime s utc-offset))))

(defun write-utime-hms-stream (utime stream &optional utc-offset)
  (with-utime-decoding-utc-offset (utime utc-offset)
    (write-string (aref +datetime-number-strings+ hour) stream)
    (write-char #\: stream)
    (write-string (aref +datetime-number-strings+ minute) stream)
    (write-char #\: stream)
    (write-string (aref +datetime-number-strings+ second) stream)))

(defun write-utime-hm (utime &key utc-offset stream)
  (if stream
      (write-utime-hm-stream utime stream utc-offset)
    (with-output-to-string (s)
      (write-utime-hm-stream utime s utc-offset))))

(defun write-utime-hm-stream (utime stream &optional utc-offset)
  (with-utime-decoding-utc-offset (utime utc-offset)
    (write-string (aref +datetime-number-strings+ hour) stream)
    (write-char #\: stream)
    (write-string (aref +datetime-number-strings+ minute) stream)))


(defun write-utime-ymdhms (utime &key stream utc-offset)
  (if stream
      (write-utime-ymdhms-stream utime stream utc-offset)
    (with-output-to-string (s)
      (write-utime-ymdhms-stream utime s utc-offset))))

(defun write-utime-ymdhms-stream (utime stream &optional utc-offset)
  (with-utime-decoding-utc-offset (utime utc-offset)
    (write-string (prefixed-fixnum-string year nil 4) stream)
    (write-char #\/ stream)
    (write-string (aref +datetime-number-strings+ month) stream)
    (write-char #\/ stream)
    (write-string (aref +datetime-number-strings+ day-of-month) stream)
    (write-char #\space stream)
    (write-string (aref +datetime-number-strings+ hour) stream)
    (write-char #\: stream)
    (write-string (aref +datetime-number-strings+ minute) stream)
    (write-char #\: stream)
    (write-string (aref +datetime-number-strings+ second) stream)))

(defun write-utime-ymdhm (utime &key stream utc-offset)
  (if stream
      (write-utime-ymdhm-stream utime stream utc-offset)
    (with-output-to-string (s)
      (write-utime-ymdhm-stream utime s utc-offset))))

(defun write-utime-ymdhm-stream (utime stream &optional utc-offset)
  (with-utime-decoding-utc-offset (utime utc-offset)
    (write-string (prefixed-fixnum-string year nil 4) stream)
    (write-char #\/ stream)
    (write-string (aref +datetime-number-strings+ month) stream)
    (write-char #\/ stream)
    (write-string (aref +datetime-number-strings+ day-of-month) stream)
    (write-char #\space stream)
    (write-string (aref +datetime-number-strings+ hour) stream)
    (write-char #\: stream)
    (write-string (aref +datetime-number-strings+ minute) stream)))

(defun copy-binary-stream (in out &key (chunk-size 16384))
  (do* ((buf (make-array chunk-size :element-type '(unsigned-byte 8)))
	(pos (read-sequence buf in) (read-sequence buf in)))
      ((zerop pos))
    (write-sequence buf out :end pos)))

