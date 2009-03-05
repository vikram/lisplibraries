;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: FLEXI-STREAMS; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/flexi-streams/output.lisp,v 1.37 2007/02/19 07:48:00 edi Exp $

;;; Copyright (c) 2005-2007, Dr. Edmund Weitz.  All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :flexi-streams)

#-:lispworks
(defmethod write-byte* (byte (flexi-output-stream flexi-output-stream))
  "Writes one byte \(octet) to the underlying stream of
FLEXI-OUTPUT-STREAM."
  (declare (optimize speed))
  (with-accessors ((stream flexi-stream-stream))
      flexi-output-stream
    (write-byte byte stream)))

#+:lispworks
(defmethod write-byte* (byte (flexi-output-stream flexi-output-stream))
  "Writes one byte \(octet) to the underlying stream of
FLEXI-OUTPUT-STREAM."
  (declare (optimize speed))
  ;; we use WRITE-SEQUENCE because WRITE-BYTE doesn't work with all
  ;; bivalent streams in LispWorks (4.4.6)
  (with-accessors ((stream flexi-stream-stream))
      flexi-output-stream
    (write-sequence (make-array 1 :element-type 'octet
                                :initial-element byte)
                    stream)
    byte))

#+:lispworks
(defmethod write-byte* (byte (flexi-output-stream flexi-binary-output-stream))
  "Writes one byte \(octet) to the underlying stream of
FLEXI-OUTPUT-STREAM.  Optimized version \(only needed for LispWorks)
in case the underlying stream is binary."
  (declare (optimize speed))
  (with-accessors ((stream flexi-stream-stream))
      flexi-output-stream
    (write-byte byte stream)))
            
(defmethod stream-write-char ((stream flexi-latin-1-output-stream) char)
  (declare (optimize speed))
  (let ((octet (char-code char)))
    (when (> octet 255)
      (signal-encoding-error stream "~S is not a LATIN-1 character." char))
    (write-byte* octet stream))
  char)

(defmethod stream-write-char ((stream flexi-ascii-output-stream) char)
  (declare (optimize speed))
  (let ((octet (char-code char)))
    (when (> octet 127)
      (signal-encoding-error stream "~S is not an ASCII character." char))
    (write-byte* octet stream))
  char)

(defmethod stream-write-char ((stream flexi-8-bit-output-stream) char)
  (declare (optimize speed))
  (with-accessors ((encoding-hash flexi-stream-encoding-hash))
      stream
    (let ((octet (gethash (char-code char) encoding-hash)))
      (unless octet
        (signal-encoding-error stream "~S is not in this encoding." char))
      (write-byte* octet stream))
    char))

(defmethod stream-write-char ((stream flexi-utf-8-output-stream) char)
  (declare (optimize speed))
  (let ((char-code (char-code char)))
    (tagbody
     (cond ((< char-code #x80)
            (write-byte* char-code stream)
            (go zero))
           ((< char-code #x800)
            (write-byte* (logior #b11000000 (ldb (byte 5 6) char-code)) stream)
            (go one))
           ((< char-code #x10000)
            (write-byte* (logior #b11100000 (ldb (byte 4 12) char-code)) stream)
            (go two))
           ((< char-code #x200000)
            (write-byte* (logior #b11110000 (ldb (byte 3 18) char-code)) stream)
            (go three))
           ((< char-code #x4000000)
            (write-byte* (logior #b11111000 (ldb (byte 2 24) char-code)) stream)
            (go four))
           (t (write-byte* (logior #b11111100 (ldb (byte 1 30) char-code)) stream)))
     (write-byte* (logior #b10000000 (ldb (byte 6 24) char-code)) stream)
     four
     (write-byte* (logior #b10000000 (ldb (byte 6 18) char-code)) stream)
     three
     (write-byte* (logior #b10000000 (ldb (byte 6 12) char-code)) stream)
     two
     (write-byte* (logior #b10000000 (ldb (byte 6 6) char-code)) stream)
     one
     (write-byte* (logior #b10000000 (ldb (byte 6 0) char-code)) stream)
     zero))
  char)

(defmethod stream-write-char ((stream flexi-utf-16-le-output-stream) char)
  (declare (optimize speed))
  (flet ((write-word (word)
           (write-byte* (ldb (byte 8 0) word) stream)
           (write-byte* (ldb (byte 8 8) word) stream)))
    (declare (inline write-word) (dynamic-extent (function write-word)))
    (let ((char-code (char-code char)))
      (cond ((< char-code #x10000)
             (write-word char-code))
            (t (decf char-code #x10000)
               (write-word (logior #xd800 (ldb (byte 10 10) char-code)))
               (write-word (logior #xdc00 (ldb (byte 10 0) char-code)))))))
  char)

(defmethod stream-write-char ((stream flexi-utf-16-be-output-stream) char)
  (declare (optimize speed))
  (flet ((write-word (word)
           (write-byte* (ldb (byte 8 8) word) stream)
           (write-byte* (ldb (byte 8 0) word) stream)))
    (declare (inline write-word) (dynamic-extent (function write-word)))
    (let ((char-code (char-code char)))
      (cond ((< char-code #x10000)
             (write-word char-code))
            (t (decf char-code #x10000)
               (write-word (logior #xd800 (ldb (byte 10 10) char-code)))
               (write-word (logior #xdc00 (ldb (byte 10 0) char-code)))))))
  char)

(defmethod stream-write-char ((stream flexi-utf-32-le-output-stream) char)
  (declare (optimize speed))
  (loop with char-code = (char-code char)
        for position in '(0 8 16 24)
        do (write-byte* (ldb (byte 8 position) char-code) stream))
  char)

(defmethod stream-write-char ((stream flexi-utf-32-be-output-stream) char)
  (declare (optimize speed))
  (loop with char-code = (char-code char)
        for position in '(24 16 8 0)
        do (write-byte* (ldb (byte 8 position) char-code) stream))
  char)

(defmethod stream-write-char ((stream flexi-cr-mixin) char)
  "The `base' method for all streams which need end-of-line
conversion.  Uses CALL-NEXT-METHOD to do the actual work of
writing one or more characters to the stream."
  (declare (optimize speed))
  (with-accessors ((external-format flexi-stream-external-format))
      stream
    (case char
      (#\Newline     
       (case (external-format-eol-style external-format)
         (:cr (call-next-method stream #\Return))
         (:crlf (call-next-method stream #\Return)
          (call-next-method stream #\Linefeed))))
      (otherwise (call-next-method)))
    char))

(defmethod stream-write-char :after ((stream flexi-output-stream) char)
  (declare (optimize speed))
  ;; update the column unless we're in the middle of the line and
  ;; the current value is NIL
  (with-accessors ((column flexi-stream-column))
      stream
    (cond ((char= char #\Newline) (setq column 0))
          (column (incf (the integer column))))))

(defmethod stream-clear-output ((flexi-output-stream flexi-output-stream))
  "Simply calls the corresponding method for the underlying
output stream."
  (declare (optimize speed))
  (with-accessors ((stream flexi-stream-stream))
      flexi-output-stream
    (clear-output stream)))

(defmethod stream-finish-output ((flexi-output-stream flexi-output-stream))
  "Simply calls the corresponding method for the underlying
output stream."
  (declare (optimize speed))
  (with-accessors ((stream flexi-stream-stream))
      flexi-output-stream
    (finish-output stream)))

(defmethod stream-force-output ((flexi-output-stream flexi-output-stream))
  "Simply calls the corresponding method for the underlying
output stream."
  (declare (optimize speed))
  (with-accessors ((stream flexi-stream-stream))
      flexi-output-stream
    (force-output stream)))

(defmethod stream-line-column ((flexi-output-stream flexi-output-stream))
  "Returns the column stored in the COLUMN slot of the
FLEXI-OUTPUT-STREAM object STREAM."
  (declare (optimize speed))
  (with-accessors ((column flexi-stream-column))
      flexi-output-stream
    column))

(defmethod stream-write-byte ((flexi-output-stream flexi-output-stream) byte)
  "Writes a byte \(octet) to the underlying stream."
  (declare (optimize speed))
  ;; set column to NIL because we don't know how to handle binary
  ;; output mixed with character output
  (with-accessors ((column flexi-stream-column))
      flexi-output-stream
    (setq column nil)
    (write-byte* byte flexi-output-stream)))

#+:allegro
(defmethod stream-terpri ((stream flexi-output-stream))
  "Writes a #\Newline character to the underlying stream."
  (declare (optimize speed))
  ;; needed for AllegroCL - grrr...
  (stream-write-char stream #\Newline))

(defmethod stream-write-sequence ((flexi-output-stream flexi-output-stream) sequence start end &key)
  "Writes all elements of the sequence SEQUENCE from START to END
to the underlying stream.  The elements can be either octets or
characters.  Characters are output according to the current
encoding \(external format) of the FLEXI-OUTPUT-STREAM object
STREAM."
  (declare (optimize speed) (type (integer 0 *) start end))
  (with-accessors ((stream flexi-stream-stream))
      flexi-output-stream
    (cond ((and (arrayp sequence)
                (subtypep (array-element-type sequence) 'octet))
           (write-sequence sequence stream :start start :end end))
          (t (loop for index from start below end
                   for element = (elt sequence index)
                   when (characterp element)
                   do (stream-write-char flexi-output-stream element)
                   else
                   do (stream-write-byte flexi-output-stream element))
             sequence))))
