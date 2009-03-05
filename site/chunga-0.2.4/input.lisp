;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CHUNGA; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/chunga/input.lisp,v 1.12 2007/01/01 23:39:36 edi Exp $

;;; Copyright (c) 2006-2007, Dr. Edmund Weitz.  All rights reserved.

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

(in-package :chunga)

(defmethod chunked-input-stream-extensions (object)
  "The default method which always returns the empty list."
  nil)

(defmethod chunked-input-stream-trailers (object)
  "The default method which always returns the empty list."
  nil)

(defmethod chunked-stream-input-chunking-p (object)
  "The default method for all objects which are not of type
CHUNKED-INPUT-STREAM."
  nil)

(defmethod (setf chunked-stream-input-chunking-p) (new-value (stream chunked-input-stream))
  "Switches input chunking for STREAM on or off."
  (unless (eq (not new-value) (not (chunked-stream-input-chunking-p stream)))
    (with-slots (input-limit input-index expecting-crlf-p chunk-extensions chunk-trailers input-stream)
        stream
      (cond (new-value
             (setq ;; wrap with flexi stream for header parsing
                   input-stream (make-flexi-stream input-stream :external-format +latin-1+)
                   expecting-crlf-p nil
                   input-limit 0
                   input-index 0
                   chunk-extensions nil
                   chunk-trailers nil))
            (t (when (< input-index input-limit)
                 (error "Not all chunks from ~S have been read completely."
                        stream))
               ;; switch back to original inner stream
               (setq input-stream (flexi-stream-stream input-stream))))))
  (setf (slot-value stream 'input-chunking-p) new-value))

(defmethod stream-clear-input ((stream chunked-input-stream))
  "Implements CLEAR-INPUT by resetting the internal chunk buffer."
  (when (chunked-stream-input-chunking-p stream)
    (setf (chunked-stream-input-index stream) 0
          (chunked-stream-input-limit stream) 0))
  ;; clear input on inner stream
  (clear-input (chunked-stream-input-stream stream))
  nil)

(defmethod chunked-input-available-p ((stream chunked-input-stream))
  "Whether there's unread input waiting in the chunk buffer."
  (< (chunked-stream-input-index stream)
     (chunked-stream-input-limit stream)))

(defmethod stream-listen ((stream chunked-input-stream))
  "We first check if input chunking is enabled and if there's
something in the buffer.  Otherwise we poll the underlying stream."
  (cond ((and (chunked-stream-input-chunking-p stream)
              (chunked-input-available-p stream)))
        (t (listen (chunked-stream-input-stream stream)))))

(defmethod fill-buffer ((stream chunked-input-stream))
  "Re-fills the chunk buffer.  Returns NIL if chunking has ended."
  (let ((inner-stream (chunked-stream-input-stream stream))
        ;; set up error function for the functions in `read.lisp'
        (*current-error-function*
         (lambda (last-char expected-chars)
             "The function which is called when an unexpected
character is seen.  Signals INPUT-CHUNKING-BODY-CORRUPTED."
             (error 'input-chunking-body-corrupted
                    :stream stream
                    :last-char last-char
                    :expected-chars expected-chars))))
    (labels ((add-extensions ()
               "Reads chunk extensions \(if there are any) and stores
them into the corresponding slot of the stream."
               (when-let (extensions (read-name-value-pairs inner-stream))
                 (warn "Adding uninterpreted extensions to stream ~S." stream)
                 (setf (slot-value stream 'chunk-extensions)
                       (append (chunked-input-stream-extensions stream) extensions)))
               (assert-crlf inner-stream))
             (get-chunk-size ()
               "Reads chunk size header \(including optional
extensions) and returns the size."
               #+:clisp (setf (flexi-stream-element-type inner-stream) 'character)
               (when (expecting-crlf-p stream)
                 (assert-crlf inner-stream))
               (setf (expecting-crlf-p stream) t)
               ;; read hexadecimal number
               (let (last-char)
                 (prog1 (loop for weight = (digit-char-p (setq last-char
                                                               (read-char inner-stream))
                                                         16)
                              for result = (if weight
                                             (+ weight (* 16 (or result 0)))
                                             (return result)))
                   ;; unread first octet which wasn't a digit
                   (unread-char last-char inner-stream)
                   (add-extensions)
                   #+:clisp (setf (flexi-stream-element-type inner-stream) 'octet)))))
      (let ((chunk-size (get-chunk-size)))
        (with-slots (input-buffer input-limit input-index)
            stream
          (setq input-index 0
                input-limit chunk-size)
          (cond ((zerop chunk-size)
                 ;; turn chunking off
                 (setf (chunked-stream-input-chunking-p stream) nil
                       (slot-value stream 'chunk-trailers) (read-http-headers inner-stream)
                       input-limit 0)
                 ;; return NIL
                 (return-from fill-buffer))
                ((> chunk-size (length input-buffer))
                 ;; replace buffer if it isn't big enough for the next chunk
                 (setq input-buffer (make-array chunk-size :element-type 'octet))))
          (unless (= (read-sequence input-buffer inner-stream
                                    :start 0 :end chunk-size)
                     chunk-size)
            (error 'input-chunking-unexpected-end-of-file
                   :stream stream))
          chunk-size)))))

(defmethod stream-read-byte ((stream chunked-input-stream))
  "Reads one byte from STREAM.  Checks the chunk buffer first, if
input chunking is enabled.  Re-fills buffer is necessary."
  (unless (chunked-stream-input-chunking-p stream)
    (return-from stream-read-byte (read-byte (chunked-stream-input-stream stream) nil :eof)))
  (unless (chunked-input-available-p stream)
    (unless (fill-buffer stream)
      (return-from stream-read-byte :eof)))
  (with-slots (input-buffer input-index)
      stream
    (prog1 (aref input-buffer input-index)
      (incf input-index))))

(defmethod stream-read-sequence ((stream chunked-input-stream) sequence start end &key)
  "Fills SEQUENCE by adding data from the chunk buffer and re-filling
it until enough data was read.  Works directly on the underlying
stream if input chunking is off."
  (unless (chunked-stream-input-chunking-p stream)
    (return-from stream-read-sequence
      (read-sequence sequence (chunked-stream-input-stream stream) :start start :end end)))
  (loop
   (when (>= start end)
     (return-from stream-read-sequence start))   
   (unless (chunked-input-available-p stream)
     (unless (fill-buffer stream)
       (return-from stream-read-sequence start)))
   (with-slots (input-buffer input-limit input-index)
       stream
     (replace sequence input-buffer
              :start1 start :end1 end
              :start2 input-index :end2 input-limit)
     (let ((length (min (- input-limit input-index)
                        (- end start))))
       (incf start length)
       (incf input-index length)))))
