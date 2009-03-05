;; This software is Copyright (c) 2006, Steffen Siering.
;; <COMPANY> grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :cffi-util)

;;;; * C buffer streams

;;using flexi-streams in-memory streams to model streams upon cbuffer :)

;;mixin
(defclass cbuffer-stream-mixin ()
  ((pos :accessor stream-position
	:initarg :position
	:initform 0)
   (cbuffer :accessor stream-cbuffer
	    :initarg :cbuffer
	    :initform (make-cbuffer))))

;;;calling close on one of these streams will free the cbuffer. You may...
;;;- ...use the cbuffer any time later if you don't close the stream
;;;     (don't forget to free it, when cbuffer was created using make-cbuffer,
;;;      if you created the cbuffer with make-cbuffer-from-pointer than
;;;      there is no need to free any resources unless you have to release the pointer)
;;;- ...use one cbuffer between more then one streams.
(defclass cbuffer-input-stream (cbuffer-stream-mixin in-memory-input-stream)
  ())

;;;be carefull when using a not self allocated cbuffer with outputstream,
;;;since writing may allocate a new buffer thus you finally have to free resources...
(defclass cbuffer-output-stream (cbuffer-stream-mixin in-memory-output-stream)
  ())

(defmethod (setf stream-position) (value (stream cbuffer-stream-mixin))
  (cbuffer-checkbounds (stream-cbuffer stream) value t)
  (setf (slot-value stream 'pos) value))

(defmethod open-stream-p ((stream cbuffer-stream-mixin))
  (not (null (stream-cbuffer stream))))

(defmethod stream-check-eof-no-hang ((stream cbuffer-stream-mixin))
  (if (= (stream-position stream)
	 (cbuffer-size (stream-cbuffer stream)))
      :eof
      nil))

(defmethod close ((stream cbuffer-stream-mixin) &key abort)
  (declare (ignore abort))
  (free-cbuffer (stream-cbuffer stream))
  t)

(defun stream-eof-p (stream)
  (eq (stream-check-eof-no-hang stream) :eof))

;;input functions
(defmethod stream-read-byte ((stream cbuffer-input-stream))
  (if (stream-eof-p stream)
      :eof
      (with-slots (pos cbuffer) stream
	(let ((byte (cbuffer-byte cbuffer pos)))
	  (incf pos)
	  byte))))

(defmethod stream-listen ((stream cbuffer-input-stream))
  (not (stream-eof-p stream)))

(defmethod stream-read-sequence ((stream cbuffer-input-stream) sequence
				 start end &key)
  (with-slots (pos cbuffer) stream
    (loop for index from start below end
	  while (< pos (cbuffer-size cbuffer))
	  do (setf (elt sequence index) (cbuffer-byte cbuffer pos)
		   pos (1+ pos))
	  finally (return index))))
;;output functions

(defmethod stream-write-byte ((stream cbuffer-output-stream) byte)
  (with-slots (pos (buf cbuffer)) stream
    (cbuffer-ensure-size buf (1+ pos))
    (setf (cbuffer-byte buf pos) byte
	  pos (1+ pos))
    (cbuffer-adjust-min-size buf pos))
  byte)

(defmethod stream-write-sequence ((stream cbuffer-output-stream) sequence
				  start end &key)
  (with-slots (pos (buf cbuffer)) stream
    (cbuffer-ensure-size buf (+ pos (- end start)))
    (loop for index from start below end
	  do (setf (cbuffer-byte buf pos) (elt sequence index)
		   pos (1+ pos))
	  finally (return sequence))))

;;input-stream-p and output-stream-p are builtin functions in clisp

#-clisp
(defmethod input-stream-p ((stream cbuffer-input-stream))
  t)

#-clisp
(defmethod input-stream-p ((stream cbuffer-output-stream))
  nil)

#-clisp
(defmethod output-stream-p ((stream cbuffer-input-stream))
  nil)

#-clisp
(defmethod output-stream-p ((stream cbuffer-output-stream))
  t)

;;;use flexi-streams instead of in-memory streams...

(defun make-cbuffer-input-stream (cbuffer
				  &key
				  (external-format (make-external-format
						    :latin1)))
  (make-flexi-stream (make-instance 'cbuffer-input-stream
				    :cbuffer cbuffer)
		     :external-format external-format))

(defun make-cbuffer-output-stream (&key
				   cbuffer
				   (position 0)
				   (external-format (make-external-format
						     :latin1)))
  (make-flexi-stream (make-instance 'cbuffer-output-stream
				    :position position
				    :cbuffer (if cbuffer
						 cbuffer
						 (make-cbuffer)))
		     :external-format external-format))

(defmethod stream-cbuffer ((stream flexi-stream))
  (stream-cbuffer (flexi-stream-stream stream)))

(defmethod stream-position ((stream flexi-stream))
  (stream-position (flexi-stream-stream stream)))

(defmethod (setf stream-position) (pos (stream flexi-stream))
  (setf (stream-position (flexi-stream-stream stream)) pos))
