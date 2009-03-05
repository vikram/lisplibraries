;; This software is Copyright (c) 2006, Steffen Siering.
;; <COMPANY> grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :cffi-util)

;;;; * C buffers

(defstruct cbuffer
  (data (foreign-alloc :uchar :count 16))
  (size 0)
  (length 16))

;;cbuffer pool

(defvar *use-cbuffer-pool* t "...")

;;max std. pool size: 16 * 4MB = 64MB
(defvar *cbuffer-pool-max-size* 16 "max num of buffers in pool")

(defvar *cbuffer-pool-min-buffer-size* 1024 "min size of buffers in pool")

(defvar *cbuffer-pool-max-buffer-size* (* 4 1024 1024)) ;;4MB

(defun make-cbuffer-pool ()
  "make-cbuffer-pool creates a priority-pool using the length
   of buffers as priority... Largest Buffers have highest priority"
  (let* ((pool (make-priority-pool :key-fn #'cbuffer-length
				   :comparator (build-comparator #'= :< #'>)))
	 (std-add-strategie (pool-add-strategie pool)))
    (setf (pool-add-strategie pool)
	  (lambda (obj store)
	    (when (and *use-cbuffer-pool*
		       (< (pool-count pool) *cbuffer-pool-max-size*)
		       (>= (cbuffer-length obj)
			   *cbuffer-pool-min-buffer-size*)
		       (<= (cbuffer-length obj)
			   *cbuffer-pool-max-buffer-size*))
	      (funcall std-add-strategie obj store))))
    pool))

(defparameter *cbuffer-pool* (make-cbuffer-pool))

(defun clear-cbuffer-pool ()
  (unless (empty-pool-p *cbuffer-pool*)
    (free-cbuffer-final (pool-next *cbuffer-pool*))
    (clear-cbuffer-pool)))
;;

(defcfun "memcpy"
    :pointer
  (dest :pointer)
  (src :pointer)
  (length :int))

(defun cbuffer-gain ()
  (if-bind buf (pool-next *cbuffer-pool*)
	   (progn
	     (setf (cbuffer-size buf) 0)
	     buf)
	   (make-cbuffer)))

(defun alloc-cbuffer ()
  (cbuffer-gain))

(defun make-cbuffer-from-pointer (ptr size)
  (make-cbuffer :data ptr :size size :length size))

(defun cbuffer-eob-p (buf)
  (= (cbuffer-size buf)
     (cbuffer-length buf)))

(defun cbuffer-resize (buf new-size)
  "resizes cbuffer by creating a new one and copying the data along.
   if new-size < size then buffer may be truncated"
  (let ((old-data (cbuffer-data buf))
	(old-length (cbuffer-length buf))
	(new-data (foreign-alloc :uchar :count new-size))
	(size (if  (<= (cbuffer-size buf) new-size)
		   (cbuffer-size buf)
		   new-size)))
    (setf (cbuffer-data buf) (memcpy new-data (cbuffer-data buf) size)
	  (cbuffer-length buf) new-size)
    ;;try to add the old data-buffer to pool
    (free-cbuffer (make-cbuffer-from-pointer old-data old-length))
    buf))

(defun cbuffer-ensure-size (buf size &key (load-factor 3/4))
  (let ((length (cbuffer-length buf))
	(new-size (round (* size (1+ load-factor)))))
    (if (< length new-size)
	(cbuffer-resize buf new-size)
	buf)
    (setf (cbuffer-size buf) size)))

(defun cbuffer-adjust-min-size (buf size)
  "WARNING: size must be smaller then length!!!
            use cbuffer-ensure-size before using cbuffer-adjust-min-size 
            for example"
  (if (> size (cbuffer-size buf))
      (setf (cbuffer-size buf) size)))

(defun free-cbuffer-final (buf)
  (foreign-free (cbuffer-data buf))
  (setf (cbuffer-data buf) nil)
  t)

(defun free-cbuffer (buf)
  (when (and (cbuffer-p buf) (cbuffer-data buf))
    (if (pool-add buf *cbuffer-pool*) t (free-cbuffer-final buf))))

(defun cbuffer-checkbounds (buf pos &optional error)
  (if (or (>= pos (cbuffer-size buf))
	  (< pos 0))
      (cond ((functionp error) (funcall error))
	    (error (error "buffer out of bounds exception."))
	    (t nil))
      t))

(defun cbuffer-byte (buf pos)
  (when (cbuffer-checkbounds buf pos t)     
      (mem-aref (cbuffer-data buf) :uchar pos)))

(defun (setf cbuffer-byte) (value buf pos)
  (when (cbuffer-checkbounds buf pos t)
    (setf (mem-aref (cbuffer-data buf) :uchar pos) value)))

(defmacro with-cbuffer (var &body body)
    `(let ((,var (alloc-cbuffer)))
      (unwind-protect
	   (progn ,@body)
	(free-cbuffer ,var))))
