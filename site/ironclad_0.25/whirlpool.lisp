;;;; This file implements the Whirlpool message-digest algoritm, as
;;;; defined in The WHIRLPOOL Hashing Function, by Paulo S.L.M. Barreto1
;;;; and Vincent Rijmen, revised on Revised on May 24, 2003 (1).
;;;;
;;;; It was written by Peter Gijsels.
;;;; Copyright (c) 2007, Peter Gijsels
;;;; All rights reserved.
;;;;
;;;; This software is "as is", and has no warranty of any kind.  The
;;;; authors assume no responsibility for the consequences of any use of
;;;; this software.

(in-package :crypto)

(eval-when (:compile-toplevel :load-toplevel :execute)
(deftype whirlpool-regs () '(simple-array (unsigned-byte 32) (64)))
(defun initial-whirlpool-regs ()
  (make-array 64 :element-type '(unsigned-byte 32) :initial-element 0))
(defconstant +whirlpool-regs-hash-offset+ 0)
(defconstant +whirlpool-regs-k-offset+ 16)
(defconstant +whirlpool-regs-state-offset+ 32)
(defconstant +whirlpool-regs-l-offset+ 48)
) ; EVAL-WHEN

(defconst +pristine-whirlpool-registers+ (initial-whirlpool-regs))

(defun whirlpoolregs-digest (regs buffer &optional (start 0))
  (declare (type whirlpool-regs regs)
           (type (integer 0 #.(- array-dimension-limit 64)) start))
  (flet ((stuff-registers (buffer start)
             (dotimes (i 16 buffer)
               (setf (ub32ref/be buffer (+ start (* 4 i))) (aref regs i)))))
    (declare (inline stuff-registers))
    (cond
      (buffer (stuff-registers buffer start))
      (t (stuff-registers (make-array 64 :element-type '(unsigned-byte 8)) 0)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defconstant +whirlpool-rounds+ 10 "The number of rounds. The default is 10.")
)

(eval-when (:compile-toplevel)
  ;;; Code to generate lookup tables +C-EVEN+ and +C-ODD+.

  (defconst +E+ #(#x1 #xB #x9 #xC #xD #x6 #xF #x3 #xE #x8 #x7 #x4 #xA #x2 #x5 #x0))
  (defconst +R+ #(#x7 #xC #xB #xD #xE #x4 #x9 #xF #x6 #x3 #x8 #xA #x2 #x5 #x1 #x0))

  (defun E (i) (aref +E+ i))

  (defun R (i) (aref +R+ i))

  (defun E-1 (i) (position i +E+))

  (defun byte-xor (i1 i2) (logxor i1 i2))

  (defun S-internal (u v)
    "The S-box internals. Corresponds to equations on page 10 of (1)."
    (let ((r (R (byte-xor (E u) (E-1 v)))))
      (values (E (byte-xor (E u) r))
              (E-1 (byte-xor (E-1 v) r)))))

  (defun S (i)
    "The S-box function."
    (let ((u (ldb (byte 4 4) i))
          (v (ldb (byte 4 0) i)))
      (multiple-value-bind (u_ v_) (S-internal u v)
        (let ((result 0))
          (setf (ldb (byte 4 4) result) u_
                (ldb (byte 4 0) result) v_)
          result))))

  (defconstant +P8+ #.(reduce #'+ (mapcar #'(lambda (x) (expt 2 x)) '(8 4 3 2 0)))
               "The primitive polynomial of degree 8 for GF(2^8).")

  ;; Arithmetic in the Galois Field GF(2^8).
  (defun GF-add (x y)
    (logxor x y))

  (defun GF-shift (x n)
    (ash x n))
   
  (defun GF-reduce (x)
    (let ((result x))
      (loop until (< (integer-length result) (integer-length +P8+))
        do (setf result (GF-add result (GF-shift +P8+ (- (integer-length result) (integer-length +P8+))))))
      result))

  (defun GF-mult (x y)
    (loop with result = 0
       for i downfrom (integer-length y) to 0
       do (progn
            (setf result (GF-reduce (GF-shift result 1)))
            (unless (zerop (ldb (byte 1 i) y))
              (setf result (GF-add result x))))
       finally (return result)))

  (defun cir (vector)
    "The circulant matrix whose first row is VECTOR."
    (loop with n = (length vector)
       with result = (make-array (list n n))
       for i below n
       do (loop for j below n
             do (setf (aref result i j) (aref vector (mod (- j i) n))))
       finally (return result)))
  
  (defparameter *C* (cir #(1 1 4 1 8 5 2 9)))

  (defun calculate-table-word (i j offset)
    (loop with Sx = (S j)
       with result = 0
       for k below 4
       do (setf (ldb (byte 8 (- 32 (* (1+ k) 8))) result) 
                (GF-mult Sx (aref *C* i (+ k offset))))
       finally (return result)))

  (defun calculate-c-even ()
    (loop with result = (make-array '(8 256) :element-type '(unsigned-byte 32)
                                    :initial-element 0)
       for i below 8
       do (dotimes (j 256)
            (setf (aref result i j) (calculate-table-word i j 0)))
       finally (return result)))

  (defun calculate-c-odd ()
    (loop with result = (make-array '(8 256) :element-type '(unsigned-byte 32)
                                     :initial-element 0)
       for i below 8
       do (dotimes (j 256)
            (setf (aref result i j) (calculate-table-word i j 4)))
       finally (return result)))
) ; EVAL-WHEN

(declaim (type (simple-array (unsigned-byte 32) (22)) +rc+))
(defconst +rc+
  #.(loop with result = (make-array 22 :element-type '(unsigned-byte 32)
                                    :initial-element 0)
       with one-row-of-bytes = (make-array 8 :element-type '(unsigned-byte 8))
       for r from 1 to +whirlpool-rounds+
       do (progn
            (loop for j below 8 do
                 (setf (aref one-row-of-bytes j) (s (+ (* 8 (- r 1)) j))))
            (setf (aref result (* 2 r)) (ub32ref/be one-row-of-bytes 0))
            (setf (aref result (+ (* 2 r) 1)) (ub32ref/be one-row-of-bytes 4)))
       finally (return result)))

(declaim (type (simple-array (unsigned-byte 32) (8 256)) +C-EVEN+ +C-ODD+))
(defconst +C-EVEN+ #.(calculate-c-even))
(defconst +C-ODD+ #.(calculate-c-odd))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;;; Macro helper functions.
  (defun extract-byte (k row column)
    (if (>= column 4)
        `(ldb (byte 8 ,(- 24 (* 8 (- column 4)))) (,k ,(1+ (* 2 row))))
        `(ldb (byte 8 ,(- 24 (* 8 column))) (,k ,(* 2 row)))))
  
  (defun split (lst)
    (let* ((n (length lst))
           (mid (floor n 2)))
      (values
       (subseq lst 0 mid)
       (subseq lst mid))))
  
  (defun generate-xor (terms)
    (if (endp (cdr terms))
        (car terms)
        (multiple-value-bind (terms1 terms2) (split terms)
          `(logxor ,(generate-xor terms1) ,(generate-xor terms2)))))
  
  (defun one-slice (to from i)
    (let ((indices (loop for n below 8 collect (gensym))))
      `(let (,@(loop for index in indices
                     for j below 8
                     collect `(,index ,(extract-byte from (mod (- i j) 8) j))))
        (setf (,to ,(* 2 i))
         ,(generate-xor `,(loop for index in indices
                                for j below 8
                                collect `(aref +c-even+ ,j ,index))))
        (setf (,to ,(1+ (* 2 i)))
         ,(generate-xor `,(loop for index in indices
                                for j below 8
                                collect `(aref +c-odd+ ,j ,index)))))))
) ; EVAL-WHEN

(defmacro lookup-in-c (to from)
  `(progn
    ,@(loop for i below 8 collect (one-slice to from i))))

(defun update-whirlpool-block (regs block)
  "This is the core part of the Whirlpool algorithm. It takes a complete 16
word block of input, and updates the working state in the regs."
  (declare (type whirlpool-regs regs)
	   (type (simple-array (unsigned-byte 32) (16)) block))
  (macrolet ((hash (i)
               `(aref regs (+ ,i +whirlpool-regs-hash-offset+)))
             (k (i)
               `(aref regs (+ ,i +whirlpool-regs-k-offset+)))
             (state (i)
               `(aref regs (+ ,i +whirlpool-regs-state-offset+)))
             (l (i)
               `(aref regs (+ ,i +whirlpool-regs-l-offset+))))
    ;; Compute and apply K^0 to the cipher state
    (loop for i below 16
       do (setf (state i) (logxor (aref block i) (setf (k i) (hash i)))))
    ;; Iterate over all rounds
    (loop for r of-type (integer 1 11) from 1 to +whirlpool-rounds+
       do (progn
            ;; Compute K^r from K^{r-1}
            (lookup-in-c l k)
            (setf (l 0) (logxor (l 0) (aref +rc+ (* 2 r))))
            (setf (l 1) (logxor (l 1) (aref +rc+ (+ (* 2 r) 1))))
            (loop for i below 16
               do (setf (k i) (l i)))
            ;; Apply the r-th round transformation
            (lookup-in-c l state)
            (loop for i below 16
               do (setf (l i) (logxor (l i) (k i))))
            (loop for i below 16
               do (setf (state i) (l i)))))
    ;; Apply the Miyaguchi-Preneel compression function
    (loop for i below 16
       do (setf (hash i)
                (logxor (hash i)
                        (logxor (state i)
                                (aref block i)))))
    regs))

;;; Mid-Level Drivers

(defclass whirlpool ()
  ((regs :reader registers :type whirlpool-regs
         :initform (initial-whirlpool-regs))
   (amount :accessor amount-processed :type (unsigned-byte 64) :initform 0)
   (block :reader whirlpool-block :type (simple-array (unsigned-byte 32) (16))
          :initform (make-array 16 :element-type '(unsigned-byte 32)))
   (buffer :reader buffer :type (simple-array (unsigned-byte 8) (64))
           :initform (make-array 64 :element-type '(unsigned-byte 8)))
   (buffer-index :accessor buffer-index :type (integer 0 63) :initform 0)))

(defmethod reinitialize-instance ((state whirlpool) &rest initargs)
  (declare (ignore initargs))
  (let ((regs (registers state)))
    (replace regs +pristine-whirlpool-registers+)
    (setf (amount-processed state) 0
          (buffer-index state) 0)
    state))

(defmethod copy-digest ((state whirlpool))
  (let ((copy (make-instance 'whirlpool)))
    (let ((orig-regs (registers state))
          (copy-regs (registers copy)))
      (replace copy-regs orig-regs))
    (replace (buffer copy) (buffer state))
    (setf (amount-processed copy) (amount-processed state)
          (buffer-index copy) (buffer-index state))
    copy))

(define-digest-updater whirlpool
  "Update the given whirlpool state from sequence, which is either a
simple-string or a simple-array with element-type (unsigned-byte 8),
bounded by start and end, which must be numeric bounding-indices."
  (let* ((regs (registers state))
	 (block (whirlpool-block state))
	 (buffer (buffer state))
	 (buffer-index (buffer-index state))
	 (length (- end start)))
    (declare (type whirlpool-regs regs)
	     (type fixnum length)
	     (type (integer 0 63) buffer-index)
	     (type (simple-array (unsigned-byte 32) (16)) block)
	     (type (simple-array (unsigned-byte 8) (64)) buffer))
    ;; Handle old rest
    (unless (zerop buffer-index)
      (let ((amount (min (- 64 buffer-index) length)))
	(declare (type (integer 0 63) amount))
	(copy-to-buffer sequence start amount buffer buffer-index)
	(setq start (the fixnum (+ start amount)))
        (let ((new-index (mod (+ buffer-index amount) 64)))
          (when (zerop new-index)
            (fill-block-ub8-be block buffer 0)
            (update-whirlpool-block regs block))
          (when (>= start end)
            (setf (buffer-index state) new-index)
            (incf (amount-processed state) length)
            (return-from update-digest state)))))
    (loop for offset of-type index from start below end by 64
          until (< (- end offset) 64)
          do
          (fill-block-ub8-be block sequence offset)
          (update-whirlpool-block regs block)
          finally
          (let ((amount (- end offset)))
            (unless (zerop amount)
              (copy-to-buffer sequence offset amount buffer 0))
            (setf (buffer-index state) amount)))
    (incf (amount-processed state) length)
    state))

(define-digest-finalizer whirlpool 64
  "If the given whirlpool-state has not already been finalized, finalize it,
by processing any remaining input in its buffer, with suitable padding
and appended bit-length, as specified by the Whirlpool standard.

The resulting whirlpool message-digest is returned as an array of 64
 (unsigned-byte 8) values.  Calling UPDATE-WHIRLPOOL-STATE after a call to
FINALIZE-WHIRLPOOL-STATE results in unspecified behaviour."
  (let ((regs (registers state))
        (block (whirlpool-block state))
        (buffer (buffer state))
        (buffer-index (buffer-index state))
        (total-length (* 8 (amount-processed state))))
    (declare (type whirlpool-regs regs)
             (type (integer 0 63) buffer-index)
             (type (simple-array (unsigned-byte 32) (16)) block)
             (type (simple-array (unsigned-byte 8) (64)) buffer))
    ;; Add mandatory bit 1 padding
    (setf (aref buffer buffer-index) #x80)
    ;; Fill with 0 bit padding
    (loop for index of-type (integer 0 64)
       from (1+ buffer-index) below 64
       do (setf (aref buffer index) #x00))
    (fill-block-ub8-be block buffer 0)
    ;; Flush block first if length wouldn't fit
    (when (>= buffer-index 32)
      (update-whirlpool-block regs block)
      ;; Create new fully 0 padded block
      (loop for index of-type (integer 0 16) from 0 below 16
         do (setf (aref block index) #x00000000)))
    ;; Add 256 bit message bit length
    (loop for i of-type (integer 0 8) from 0 below 8
       do (setf (aref block (+ 8 i))
                (ldb (byte 32 (- 256 (* 32 (1+ i)))) total-length)))
    ;; Flush last block
    (update-whirlpool-block regs block)
    ;; Done, remember digest for later calls
    (finalize-registers state regs)))

(defdigest whirlpool :digest-length 64)
