;;;; This is an implementation of the US Secure Hash Algorithm 1 (SHA1),
;;;; defined in RFC 3174, written by D. Eastlake and P. Jones, September
;;;; 2001.  The RFC was based on the document "Secure Hash Standard",
;;;; United States of America, National Institute of Science and Technology,
;;;; Federal Information Processing Standard (FIPS) 180-1, April 1993.
;;;;
;;;; It was written by Nathan J. Froyd, with many of the main ideas and
;;;; functions grabbed from Pierre R. Mai's CL implementation of MD5,
;;;; available at http://www.pmsf.de/pmai/MD5.html.
;;;;
;;;; This implementation should work on any conforming Common Lisp
;;;; implementation, but it has been optimized for CMU CL and SBCL.
;;;;
;;;; The implementation makes heavy use of (UNSIGNED-BYTE 32) arithmetic;
;;;; if your CL implementation does not implement unboxed arithmetic on
;;;; such numbers, performance will likely be greater in a 16-bit
;;;; implementation. 
;;;;
;;;; This software is "as is", and has no warranty of any kind.  The
;;;; authors assume no responsibility for the consequences of any use
;;;; of this software.

(in-package :crypto)

;;; nonlinear functions

(defconstant +k1+ #x5a827999)
(defconstant +k2+ #x6ed9eba1)
(defconstant +k3+ #x8f1bbcdc)
(defconstant +k4+ #xca62c1d6)

;;; working set

(define-digest-registers (sha1 :endian :big)
  (a #x67452301)
  (b #xefcdab89)
  (c #x98badcfe)
  (d #x10325476)
  (e #xc3d2e1f0))

(defconst +pristine-sha1-registers+ (initial-sha1-regs))

(macrolet ((sha1-rounds (block func constant low high &rest initial-order)
             ;; Yay for "implementation-dependent" behavior (6.1.1.4).
             (let ((xvars (apply #'make-circular-list initial-order)))
               (loop for i from low upto high
                     for vars on xvars by #'cddddr
                     collect (let ((a-var (first vars))
                                   (b-var (second vars))
                                   (c-var (third vars))
                                   (d-var (fourth vars))
                                   (e-var (fifth vars)))
                               `(setf ,e-var 
                                      (mod32+ (rol32 ,a-var 5)
                                              (mod32+ (mod32+ (,func ,b-var ,c-var ,d-var) ,e-var)
                                                      (mod32+ (aref ,block ,i) ,constant)))
                                      ,b-var (rol32 ,b-var 30))) into forms
                     finally (return `(progn ,@forms))))))
(defun update-sha1-block (regs block)
  (declare (type sha1-regs regs)
           (type (simple-array (unsigned-byte 32) (80)) block)
           #.(burn-baby-burn))
  (let ((a (sha1-regs-a regs)) (b (sha1-regs-b regs))
	(c (sha1-regs-c regs)) (d (sha1-regs-d regs))
        (e (sha1-regs-e regs)))
    (flet ((f1 (x y z)
             (declare (type (unsigned-byte 32) x y z))
             #+cmu
             (kernel:32bit-logical-xor z
                                       (kernel:32bit-logical-and x
                                                                 (kernel:32bit-logical-xor y z)))
             #-cmu
             (logxor z (logand x (logxor y z))))
           (f2 (x y z)
             (declare (type (unsigned-byte 32) x y z))
             #+cmu
             (kernel:32bit-logical-xor x (kernel:32bit-logical-xor y z))
             #-cmu
             (ldb (byte 32 0) (logxor x y z)))
           (f3 (x y z)
             (declare (type (unsigned-byte 32) x y z))
             #+cmu
             (kernel:32bit-logical-or (kernel:32bit-logical-or
                                       (kernel:32bit-logical-and x y)
                                       (kernel:32bit-logical-and x z))
                                      (kernel:32bit-logical-and y z))
             #-cmu
             (ldb (byte 32 0)
                  (logior (logand x y) (logand x z) (logand y z)))))
      (declare (inline f1 f2 f3))
      ;; core of the algorithm
      (sha1-rounds block f1 +k1+ 0 19 a b c d e)
      (sha1-rounds block f2 +k2+ 20 39 a b c d e)
      (sha1-rounds block f3 +k3+ 40 59 a b c d e)
      (sha1-rounds block f2 +k4+ 60 79 a b c d e)
      ;; update and return
      (setf (sha1-regs-a regs) (mod32+ (sha1-regs-a regs) a)
            (sha1-regs-b regs) (mod32+ (sha1-regs-b regs) b)
            (sha1-regs-c regs) (mod32+ (sha1-regs-c regs) c)
            (sha1-regs-d regs) (mod32+ (sha1-regs-d regs) d)
            (sha1-regs-e regs) (mod32+ (sha1-regs-e regs) e))
      regs)))
) ; MACROLET

(declaim (inline expand-block fill-block fill-block-ub8 fill-block-char))
(defun expand-block (block)
  "Expand the first 16 words in BLOCK to fill the entire 80 word space
available."
  (declare (type (simple-array (unsigned-byte 32) (80)) block)
           #.(burn-baby-burn))
  (loop for i of-type (integer 16 80) from 16 below 80
        do (setf (aref block i)
                 (rol32 #+cmu
                        (kernel:32bit-logical-xor
                         (kernel:32bit-logical-xor (aref block (- i 3))
                                                   (aref block (- i 8)))
                         (kernel:32bit-logical-xor (aref block (- i 14))
                                                   (aref block (- i 16))))
                        #-cmu
                        (ldb (byte 32 0)
                             (logxor (aref block (- i 3))
                                     (aref block (- i 8))
                                     (aref block (- i 14))
                                     (aref block (- i 16))))
                        1))))

;;; mid-level

(defclass sha1 ()
  ((regs :reader registers :type sha1-regs :initform (initial-sha1-regs))
   (amount :accessor amount-processed :type (unsigned-byte 64)
           :initform 0)
   (block :reader sha1-block :type (simple-array (unsigned-byte 32) (80))
          :initform (make-array 80 :element-type '(unsigned-byte 32)))
   (buffer :reader buffer :type (simple-array (unsigned-byte 8) (64))
           :initform (make-array 64 :element-type '(unsigned-byte 8)))
   (buffer-index :accessor buffer-index :type (integer 0 63)
                 :initform 0)))

(defmethod reinitialize-instance ((state sha1) &rest initargs)
  (declare (ignore initargs))
  (replace (the sha1-regs (registers state)) +pristine-sha1-registers+)
  (setf (amount-processed state) 0
        (buffer-index state) 0)
  state)

(defmethod copy-digest ((state sha1))
  (let ((copy (make-instance 'sha1)))
    (replace (registers copy) (registers state))
    (replace (buffer copy) (buffer state))
    (setf (amount-processed copy) (amount-processed state)
          (buffer-index copy) (buffer-index state))
    copy))

(define-digest-updater sha1
  (let ((regs (registers state))
        (block (sha1-block state))
        (buffer (buffer state))
        (buffer-index (buffer-index state))
        (length (- end start)))
    (declare (type sha1-regs regs) (type fixnum length)
	     (type (integer 0 63) buffer-index)
	     (type (simple-array (unsigned-byte 32) (80)) block)
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
            (expand-block block)
            (update-sha1-block regs block))
          (when (>= start end)
            (setf (buffer-index state) new-index)
            (incf (amount-processed state) length)
            (return-from update-digest state)))))
    (loop for offset of-type index from start below end by 64
          until (< (- end offset) 64)
          do
          (fill-block-ub8-be block sequence offset)
          (expand-block block)
          (update-sha1-block regs block)
          finally
          (let ((amount (- end offset)))
            (unless (zerop amount)
              (copy-to-buffer sequence offset amount buffer 0))
            (setf (buffer-index state) amount)))
    (incf (amount-processed state) length)
    state))

(define-digest-finalizer sha1 20
  (let ((old-regs (registers state))
        (block (sha1-block state))
        (old-buffer (buffer state))
        (buffer-index (buffer-index state))
        (total-length (* 8 (amount-processed state)))
        (regs (make-array 5 :element-type '(unsigned-byte 32)))
        (buffer (make-array 64 :element-type '(unsigned-byte 8))))
    (declare (type sha1-regs old-regs regs)
             (type (integer 0 63) buffer-index)
             (type (simple-array (unsigned-byte 32) (80)) block)
             (type (simple-array (unsigned-byte 8) (64)) old-buffer buffer)
             (dynamic-extent regs buffer))
    ;; Copy over.
    (replace regs old-regs)
    (replace buffer old-buffer)
    (setf (aref buffer buffer-index) #x80)
    (when (> buffer-index 55)
      (loop for index of-type (integer 0 64)
         from (1+ buffer-index) below 64
         do (setf (aref buffer index) #x00))
      (fill-block-ub8-be block buffer 0)
      (expand-block block)
      (update-sha1-block regs block)
      (loop for index of-type (integer 0 16)
         from 0 below 16
         do (setf (aref block index) #x00000000)))
    (when (<= buffer-index 55)
      (loop for index of-type (integer 0 64)
         from (1+ buffer-index) below 64
         do (setf (aref buffer index) #x00))
      ;; copy the data to BLOCK prematurely
      (fill-block-ub8-be block buffer 0))
    ;; fill in the remaining block data
    (store-data-length block total-length 14 t)
    (expand-block block)
    (update-sha1-block regs block)
    (finalize-registers state regs)))

(defdigest sha1 :digest-length 20)

