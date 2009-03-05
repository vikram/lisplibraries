;;;; sha256.lisp -- implementation of SHA-2/256 from NIST

(in-package :crypto)

(define-digest-registers (sha256 :endian :big)
  (a #x6a09e667)
  (b #xbb67ae85)
  (c #x3c6ef372)
  (d #xa54ff53a)
  (e #x510e527f)
  (f #x9b05688c)
  (g #x1f83d9ab)
  (h #x5be0cd19))

(defconst +pristine-sha256-registers+ (initial-sha256-regs))

(defconst +sha256-round-constants+
#32@(#x428A2F98 #x71374491 #xB5C0FBCF #xE9B5DBA5 #x3956C25B #x59F111F1
 #x923F82A4 #xAB1C5ED5 #xD807AA98 #x12835B01 #x243185BE #x550C7DC3
 #x72BE5D74 #x80DEB1FE #x9BDC06A7 #xC19BF174 #xE49B69C1 #xEFBE4786
 #x0FC19DC6 #x240CA1CC #x2DE92C6F #x4A7484AA #x5CB0A9DC #x76F988DA
 #x983E5152 #xA831C66D #xB00327C8 #xBF597FC7 #xC6E00BF3 #xD5A79147
 #x06CA6351 #x14292967 #x27B70A85 #x2E1B2138 #x4D2C6DFC #x53380D13
 #x650A7354 #x766A0ABB #x81C2C92E #x92722C85 #xA2BFE8A1 #xA81A664B
 #xC24B8B70 #xC76C51A3 #xD192E819 #xD6990624 #xF40E3585 #x106AA070
 #x19A4C116 #x1E376C08 #x2748774C #x34B0BCB5 #x391C0CB3 #x4ED8AA4A
 #x5B9CCA4F #x682E6FF3 #x748F82EE #x78A5636F #x84C87814 #x8CC70208
 #x90BEFFFA #xA4506CEB #xBEF9A3F7 #xC67178F2))

(defun update-sha256-block (regs block)
  (declare (type sha256-regs regs))
  (declare (type (simple-array (unsigned-byte 32) (64)) block)
           #.(burn-baby-burn))
  (let ((a (sha256-regs-a regs)) (b (sha256-regs-b regs))
	(c (sha256-regs-c regs)) (d (sha256-regs-d regs))
        (e (sha256-regs-e regs)) (f (sha256-regs-f regs))
        (g (sha256-regs-g regs)) (h (sha256-regs-h regs)))
    (flet ((ch (x y z)
             #+cmu
             (kernel:32bit-logical-xor z
                                       (kernel:32bit-logical-and x
                                                                 (kernel:32bit-logical-xor y z)))
             #-cmu
             (logxor z (logand x (logxor y z))))
           (maj (x y z)
             (ldb (byte 32 0) (logxor (logand x y) (logand x z)
                                      (logand y z))))
           (sigma0 (x)
             (logxor (rol32 x 30) (rol32 x 19) (rol32 x 10)))
           (sigma1 (x)
             (logxor (rol32 x 26) (rol32 x 21) (rol32 x 7))))
      (declare (inline ch maj sigma0 sigma1))
      (macrolet ((sha256-round (i a b c d e f g h)
                   `(let ((x (mod32+ (sigma1 ,e)
                                        (mod32+ (ch ,e ,f ,g)
                                                (mod32+ ,h
                                                        (mod32+ (aref block ,i)
                                                                (aref +sha256-round-constants+ ,i)))))))
                     (declare (type (unsigned-byte 32) x))
                     (setf ,d (mod32+ ,d x)
                      ,h (mod32+ (sigma0 ,a)
                          (mod32+ (maj ,a ,b ,c) x))))))
        ;; Yay for "implementation-dependent" behavior (6.1.1.4).
        #.(let ((xvars (make-circular-list 'a 'b 'c 'd 'e 'f 'g 'h)))
            (loop for i from 0 below 64
                  for vars on xvars by #'(lambda (x) (nthcdr 7 x))
                  collect `(sha256-round ,i ,@(circular-list-subseq vars 0 8)) into forms
                  finally (return `(progn ,@forms))))
        #.(loop for slot in '(a b c d e f g h)
                collect (let ((regs-accessor (intern (format nil "SHA256-REGS-~A" slot))))
                          `(setf (,regs-accessor regs)
                            (mod32+ (,regs-accessor regs) ,slot))) into forms
                finally (return `(progn ,@forms)))
        regs))))

(defun sha256-expand-block (block)
  (declare (type (simple-array (unsigned-byte 32) (64)) block)
           #.(burn-baby-burn))
  (flet ((sigma0 (x)
           (declare (type (unsigned-byte 32) x))
           (logxor (rol32 x 25) (rol32 x 14) (mod32ash x -3)))
         (sigma1 (x)
           (declare (type (unsigned-byte 32) x))
           (logxor (rol32 x 15) (rol32 x 13) (mod32ash x -10))))
    (declare (inline sigma0 sigma1))
    (loop for i from 16 below 64 do
          (setf (aref block i)
                (mod32+ (sigma1 (aref block (- i 2)))
                        (mod32+ (aref block (- i 7))
                                (mod32+ (sigma0 (aref block (- i 15)))
                                        (aref block (- i 16)))))))
    (values)))


;;; mid-level

(defclass sha256 ()
  ((regs :reader registers :type sha256-regs :initform (initial-sha256-regs))
   (amount :accessor amount-processed :type (unsigned-byte 64) :initform 0)
   (block :reader sha256-block :type (simple-array (unsigned-byte 32) (64))
          :initform (make-array 64 :element-type '(unsigned-byte 32)))
   (buffer :reader buffer :type (simple-array (unsigned-byte 8) (64))
           :initform (make-array 64 :element-type '(unsigned-byte 8)))
   (buffer-index :accessor buffer-index :type (integer 0 63) :initform 0)))

(defmethod reinitialize-instance ((state sha256) &rest initargs)
  (declare (ignore initargs))
  (replace (the sha256-regs (registers state)) +pristine-sha256-registers+)
  (setf (amount-processed state) 0
        (buffer-index state) 0)
  state)

(defmethod copy-digest ((state sha256))
  (let ((copy (make-instance 'sha256)))
    (replace (registers copy) (registers state))
    (replace (buffer copy) (buffer state))
    (setf (amount-processed copy) (amount-processed state)
          (buffer-index copy) (buffer-index state))
    copy))

(define-digest-updater sha256
  (let ((regs (registers state))
        (block (sha256-block state))
        (buffer (buffer state))
        (buffer-index (buffer-index state))
        (length (- end start)))
    (declare (type sha256-regs regs) (type fixnum length)
	     (type (integer 0 63) buffer-index)
	     (type (simple-array (unsigned-byte 32) (64)) block)
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
            (sha256-expand-block block)
            (update-sha256-block regs block))
          (when (>= start end)
            (setf (buffer-index state) new-index)
            (incf (amount-processed state) length)
            (return-from update-digest state)))))
    (loop for offset of-type index from start below end by 64
          until (< (- end offset) 64)
          do
          (fill-block-ub8-be block sequence offset)
          (sha256-expand-block block)
          (update-sha256-block regs block)
          finally
          (let ((amount (- end offset)))
            (unless (zerop amount)
              (copy-to-buffer sequence offset amount buffer 0))
            (setf (buffer-index state) amount)))
    (incf (amount-processed state) length)
    state))

(define-digest-finalizer sha256 32
  (let ((old-regs (registers state))
        (block (sha256-block state))
        (old-buffer (buffer state))
        (buffer-index (buffer-index state))
        (total-length (* 8 (amount-processed state)))
        (regs (make-array 8 :element-type '(unsigned-byte 32)))
        (buffer (make-array 64 :element-type '(unsigned-byte 8))))
    (declare (type sha256-regs old-regs regs)
             (type (integer 0 63) buffer-index)
             (type (simple-array (unsigned-byte 32) (64)) block)
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
      (sha256-expand-block block)
      (update-sha256-block regs block)
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
    (sha256-expand-block block)
    (update-sha256-block regs block)
    (finalize-registers state regs)))

(defdigest sha256 :digest-length 32)
