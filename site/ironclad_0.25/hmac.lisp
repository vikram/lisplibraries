;;;; hmac.lisp -- RFC 2104 keyed hashing for message authentication

(in-package :crypto)

;;; FIXME: this actually should be dependent upon the blocksize of the
;;; chosen hash function.  This only works because all the hash functions
;;; we support have a 64-byte block size.

(defconstant +hmac-blocksize+ 64)

(defclass hmac ()
  ((inner-digest :reader inner-digest :initarg :inner-digest)
   (outer-digest :reader outer-digest :initarg :outer-digest)))

(defun make-hmac (key digest-name)
  (declare (type (simple-array (unsigned-byte 8) (*)) key))
  (let ((inner (make-digest digest-name))
        (outer (make-digest digest-name))
        (inner-padding (make-array +hmac-blocksize+
                                   :element-type '(unsigned-byte 8)
                                   :initial-element #x36))
        (outer-padding (make-array +hmac-blocksize+
                                   :element-type '(unsigned-byte 8)
                                   :initial-element #x5c))
        (padded-key (make-array +hmac-blocksize+
                                :element-type '(unsigned-byte 8)
                                :initial-element 0)))
    (declare (type (simple-array (unsigned-byte 8) (64))
                   inner-padding outer-padding padded-key))
    (when (> (length key) +hmac-blocksize+)
      (setf key (digest-sequence digest-name key)))
    (replace padded-key key)
    (xor-block +hmac-blocksize+ padded-key inner-padding 0 inner-padding 0)
    (update-digest inner inner-padding)
    (xor-block +hmac-blocksize+ padded-key outer-padding 0 outer-padding 0)
    (update-digest outer outer-padding)
    (make-instance 'hmac :inner-digest inner :outer-digest outer)))

(defun update-hmac (hmac sequence &key (start 0) (end (length sequence)))
  (declare (type (simple-array (unsigned-byte 8) (*)) sequence))
  (update-digest (inner-digest hmac) sequence :start start :end end)
  hmac)

(defun hmac-digest (hmac)
  (let ((x (copy-digest (outer-digest hmac))))
    (update-digest x (produce-digest (inner-digest hmac)))
    (produce-digest x)))
