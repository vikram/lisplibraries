(in-package :araneida)

;;;; This assumes an ASCII-based character set.  It's not the fastest
;;;; base64 decoder in the world either, I expect - but presently it's
;;;; only used for http basic authentication header decoding 

(defun base64-decode-quad (q)
  (declare (optimize (speed 3)))
  (labels ((lookup (c)
                   (or
                    (position c
                              "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/") 0)))
    (destructuring-bind (q1 q2 q3 q4 &rest ignored) q
      (declare (ignore ignored))
      (let ((val (coerce 0 '(unsigned-byte 32))))
        (setf (ldb (byte 6 0) val) (lookup q4))
        (setf (ldb (byte 6 6) val) (lookup q3))
        (setf (ldb (byte 6 12) val) (lookup q2))
        (setf (ldb (byte 6 18) val) (lookup q1))
        (let ((v1 (ldb (byte 8 16) val))
              (v2 (ldb (byte 8 8) val))
              (v3 (ldb (byte 8 0) val)))
          (list (code-char v1) (code-char v2) (code-char v3)))))))

(defun base64-decode (string)
  "Decode the MIME Base64-encoded data in STRING, as per RFC1521 section 5.2"
  ;; (1) Delete all the characters not in the set [A-Za-z0-9+=]
  ;; (2) assert string length is a multiple of 4
  ;; (3) strip all #\= from end of string
  ;; (4) assign each byte a number 0-63 based on its position in the
  ;;    set A-Za-z0-9+/ 
  ;; (5) iterate over the string in 4-character chunks.  For each
  ;;    chunk, output 3 characters
  (let ((len (truncate (* (or (position #\= string) (length string)) .75))))
    (subseq 
     (coerce 
      (loop for q on (coerce string 'list) by #'cddddr
            for out-chunk = (base64-decode-quad q)
            append out-chunk)
      'string)
     0 len)))