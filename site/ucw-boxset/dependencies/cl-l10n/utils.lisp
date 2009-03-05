;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;; See the file LICENCE for licence information.
(in-package #:cl-l10n)

(deflogger l10n-logger ()
  :level +info+
  :appender (make-instance 'brief-stream-log-appender :stream t))

(defun l10n-logger.level ()
  (log.level (get-logger 'l10n-logger)))

(defun (setf l10n-logger.level) (level)
  (setf (log.level (get-logger 'l10n-logger.level)) level))

;;  Macros
;;;;;;;;;;;

;; dont worry it's nothing like if*
(defmacro or* (&rest vals)
  "(or* (string= foo a b) (char= foo b)) == 
  (or (string= foo a) (string= foo b) (char= foo b))"
  `(or ,@(mappend #'(lambda (x)
                      (destructuring-bind (test val &rest args) x
                        (if (singlep args)
                            `((,test ,val ,@args))
                            (mapcar #'(lambda (y) 
                                        `(,test ,val ,y))
                                    args))))
                  vals)))


;; Functions
;;;;;;;;;;;;;;

(defun read-key->value-text-file-into-hashtable (file)
  (with-input-from-file (stream file :external-format :utf-8)
    (iter (with result = (make-hash-table :test #'equal))
          (for line in-stream stream :using (lambda (stream eof-error-p eof-value)
                                              (let ((result (read-line stream eof-error-p eof-value)))
                                                (if (eq result eof-value)
                                                    eof-value
                                                    (trim result)))))
          (for line-number from 0)
          (when (or (zerop (length line))
                    (starts-with line ";"))
            (next-iteration))
          (for pieces = (split (load-time-value
                                (create-scanner
                                 (strcat "[ |" #\Tab "]+")))
                               line))
          (for split-count = (length pieces))
          (when (> split-count 2)
            (warn "Syntax error at line ~A, too many pieces after split: ~A" line-number pieces))
          (for singular = (elt pieces 0))
          (for plural = (if (= split-count 1)
                            singular
                            (elt pieces 1)))
          (setf (gethash singular result) plural)
          (finally (return result)))))

(defun capitalize-first-letter (str)
  (if (and (> (length str) 0)
           (not (upper-case-p (elt str 0))))
      (capitalize-first-letter! (copy-seq str))
      str))

(defun capitalize-first-letter! (str)
  (setf (aref str 0) (char-upcase (aref str 0)))
  str)

(defun mappend (fn &rest lists)
  (apply #'append (apply #'mapcar fn lists)))

(defun required-arg (name)
  (error "~A is a required argument" name))

(defvar *whitespace* (list #\Space #\Tab))

(defun strcat-separated-by (separator &rest args)
  (iter (for el in args)
        (unless el
          (next-iteration))
        (unless (first-time-p)
          (collect separator into components))
        (collect el into components)
        (finally (return (apply #'strcat components)))))

(defun trim (string &optional (bag *whitespace*))
  (string-trim bag string))

(defun group (list n)
  (assert (> n 0))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons (subseq source 0 n) acc))
                   (nreverse (cons source acc))))))
    (if list (rec list nil) nil)))

(defun winner (test get seq)
  (if (null seq)
      nil
      (let* ((val (elt seq 0))
             (res (funcall get val)))
        (dolist (x (subseq seq 1) (values val res))
          (let ((call (funcall get x)))
            (when (funcall test call res)
              (setf res call
                    val x)))))))

(defun float-part (float)
  (if (zerop float)
      ""
      (multiple-value-call 'extract-float-part (flonum-to-digits float))))

(defun extract-float-part (dp-pos aft)
  (let ((length (length aft)))
    (if (> dp-pos length)
        ""
        (with-output-to-string (x)
          (cond ((minusp dp-pos)
                 (dotimes (z (abs dp-pos))
                   (princ 0 x))
                 (princ aft x))
                (t (princ (subseq aft dp-pos)
                          x)))))))

;; From sbcl sources (src/code/print.lisp)
(defconstant single-float-min-e
  (nth-value 1 (decode-float least-positive-single-float)))
(defconstant double-float-min-e
  (nth-value 1 (decode-float least-positive-double-float)))

(defun flonum-to-digits (v)
  (let ((print-base 10)                 ; B
        (float-radix 2)                 ; b
        (float-digits (float-digits v)) ; p
        (digit-characters "0123456789")
        (min-e
         (etypecase v
           (single-float single-float-min-e)
           (double-float double-float-min-e))))
    (multiple-value-bind (f e)
        (integer-decode-float v)
      (let ((high-ok (evenp f))
            (low-ok (evenp f))
            (result (make-array 50 :element-type 'base-char
                                :fill-pointer 0 :adjustable t)))
        (labels ((scale (r s m+ m-)
                   (do ((k 0 (1+ k))
                        (s s (* s print-base)))
                       ((not (or (> (+ r m+) s)
                                 (and high-ok (= (+ r m+) s))))
                        (do ((k k (1- k))
                             (r r (* r print-base))
                             (m+ m+ (* m+ print-base))
                             (m- m- (* m- print-base)))
                            ((not (or (< (* (+ r m+) print-base) s)
                                      (and (not high-ok) (= (* (+ r m+) print-base) s))))
                             (values k (generate r s m+ m-)))))))
                 (generate (r s m+ m-)
                   (let (d tc1 tc2)
                     (tagbody
                      loop
                       (setf (values d r) (truncate (* r print-base) s))
                       (setf m+ (* m+ print-base))
                       (setf m- (* m- print-base))
                       (setf tc1 (or (< r m-) (and low-ok (= r m-))))
                       (setf tc2 (or (> (+ r m+) s)
                                     (and high-ok (= (+ r m+) s))))
                       (when (or tc1 tc2)
                         (go end))
                       (vector-push-extend (char digit-characters d) result)
                       (go loop)
                      end
                       (let ((d (cond
                                  ((and (not tc1) tc2) (1+ d))
                                  ((and tc1 (not tc2)) d)
                                  (t    ; (and tc1 tc2)
                                   (if (< (* r 2) s) d (1+ d))))))
                         (vector-push-extend (char digit-characters d) result)
                         (return-from generate result))))))
          (if (>= e 0)
              (if (/= f (expt float-radix (1- float-digits)))
                  (let ((be (expt float-radix e)))
                    (scale (* f be 2) 2 be be))
                  (let* ((be (expt float-radix e))
                         (be1 (* be float-radix)))
                    (scale (* f be1 2) (* float-radix 2) be1 be)))
              (if (or (= e min-e) (/= f (expt float-radix (1- float-digits))))
                  (scale (* f 2) (* (expt float-radix (- e)) 2) 1 1)
                  (scale (* f float-radix 2)
                         (* (expt float-radix (- 1 e)) 2) float-radix 1))))))))

#+(or) 
(defun flonum-to-digits (v &optional position relativep)
  (let ((print-base 10) ; B
        (float-radix 2) ; b
        (float-digits (float-digits v)) ; p
        (digit-characters "0123456789")
        (min-e
         (etypecase v
           (single-float single-float-min-e)
           (double-float double-float-min-e))))
    (multiple-value-bind (f e)
        (integer-decode-float v)
      (let (;; FIXME: these even tests assume normal IEEE rounding
            ;; mode.  I wonder if we should cater for non-normal?
            (high-ok (evenp f))
            (low-ok (evenp f))
            (result (make-array 50 :element-type 'base-char
                                :fill-pointer 0 :adjustable t)))
        (labels ((scale (r s m+ m-)
                   (do ((k 0 (1+ k))
                        (s s (* s print-base)))
                       ((not (or (> (+ r m+) s)
                                 (and high-ok (= (+ r m+) s))))
                        (do ((k k (1- k))
                             (r r (* r print-base))
                             (m+ m+ (* m+ print-base))
                             (m- m- (* m- print-base)))
                            ((not (or (< (* (+ r m+) print-base) s)
                                      (and (not high-ok)
                                           (= (* (+ r m+) print-base) s))))
                             (values k (generate r s m+ m-)))))))
                 (generate (r s m+ m-)
                   (let (d tc1 tc2)
                     (tagbody
                      loop
                        (setf (values d r) (truncate (* r print-base) s))
                        (setf m+ (* m+ print-base))
                        (setf m- (* m- print-base))
                        (setf tc1 (or (< r m-) (and low-ok (= r m-))))
                        (setf tc2 (or (> (+ r m+) s)
                                      (and high-ok (= (+ r m+) s))))
                        (when (or tc1 tc2)
                          (go end))
                        (vector-push-extend (char digit-characters d) result)
                        (go loop)
                      end
                        (let ((d (cond
                                   ((and (not tc1) tc2) (1+ d))
                                   ((and tc1 (not tc2)) d)
                                   (t ; (and tc1 tc2)
                                    (if (< (* r 2) s) d (1+ d))))))
                          (vector-push-extend (char digit-characters d) result)
                          (return-from generate result)))))
                 (initialize ()
                   (let (r s m+ m-)
                     (if (>= e 0)
                         (let* ((be (expt float-radix e))
                                (be1 (* be float-radix)))
                           (if (/= f (expt float-radix (1- float-digits)))
                               (setf r (* f be 2)
                                     s 2
                                     m+ be
                                     m- be)
                               (setf r (* f be1 2)
                                     s (* float-radix 2)
                                     m+ be1
                                     m- be)))
                         (if (or (= e min-e)
                                 (/= f (expt float-radix (1- float-digits))))
                             (setf r (* f 2)
                                   s (* (expt float-radix (- e)) 2)
                                   m+ 1
                                   m- 1)
                             (setf r (* f float-radix 2)
                                   s (* (expt float-radix (- 1 e)) 2)
                                   m+ float-radix
                                   m- 1)))
                     (when position
                       (when relativep
                         (assert (> position 0))
                         (do ((k 0 (1+ k))
                              ;; running out of letters here
                              (l 1 (* l print-base)))
                             ((>= (* s l) (+ r m+))
                              ;; k is now \hat{k}
                              (if (< (+ r (* s (/ (expt print-base (- k position)) 2)))
                                     (* s (expt print-base k)))
                                  (setf position (- k position))
                                  (setf position (- k position 1))))))
                       (let ((low (max m- (/ (* s (expt print-base position)) 2)))
                             (high (max m+ (/ (* s (expt print-base position)) 2))))
                         (when (<= m- low)
                           (setf m- low)
                           (setf low-ok t))
                         (when (<= m+ high)
                           (setf m+ high)
                           (setf high-ok t))))
                     (values r s m+ m-))))
          (multiple-value-bind (r s m+ m-) (initialize)
            (scale r s m+ m-)))))))
;; EOF
