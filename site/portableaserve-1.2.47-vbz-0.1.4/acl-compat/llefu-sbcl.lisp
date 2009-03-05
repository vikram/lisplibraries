;;;;
;;;; low-level-external-format-utils.lisp
;;;;
;;;; Author:    VOROSBARANYI Zoltan
;;;; Copyright: VOROSBARANYI Zoltan, 2006
;;;; License:   LLGPL
;;;;
;;;; Low level external format utilities for ACL-COMPAT.
;;;; It uses low level internal functions from SBCL.
;;;; 

(in-package #:llefu-sbcl)

#+nil
(declaim (optimize (safety 3) (debug 3)))

(defmacro defconst (name value &optional doc)
  `(defconstant ,name
    (if (boundp ',name)
        (symbol-value ',name)
        ,value)
    ,@(when doc (list doc))))

(defconst +external-format-table-initial-size+ 200)

(defconst +ef-pkg+ "SB-IMPL")

(defconst +mapper-prefix+ "CODE->")
(defconst +mapper-suffix+ "-MAPPER")
(defconst +rev-mapper-suffix+ "->CODE-MAPPER")

(defvar *external-format-table*
  (make-hash-table :size +external-format-table-initial-size+))

(defun extract-ef-name (symbol prefix suffix)
  "Get external format name as string as is used in various functions.
Strips prefix and suffix."
  (let* ((name (symbol-name symbol))
         (name-len (length name))
         (prefix-len (length prefix))
         (suffix-len (length suffix)))
    (if (and (> name-len (+ prefix-len suffix-len))
             (string= name prefix :end1 prefix-len)
             (string= name suffix :start1 (- name-len suffix-len)))
        (subseq name prefix-len (- name-len suffix-len))
        nil)))

(defun collect-external-formats (prefix suffix)
  "Get symbol names from +EF-PKG+ which contains prefix and suffix,
then strip them."
  (remove-if #'null
             (mapcar #'(lambda (sym) (extract-ef-name sym prefix suffix))
                     (remove-if-not #'fboundp
                                    (apropos-list prefix +ef-pkg+)))))

(defun fixed-external-formats ()
  "Collect all external format names which are used in mapper functions
CODE->[EF]-MAPPER where [EF] denotes external format name."
  (collect-external-formats +mapper-prefix+ +mapper-suffix+))

(defun add-ef (ef list)
  (setf (gethash ef *external-format-table*) list))

(defun ascii-compatible-fixed-ef-p (code->ef-mapper)
  "Check ASCII-compatibility for fixed length external format."
  (loop :for code :from 0 :to 127
        :always (= code (funcall code->ef-mapper code))))

#|
(defun ascii-compatible-variable-ef-p (bytes-per simple-get)
  "Check ASCII-compatibility for variable length external format."
  (let ((a (make-array 1 :element-type '(unsigned-byte 8))))
    (loop :for code :from 0 :to 127
          :always (progn
                    (setf (aref a 0) code)
                    (let ((len (ignore-errors (funcall bytes-per a 0 1))))
                      (when (and len (= len 1))
                        (let ((ch (funcall simple-get a 0 1)))
                          (char= (code-char code) ch))))))))
|#

(defun add-fixed-ef (ef-name)
  "Find mapper functions and all keyword symbols for the external
format and add to the table using ADD-EF"
  (let* ((ef-sym (find-symbol ef-name "KEYWORD"))
         (code->ef-mapper-sym (find-symbol (concatenate 'string
                                                        +mapper-prefix+
                                                        ef-name
                                                        +mapper-suffix+)
                                           +ef-pkg+))
         (ef->code-mapper-sym (find-symbol (concatenate 'string
                                                        ef-name
                                                        +rev-mapper-suffix+)
                                           +ef-pkg+)))
    (when (and ef-sym code->ef-mapper-sym ef->code-mapper-sym)
      (let ((code->ef-mapper (symbol-function code->ef-mapper-sym))
            (ef->code-mapper (symbol-function ef->code-mapper-sym))
            (ef-sym-list (first (sb-impl::find-external-format ef-sym))))
        (when (and code->ef-mapper ef->code-mapper ef-sym-list)
          (let* ((ac (ascii-compatible-fixed-ef-p code->ef-mapper))
                 (ef-entry (list nil ac code->ef-mapper ef->code-mapper)))
            (mapc #'(lambda (ef) (add-ef ef ef-entry)) ef-sym-list))
          t)))))

(macrolet ((set-array (rel-pos value)
             (cond ((= 0 rel-pos)
                    `(setf (aref array pos) ,value))
                   ((= 1 rel-pos)
                    `(setf (aref array (1+ pos)) ,value))
                   (t
                    `(setf (aref array (+ ,rel-pos pos)) ,value)))))

  (defun ch->utf8 (code array pos length)
    (declare (optimize (speed 3) (safety 0))
             (type fixnum code pos length)
             (type (simple-array (unsigned-byte 8)) array))
    (ecase length
      (1 (set-array 0 code))
      (2 (set-array 0 (logior #b11000000 (ldb (byte 5 6) code)))
         (set-array 1 (logior #b10000000 (ldb (byte 6 0) code))))
      (3 (set-array 0 (logior #b11100000 (ldb (byte 4 12) code)))
         (set-array 1 (logior #b10000000 (ldb (byte 6 6) code)))
         (set-array 2 (logior #b10000000 (ldb (byte 6 0) code))))
      (4 (set-array 0 (logior #b11110000 (ldb (byte 3 18) code)))
         (set-array 1 (logior #b10000000 (ldb (byte 6 12) code)))
         (set-array 2 (logior #b10000000 (ldb (byte 6 6) code)))
         (set-array 3 (logior #b10000000 (ldb (byte 6 0) code))))))

  (defun ch->ef (ef-sym ucs-to-ef code array pos length)
    (declare (optimize (speed 3) (safety 0))
             (type fixnum code pos length)
             (type (simple-array (unsigned-byte 8)) array))
    (let ((code (funcall ucs-to-ef code)))
      (declare (fixnum code))
      (if code
          (ecase length
            (1 (set-array 0 code))
            (2 (set-array 0 (ldb (byte 8 8) code))
               (set-array 1 (ldb (byte 8 0) code)))
            (3 (set-array 0 (ldb (byte 8 16) code))
               (set-array 1 (ldb (byte 8 8) code))
               (set-array 2 (ldb (byte 8 0) code)))
            (4 (set-array 0 (ldb (byte 8 24) code))
               (set-array 1 (ldb (byte 8 16) code))
               (set-array 2 (ldb (byte 8 8) code))
               (set-array 3 (ldb (byte 8 0) code))))
          (sb-impl::encoding-error ef-sym "" 0))))

  (defun ch->ucs-2le (code array pos length)
    (declare (optimize (speed 3) (safety 0))
             (type fixnum code pos)
             (ignore length)
             (type (simple-array (unsigned-byte 8)) array))
    (if (< code #x10000)
        (progn
          (set-array 0 (ldb (byte 8 0) code))
          (set-array 1 (ldb (byte 8 8) code)))
        (sb-impl::encoding-error :ucs-2le "" 0)))

  (defun ch->ucs-2be (code array pos length)
    (declare (optimize (speed 3) (safety 0))
             (type fixnum code pos)
             (ignore length)
             (type (simple-array (unsigned-byte 8)) array))
    (if (< code #x10000)
        (progn
          (set-array 0 (ldb (byte 8 8) code))
          (set-array 1 (ldb (byte 8 0) code)))
        (sb-impl::encoding-error :ucs-2be "" 0))))

(defun code-length-utf8 (start-octet)
  "This function expects start byte and returns the length of the 
correct UTF-8 sequence or 0 otherwise."
  (declare (optimize (speed 3) (safety 0))
           (type (unsigned-byte 8) start-octet))
  (cond
    ((zerop (logand start-octet #b10000000)) 1)
    ((= (logand start-octet #b11100000) #b11000000) 2)
    ((= (logand start-octet #b11110000) #b11100000) 3)
    ((= (logand start-octet #b11111000) #b11110000) 4)
    ((= (logand start-octet #b11111100) #b11111000) 5)
    ((= (logand start-octet #b11111110) #b11111100) 6)
    (t 0)))

(defparameter *variable-ef-table*
  `((:utf8 
     t
     t
     ,#'sb-impl::bytes-per-utf8-character-aref
     ,#'code-length-utf8
     ,#'sb-impl::simple-get-utf8-char-aref
     ,#'sb-impl::char-len-as-utf8
     ,#'sb-impl::char->utf8
     ,#'ch->utf8)
    (:euc-jp
     t
     t
     ,#'sb-impl::bytes-per-euc-jp-character-aref
     ,#'(lambda (e)
         (let ((r (sb-impl::mb-len-as-eucjp e)))
           (if r r 0)))
     ,#'sb-impl::simple-get-euc-jp-char-aref
     ,#'(lambda (code) (sb-impl::mb-char-len (sb-impl::ucs-to-eucjp code)))
     ,#'(lambda (char dest) (sb-impl::char->-euc-jp char dest "" 0))
     ,#'(lambda (code array pos length)
         (ch->ef :euc-jp #'sb-impl::ucs-to-eucjp code array pos length)))
    (:shift_jis
     t
     t
     ,#'sb-impl::bytes-per-shift_jis-character-aref
     ,#'(lambda (e)
         (let ((r (sb-impl::mb-len-as-sjis e)))
           (if r r 0)))
     ,#'sb-impl::simple-get-shift_jis-char-aref
     ,#'(lambda (code) (sb-impl::mb-char-len (sb-impl::ucs-to-sjis code)))
     ,#'(lambda (char dest) (sb-impl::char->-shift_jis char dest "" 0))
     ,#'(lambda (code array pos length)
         (ch->ef :shift_jis #'sb-impl::ucs-to-sjis code array pos length)))
    (:ucs-2le
     t
     nil
     ,#'sb-impl::bytes-per-ucs-2le-character-aref
     ,(constantly 2)
     ,#'sb-impl::simple-get-ucs-2le-char-aref
     ,(constantly 2)
     ,#'sb-impl::char->ucs-2le
     ,#'ch->ucs-2le)
    (:ucs-2be
     t
     nil
     ,#'sb-impl::bytes-per-ucs-2be-character-aref
     ,(constantly 2)
     ,#'sb-impl::simple-get-ucs-2be-char-aref
     ,(constantly 2)
     ,#'sb-impl::char->ucs-2be
     ,#'ch->ucs-2be)))

(defun add-variable-ef (var-ef)
  (let* ((ef-sym (car var-ef))
         (ef-entry (cdr var-ef))
         (ef-sym-list (first (sb-impl::find-external-format ef-sym))))
    (mapc #'(lambda (ef) (add-ef ef ef-entry)) ef-sym-list)))

(defun add-identity-ef (ef-sym)
  (let ((ef-sym-list (first (sb-impl::find-external-format ef-sym))))
    (when ef-sym-list
      (let ((ef-entry (list nil t #'identity #'identity)))
        (mapc #'(lambda (ef) (add-ef ef ef-entry)) ef-sym-list))
      t)))

(defun fill-ef-table ()
  "Collect all external format names and fill the external format table."
  (let ((fix-ef-names (fixed-external-formats)))
    (add-identity-ef :latin1)
    (mapc #'add-fixed-ef fix-ef-names)
    (mapc #'add-variable-ef *variable-ef-table*)
    (values)))

(fill-ef-table)

;;;
;;; External interface:
;;;

(defun get-ef (ef-sym)
  "Find external format. The result should be fed to the selector functions."
  (gethash ef-sym *external-format-table*))

(defun variable-length-ef-p (ef)
  "Selector function. Returns whether this external function uses
variable length encoding. If returns NIL then the mapper selectors
should be used."
  (first ef))

(defun ascii-compatible-ef-p (ef)
  "Selector function. Returns true if the external format is ASCII compatible,
i.e. the lower 128 codes are equivalent with ASCII-encoding."
  (second ef))

(defun code->ef-mapper (ef)
  "Selector function. The returned function maps character codes to
external format byte."
  (unless (variable-length-ef-p ef)
    (third ef)))

(defun ef->code-mapper (ef)
  "Selector function. The returned function maps external format byte to
character code."
  (unless (variable-length-ef-p ef)
    (fourth ef)))

(defun bytes-per-char-ef (ef)
  "Selector function. The returned function expects three parameters:
an array of (unsigned-byte 8), starting position, and end of array
as usual; returns the length of the code sequence. It checks for
valid code sequence."
  (when (variable-length-ef-p ef)
    (third ef)))

(defun ef-code-length (ef)
  "Selector function. The returned function expects start byte
of the code sequence, returns the length of the code if valid
start byte is given and 0 otherwise. Note that BYTES-PER-CHAR-EF should
be used for computing length value passed to SIMPLE-GET-CHAR."
  (when (variable-length-ef-p ef)
    (fourth ef)))

(defun simple-get-char-ef (ef)
  "Selector function. The returned function expects three parameters:
an array of (unsigned-byte 8), starting position, and sequence length
as returned by the function which was the result of BYTES-PER-CHAR-EF;
it returns the character. It does no check for valid code sequence."
  (when (variable-length-ef-p ef)
    (fifth ef)))

(defun char-len-as-ef (ef)
  "Selector function. The returned function expects a character code,
and returns the length of the external format's code sequence."
  (when (variable-length-ef-p ef)
    (sixth ef)))

(defun char->ef-push (ef)
  "Selector function. The returned function expects a character code,
and an array of (unsigned-byte 8) with fill pointer, then extends
the array with VECTOR-PUSH-EXTEND with the external format code sequence
values."
  (when (variable-length-ef-p ef)
    (seventh ef)))

(defun char->ef (ef)
  "Selector function. The returned function expects a character code,
an array of (unsigned-byte 8), a position and length as returned by 
the function which was the result of CHAR-LEN-AS-EF. The sequence is then
written to the array starting with position using SETF (AREF ...)"
  (when (variable-length-ef-p ef)
    (eighth ef)))

;;;
;;; Test
;;;

#+nil
(defun test-llefu ()
  (let* ((test-string "Árvíztűrő tükörfúrógép, いろはにほへど, €, זה כיף")
         (test-jp-string "いろはにほへど 草なぎ剛")
         (test-string-len (length test-string))
         (test-jp-string-len (length test-jp-string))
         (alen (* 4 (max test-string-len test-jp-string-len)))
         (a (make-array alen :element-type '(unsigned-byte 8)))
         (ef-list '(:utf8 :ucs-2le :ucs-2be))
         (jp-ef-list '(:euc-jp :shift_jis)))

    (labels
        ((to-ef (string ef)
           (let ((char-len-as-ef (char-len-as-ef ef))
                 (char->ef (char->ef ef)))
             (loop :with pos = 0
                   :with len = 0
                   :for ch :across string
                   :do (progn
                         (setq len (funcall char-len-as-ef (char-code ch)))
                         (funcall char->ef (char-code ch) a pos len)
                         (incf pos len))
                   :finally (return pos))))

         (from-ef (arr len ef)
           (let ((ef-code-length (ef-code-length ef))
                 (bytes-per-char-ef (bytes-per-char-ef ef))
                 (simple-get-char-ef (simple-get-char-ef ef)))
             (with-output-to-string (s)
               (loop :with pos = 0
                     :while (< pos len)
                     :do (progn
                           (let* ((len1 (funcall ef-code-length
                                                 (aref arr pos)))
                                  (len2 (funcall bytes-per-char-ef
                                                 arr pos len))
                                  (ch (funcall simple-get-char-ef
                                               arr pos len2)))
                             (when (/= len1 len2)
                               (error "Sequence lengths do not match!"))
                             (princ ch s)
                             (incf pos len2)))))))
           
         (print-array (arr len)
           (loop :for i :from 0 :to (1- len)
                 :do (format t "~X " (aref arr i))
                 :finally (format t "~&")))

         (test-ef-list (ef-list test-string)
           (loop :with pos = 0
                 :with s
                 :with ef
                 :for ef-sym :in ef-list
                 :do (progn
                       (setq ef (get-ef ef-sym))
                       (setq pos (to-ef test-string ef))
                       (print-array a pos)
                       (setq s (from-ef a pos ef))
                       (format t "~A~&" s)
                       (when (not (string= test-string s))
                         (error "Strings do not match!"))))
           t))

      (test-ef-list ef-list test-string)
      (test-ef-list jp-ef-list test-jp-string))))
