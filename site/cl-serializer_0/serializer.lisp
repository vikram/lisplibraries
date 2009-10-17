;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-serializer)

;;;;;;;;;;;
;;; Special

(def constant +nil-code+                         #x00)

(def constant +t-code+                           #x01)

(def constant +cons-code+                        #x02)

(def constant +proper-list-code+                 #x03)

(def constant +dotted-list-code+                 #x04)

;;;;;;;;;;
;;; String

(def constant +base-char-code+                   #x10)

(def constant +extended-char-code+               #x11)

(def constant +character-code+                   #x12)

(def constant +base-string-code+                 #x13)

(def constant +simple-base-string-code+          #x14)

(def constant +simple-string-code+               #x15)

(def constant +string-code+                      #x16)

;;;;;;;;;;
;;; Symbol

(def constant +symbol-code+                      #x20)

(def constant +keyword-code+                     #x21)

(def constant +uninterned-symbol-code+           #x22)

(def constant +package-code+                     #x23)

;;;;;;;;;;
;;; Number

(def constant +integer-code+                     #x30)

(def constant +rational-code+                    #x31)

(def constant +float-code+                       #x32)

(def constant +short-float-code+                 #x33)

(def constant +single-float-code+                #x34)

(def constant +double-float-code+                #x35)

(def constant +long-float-code+                  #x36)

(def constant +complex-code+                     #x37)

(def constant +number-code+                      #x38)

;;;;;;;;;;;;
;;; Compound

(def constant +simple-vector-code+               #x40)

(def constant +simple-array-code+                #x41)

(def constant +vector-code+                      #x42)

(def constant +array-code+                       #x43)

(def constant +simple-bit-vector-code+           #x44)

(def constant +bit-vector-code+                  #x45)

(def constant +hash-table-code+                  #x46)

(def constant +pathname-code+                    #x47)

(def constant +simple-unsigned-byte-8-vector-code+ #x48)

;;;;;;;;;;
;;; Object

(def constant +structure-object-code+            #x50)

(def constant +standard-object-code+             #x51)

(def constant +unbound-slot-code+                #x52)

(def constant +standard-class-code+              #x53)

(def constant +standard-direct-slot-definition-code+ #x54)

(def constant +standard-effective-slot-definition-code+ #x55)

;;;;;;;;;;;;
;;; Reserved

(def constant +first-reserved-code+              #x60)

(def constant +last-reserved-code+               #x6F)

;;;;;;;;;;;;;
;;; Reference

(def constant +reference-code+                   #x7F)

(def constant +referenced-bit-marker-index+      #x07)

(def constant +code-mask+                        #x7F)

;;;;;;;;;;;;;;;;;;
;;; Code -> lambda

(declaim (type (simple-vector 128) +serializers+ +deserializers+))

(def load-time-constant +serializers+ (make-array 128))

(def load-time-constant +deserializers+ (make-array 128))

;;;;;;;;;;;
;;; Context

(def constant +version+ 0)

(defstruct (serializer-context (:conc-name sc-))
  (buffer
   nil
   :type (simple-array (unsigned-byte 8) (*)))
  (position
   0
   :type array-index)
  (mapper
   nil
   :type function)
  (identity-map
   nil
   :type (or null hash-table))
  (list-length
   nil
   :type (or null fixnum)))

(def print-object serializer-context ()
  (princ (sc-position -self-)))

(def special-variable *deserialize-element-position*)

;;;;;;;;;
;;; Utils

(deftype simple-unsigned-byte-8-vector (&optional (size '*))
  `(simple-array (unsigned-byte 8) (,size)))

(def (function io) identity-map (context)
  (or (sc-identity-map context)
      (setf (sc-identity-map context)
            (make-hash-table :test 'eq))))

(def (function io) position-to-identity-map (context)
  (identity-map context))

(def (function io) identity-to-position-map (context)
  (identity-map context))

(def (function io) default-serializer-mapper (object context)
  "Returns (values TYPE-CODE HAS-IDENTITY WRITER-FUNCTION), where TYPE-CODE is the (unsigned-byte 8) code that identifies the object's type in the serialized output; HAS-IDENTITY is a boolean telling the engine whether to keep the object's identity through a serialize-deserialize (which is a performance overhead); and WRITER-FUNCTION is called to do the serialization after the type code has been written."
  (declare (ignore context))
  (the (values fixnum boolean function)
    (flet ((local-return (code identity)
             (values code identity (the function (aref +serializers+ code)))))
      (cond ((eq object nil)
             (local-return +nil-code+ #f))
            ((eq object t)
             (local-return +t-code+ #f))
            (t
             (etypecase object
               (cons (local-return +cons-code+ #t))
               (keyword (local-return +keyword-code+ #t))
               (symbol (if (symbol-package object)
                           (local-return +symbol-code+ #t)
                           (local-return +uninterned-symbol-code+ #t)))
               (integer (local-return +integer-code+ #f))
               (rational (local-return +rational-code+ #f))
               (float (local-return +float-code+ #f))
               (complex (local-return +complex-code+ #f))
               (base-char (local-return +base-char-code+ #f))
               (character (local-return +character-code+ #f))
               (simple-base-string (local-return +simple-base-string-code+ #t))
               (simple-string (local-return +simple-string-code+ #t))
               (string (local-return +string-code+ #t))
               (package (local-return +package-code+ #t))
               (simple-unsigned-byte-8-vector (local-return +simple-unsigned-byte-8-vector-code+ #t))
               (simple-vector (local-return +simple-vector-code+ #t))
               (simple-array (local-return +simple-array-code+ #t))
               (vector (local-return +vector-code+ #t))
               (array (local-return +array-code+ #t))
               (hash-table (local-return +hash-table-code+ #t))
               (standard-class (local-return +standard-class-code+ #t))
               (closer-mop:standard-direct-slot-definition (local-return +standard-direct-slot-definition-code+ #t))
               (closer-mop:standard-effective-slot-definition (local-return +standard-effective-slot-definition-code+ #t))
               (structure-object (local-return +structure-object-code+ #t))
               (standard-object (local-return +standard-object-code+ #t))))))))

(def (function io) default-deserializer-mapper (code context)
  (declare (ignore context))
  (the function (aref +deserializers+ code)))

(def (function io) unread-unsigned-byte-8 (context)
  (decf (sc-position context)))

(def (function o) analyze-list (list)
  "Returns two values.  The first value is one of :PROPER-LIST,
:DOTTED-LIST or :CIRCULAR-LIST.  The second value is the length of
the list.  For dotted lists, the final item is included in the
length; for circular lists, the length is NIL."
  ;; This is an adapatation of the algorithm in the Hyperspec
  ;; (see LIST-LENGTH).
  (declare (type list list))
  (loop for n fixnum = 0 :then (the (integer 0 #.(- most-positive-fixnum 2)) (+ n 2))
        for fast = list :then (cddr fast)
        for slow = list :then (cdr slow)
        ;; If fast pointer hits the end, return the count.
        do (cond ((null fast)
                  (return (values :proper-list n)))
                 ((atom fast)
                  (return (values :dotted-list (the fixnum (1+ n)))))
                 ((null (cdr fast))
                  (return (values :proper-list (the fixnum (1+ n)))))
                 ((atom (cdr fast))
                  (return (values :dotted-list (the fixnum (+ n 2)))))
                 ;; If fast pointer eventually equals slow pointer,
                 ;;  then we must be stuck in a circular list.
                 ;; (A deeper property is the converse: if we are
                 ;;  stuck in a circular list, then eventually the
                 ;;  fast pointer will equal the slow pointer.
                 ;;  That fact justifies this implementation.)
                 ((and (eq fast slow) (> n 0))
                  (return (values :circular-list 0))))))

(def macro format-log (format-specifier &rest args)
  (if cl-serializer-system:*load-with-debug-p*
      `(unless
        (ignore-errors
          (format t ,format-specifier ,@args)
          t)
        (format t "~%Error during formatting ~S" ,format-specifier))
      (values)))

;;;;;;;;;;;;;
;;; Serialize

(def (function o) serialize (object &key (output nil) (buffer-size 1024) (serializer-mapper #'default-serializer-mapper))
  (let ((context (make-serializer-context :buffer (make-array buffer-size :element-type '(unsigned-byte 8))
                                          :mapper serializer-mapper)))
    (write-variable-length-positive-integer +version+ context)
    (serialize-element object context)
    (etypecase output
      (stream (write-sequence (sc-buffer context) output :end (sc-position context)))
      (null (prog1-bind final-vector (make-array (sc-position context) :element-type '(unsigned-byte 8))
              (replace final-vector (sc-buffer context)))))))

(def (function o) serialize-element (object context)
  (declare (type serializer-context context))
  (bind (((:values type-code has-identity writer-function) (funcall (sc-mapper context) object context)))
    (check-type type-code (unsigned-byte 8))
    (check-type has-identity boolean)
    (check-type writer-function function)
    (when has-identity
      (bind ((identity-to-position-map (identity-to-position-map context))
             (position (gethash object identity-to-position-map)))
        (if position
            (bind ((buffer (sc-buffer context))
                   (code (aref buffer position)))
              (setf (logbitp +referenced-bit-marker-index+ code) 1)
              (setf (aref buffer position) code)
              (format-log "~%Serializing reference at ~A to object ~S seen at ~A" (sc-position context) object position)
              (write-unsigned-byte-8 +reference-code+ context)
              (write-variable-length-positive-integer position context)
              (return-from serialize-element (values)))
            (setf (gethash object identity-to-position-map) (sc-position context)))))
    (write-unsigned-byte-8 type-code context)
    (funcall writer-function object context)
    (values)))

;;;;;;;;;;;;;;;
;;; Deserialize

(def (function o) deserialize (input &key (deserializer-mapper #'default-deserializer-mapper))
  (let ((context
         (make-serializer-context
          :mapper deserializer-mapper
          :buffer (etypecase input
                    (array
                     (coerce input '(simple-array (unsigned-byte 8) (*))))
                    (stream
                     (read-stream-into-vector input))))))
    (unless (= +version+ (read-variable-length-positive-integer context))
      (error "Serializer version mismatch"))
    (deserialize-element context)))

(def (function o) deserialize-element (context)
  (check-type context serializer-context)
  (bind ((*deserialize-element-position* (sc-position context))
         (code-with-referenced-bit (read-unsigned-byte-8 context))
         (referenced (logbitp +referenced-bit-marker-index+ code-with-referenced-bit))
         (code (logand code-with-referenced-bit +code-mask+)))
    (if (eq code +reference-code+)
        (bind ((position (read-variable-length-positive-integer context))
               (object
                (gethash position (position-to-identity-map context) :not-found)))
          (format-log "~%Deserializing reference at ~A to object ~S at ~A" *deserialize-element-position* object position)
          (when (eq object :not-found)
            (error "Reference to ~A cannot be resolved, byte at that position is ~A"
                   position (aref (sc-buffer context) position)))
          object)
        (funcall (funcall (sc-mapper context) code context) context referenced))))

(def (function io) announce-identity (object context)
  (format-log "~%Storing referenced object ~A seen at ~A" object *deserialize-element-position*)
  (setf (gethash *deserialize-element-position* (position-to-identity-map context)) object)
  object)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Serializers and deserializers

;; TODO use -foo- naming convention for non-hygienic macro variables like -object- and -context-
(def definer serializer-deserializer (name code type serializer-form deserializer-form)
  (let ((writer-name (concatenate-symbol *package* "write-" name))
        (reader-name (concatenate-symbol *package* "read-" name)))
    `(progn
      (def (function io) ,writer-name (object context)
        (declare (ignorable object context))
        (check-type object ,type)
        (check-type context serializer-context)
        ,serializer-form
        (values))
      (def (function io) ,reader-name (context &optional referenced)
        (declare (ignorable context referenced))
        (check-type context serializer-context)
        (the ,type
          (values ,deserializer-form)))
      ,@(when code
              `((def (function io) ,(concatenate-symbol *package* "serialize-" name) (object context)
                  (check-type context serializer-context)
                  (check-type object ,type)
                  (serialize-element object context)
                  (values))
                (def (function io) ,(concatenate-symbol *package* "deserialize-" name) (context)
                  (check-type context serializer-context)
                  (the ,type
                    (values (deserialize-element context))))
                (setf (aref +serializers+ ,code) #',writer-name)
                (setf (aref +deserializers+ ,code) #',reader-name))))))

(def serializer-deserializer nil +nil-code+ null
  nil
  nil)

(def serializer-deserializer t +t-code+ t
  t
  t)

(def serializer-deserializer unsigned-byte-8 nil (unsigned-byte 8)
  (progn
    (ensure-simple-vector-size (sc-buffer context) (the fixnum (1+ (sc-position context))))
    (setf (aref (sc-buffer context) (1- (the array-index (incf (sc-position context))))) object))
  (aref (sc-buffer context) (1- (the array-index (incf (sc-position context))))))

(def serializer-deserializer unsigned-byte-32 nil (unsigned-byte 32)
  (progn
    (write-unsigned-byte-8 (ldb (byte 8 24) object) context)
    (write-unsigned-byte-8 (ldb (byte 8 16) object) context)
    (write-unsigned-byte-8 (ldb (byte 8 8) object) context)
    (write-unsigned-byte-8 (ldb (byte 8 0) object) context))
  (let ((object 0))
    (declare (type (unsigned-byte 32) object))
    (setf (ldb (byte 8 24) object) (read-unsigned-byte-8 context))
    (setf (ldb (byte 8 16) object) (read-unsigned-byte-8 context))
    (setf (ldb (byte 8 8) object) (read-unsigned-byte-8 context))
    (setf (ldb (byte 8 0) object) (read-unsigned-byte-8 context))
    object))

(def constant +integer-length-mask+ #x7F)

(def constant +integer-length-sign-bit-index+ #x07)

;; TODO: specialize on fixnums
(def serializer-deserializer integer +integer-code+ integer
  (bind ((negative (< object 0))
         (integer (if negative
                      (lognot object)
                      object))
         (length (ceiling (integer-length (abs integer)) 8))
         (length-with-sign-bit length))
    (when negative
      (setf (logbitp +integer-length-sign-bit-index+ length-with-sign-bit) 1))
    (write-unsigned-byte-8 length-with-sign-bit context)
    (loop for index :from (- (* 8 length) 8) :downto 0 :by 8
          do (write-unsigned-byte-8 (ldb (byte 8 index) integer) context)))
  (bind ((first-byte (read-unsigned-byte-8 context))
         (negative (logbitp +integer-length-sign-bit-index+ first-byte))
         (length (* 8 (logand first-byte +integer-length-mask+))))
    (let ((object 0))
      (loop for index :from (- length 8) :downto 0 :by 8
            do (setf (ldb (byte 8 index) object) (read-unsigned-byte-8 context)))
      (if negative
          (lognot object)
          object))))

(def constant +short-positive-integer+ #x7F)

;; TODO: specialize on fixnums
(def serializer-deserializer variable-length-positive-integer nil integer
  (if (<= object +short-positive-integer+)
      (write-unsigned-byte-8 object context)
      (write-integer (- object) context))
  (let ((first-byte (read-unsigned-byte-8 context)))
    (if (logbitp +integer-length-sign-bit-index+ first-byte)
        (progn
          (unread-unsigned-byte-8 context)
          (- (the fixnum (read-integer context))))
        first-byte)))

(def serializer-deserializer rational +rational-code+ rational
  (progn
    (write-integer (numerator object) context)
    (write-integer (denominator object) context))
  (/ (read-integer context) (read-integer context)))

(def serializer-deserializer float +float-code+ float
  (bind (((:values significand exponent sign)
          (integer-decode-float object)))
    (write-integer significand context)
    (write-integer exponent context)
    (write-unsigned-byte-8 (1+ sign) context))
  (bind ((significand (read-integer context))
         (exponent (read-integer context))
         (sign (1- (read-unsigned-byte-8 context))))
    (* sign (scale-float (float significand 1.0L0) exponent))))

(def serializer-deserializer complex +complex-code+ complex
  (progn
    (write-float (realpart object) context)
    (write-float (imagpart object) context))
  (complex (read-float context) (read-float context)))

(def serializer-deserializer simple-unsigned-byte-8-vector +simple-unsigned-byte-8-vector-code+ simple-unsigned-byte-8-vector
  (progn
    (write-variable-length-positive-integer (length object) context)
    (loop for octet :across object
          do (write-unsigned-byte-8 octet context)))
  (bind ((length (read-variable-length-positive-integer context))
         (object (make-array length :element-type '(unsigned-byte 8))))
    (loop for index :from 0 :below length
          do (setf (aref object index) (read-unsigned-byte-8 context)))
    object))

(def serializer-deserializer character +character-code+ character
  (write-integer (char-code object) context)
  (code-char (read-integer context)))

(def serializer-deserializer extended-char +extended-char-code+ character
  (write-integer (char-code object) context)
  (code-char (read-integer context)))

(def serializer-deserializer base-char +base-char-code+ character
  (write-unsigned-byte-8 (char-code object) context)
  (code-char (read-unsigned-byte-8 context)))

(define-symbol-macro +utf-8-mapping+ (load-time-value (babel::lookup-mapping babel::*string-vector-mappings* :utf-8)))

(def serializer-deserializer simple-base-string +simple-base-string-code+ simple-base-string
  (progn
    (write-variable-length-positive-integer (length object) context)
    (loop for character :across object
          do (write-unsigned-byte-8 (char-code character) context)))
  (bind ((length (the fixnum (read-variable-length-positive-integer context)))
         (string (make-array length :element-type 'base-char)))
    (loop for index :from 0 :below length
          do (setf (aref string index) (code-char (read-unsigned-byte-8 context))))
    (announce-identity string context)))

(def serializer-deserializer simple-string +simple-string-code+ string
  (bind ((length (length object))
         (encoded-length (funcall (the function (babel::octet-counter +utf-8-mapping+)) object 0 length -1)))
    (declare (type fixnum encoded-length))
    (write-variable-length-positive-integer encoded-length context)
    (bind ((position (sc-position context)))
      (ensure-simple-vector-size (sc-buffer context) (+ 1 position encoded-length))
      (funcall (the function (babel::encoder +utf-8-mapping+)) object 0 length (sc-buffer context) position)
      (incf (sc-position context) encoded-length)))
  (bind ((length (read-variable-length-positive-integer context))
         (start (sc-position context))
         (end (the fixnum (+ start length)))
         (buffer (sc-buffer context))
         ((:values size new-end)
          (funcall (the function (babel::code-point-counter +utf-8-mapping+)) buffer start end -1))
         (string (make-string size :element-type 'unicode-char)))
    (declare (type fixnum length start end))
    (funcall (the function (babel::decoder +utf-8-mapping+)) buffer start new-end string 0)
    (incf (sc-position context) length)
    (announce-identity string context)))

(def serializer-deserializer string +string-code+ string
  ;; TODO: this is generating garbage on the heap
  (write-simple-string (coerce object 'simple-string) context)
  (coerce (read-simple-string context) 'string))

(def serializer-deserializer generic-string nil string
  (etypecase object
    (simple-base-string
     (write-unsigned-byte-8 +simple-base-string-code+ context)
     (write-simple-base-string object context))
    (simple-string
     (write-unsigned-byte-8 +simple-string-code+ context)
     (write-simple-string object context))
    (string
     (write-unsigned-byte-8 +string-code+ context)
     (write-string object context)))
  (ecase (read-unsigned-byte-8 context)
    (#.+simple-base-string-code+
     (read-simple-base-string context))
    (#.+simple-string-code+
     (read-simple-string context))
    (#.+string-code+
     (read-string context))))

(def serializer-deserializer keyword +keyword-code+ keyword
  (write-generic-string (symbol-name object) context)
  (announce-identity (intern (read-generic-string context) :keyword) context))

(def serializer-deserializer symbol +symbol-code+ symbol
  (progn
    (write-generic-string (symbol-name object) context)
    (serialize-package (symbol-package object) context))
  (announce-identity (intern (read-generic-string context)
                             (deserialize-package context)) context))

(def serializer-deserializer uninterned-symbol +uninterned-symbol-code+ symbol
  (write-generic-string (symbol-name object) context)
  (announce-identity (make-symbol (read-generic-string context)) context))

(def serializer-deserializer package +package-code+ package
  (write-generic-string (package-name object) context)
  (bind ((package-name (read-generic-string context))
         (package (find-package package-name)))
    (unless package
      (restart-case
          (error "Cannot find package ~S while deserializing" package-name)
        (use-current-package ()
          :report (lambda (stream)
                    (format stream "Return *package* (~A) instead of ~S (not advised, use only when debugging)" *package* package-name))
          (setf package *package*))))
    (announce-identity package context)))

(def serializer-deserializer cons +cons-code+ cons
  (progn
    (serialize-element (car object) context)
    (serialize-element (cdr object) context))
  (prog1-bind cons (cons nil nil)
    (announce-identity cons context)
    (setf (car cons) (deserialize-element context))
    (setf (cdr cons) (deserialize-element context))))

(def serializer-deserializer proper-list +proper-list-code+ list
  (progn
    (unread-unsigned-byte-8 context)
    (write-unsigned-byte-8 +proper-list-code+ context)
    (write-variable-length-positive-integer (sc-list-length context) context)
    (dolist (element object)
      (serialize-element element context)))
  (let ((length (read-variable-length-positive-integer context)))
    (loop for index :from 0 :below length
          collect (deserialize-element context))))

(def serializer-deserializer dotted-list +dotted-list-code+ list
  (progn
    (unread-unsigned-byte-8 context)
    (write-unsigned-byte-8 +dotted-list-code+ context)
    (write-variable-length-positive-integer (sc-list-length context) context)
    (loop for cons :on object
          do (serialize-element (car cons) context)
          finally (serialize-element cons context)))
  (bind ((length (read-variable-length-positive-integer context))
         (list (loop repeat (1- length) collect (deserialize-element context))))
    (setf (cdr (last list)) (deserialize-element context))
    list))

(def (function io) %read-array (dimensions adjustable context)
  (bind ((element-type (deserialize-element context)))
    (prog1-bind object (make-array dimensions :element-type element-type :adjustable adjustable)
      (announce-identity object context)
      (loop for index :from 0 :below (array-total-size object)
         do (setf (row-major-aref object index) (deserialize-element context))))))

(def (function io) %write-array (object context)
  (serialize-element (array-element-type object) context)
  (loop for index :from 0 :below (array-total-size object)
        do (serialize-element (row-major-aref object index) context)))

(def serializer-deserializer simple-vector +simple-vector-code+ simple-vector
  (progn
    (write-variable-length-positive-integer (length object) context)
    (%write-array object context))
  (%read-array (read-variable-length-positive-integer context) #f context))

(def serializer-deserializer simple-array +simple-array-code+ simple-array
  (progn
    (serialize-element (array-dimensions object) context)
    (%write-array object context))
  (%read-array (deserialize-element context) #f context))
  
(def serializer-deserializer vector +vector-code+ vector
  (progn
    (write-variable-length-positive-integer (length object) context)
    (%write-array object context))
  (%read-array (read-variable-length-positive-integer context) #t context))

(def serializer-deserializer array +array-code+ array
  (progn
    (serialize-element (array-dimensions object) context)
    (%write-array object context))
  (%read-array (deserialize-element context) #t context))

(def serializer-deserializer hash-table +hash-table-code+ hash-table
  (progn
    (write-symbol (hash-table-test object) context)
    (write-variable-length-positive-integer (the fixnum (hash-table-count object)) context)
    (maphash (lambda (key value)
               (serialize-element key context)
               (serialize-element value context))
             object))
  (prog1-bind object (make-hash-table :test (read-symbol context))
    (loop repeat (the fixnum (read-variable-length-positive-integer context))
       do (setf (gethash (deserialize-element context) object) (deserialize-element context)))))

(def serializer-deserializer slot-object nil t
  (write-slot-object-slots object context (closer-mop:class-slots (class-of object)))
  (read-slot-object-slots context))

(def (function o) write-slot-object-slots (object context slots)
  (bind ((class (class-of object)))
    (declare (type list slots))
    (serialize-symbol (class-name class) context)
    (write-variable-length-positive-integer (length slots) context)
    (dolist (slot slots)
      (unless (eq (closer-mop:slot-definition-allocation slot) :class)
        (serialize-symbol (closer-mop:slot-definition-name slot) context)
        (if (closer-mop:slot-boundp-using-class class object slot)
            (serialize-element (closer-mop:slot-value-using-class class object slot) context)
            (write-unsigned-byte-8 +unbound-slot-code+ context))))))

(def (function o) read-slot-object-slots (context &optional make-instance)
  (bind ((class-name (deserialize-symbol context))
         (class (find-class class-name))
         (object (if make-instance
                     (make-instance class)
                     (allocate-instance class))))
    (announce-identity object context)
    (loop repeat (the fixnum (read-variable-length-positive-integer context))
       for slot-name = (deserialize-symbol context) do
       (if (eq +unbound-slot-code+ (read-unsigned-byte-8 context))
           (slot-makunbound object slot-name)
           (setf (slot-value object slot-name)
                 (progn
                   (unread-unsigned-byte-8 context)
                   (deserialize-element context)))))
    object))

(def serializer-deserializer structure-object +structure-object-code+ structure-object
  (write-slot-object object context)
  (read-slot-object context))

(def serializer-deserializer standard-object +standard-object-code+ standard-object
  (write-slot-object object context)
  (read-slot-object context))

(def serializer-deserializer standard-class +standard-class-code+ standard-class
  (write-symbol (class-name object) context)
  (announce-identity (find-class (read-symbol context)) context))

(def serializer-deserializer closer-mop:standard-direct-slot-definition +standard-direct-slot-definition-code+ closer-mop:standard-direct-slot-definition
  (progn
    #+sbcl(write-symbol (class-name (slot-value object 'sb-pcl::%class)) context)
    #-sbcl(not-yet-implemented)
    (write-symbol (closer-mop:slot-definition-name object) context))
  (announce-identity (find-direct-slot (read-symbol context) (read-symbol context)) context))

(def serializer-deserializer closer-mop:standard-effective-slot-definition +standard-effective-slot-definition-code+ closer-mop:standard-effective-slot-definition
  (progn
    #+sbcl(write-symbol (class-name (slot-value object 'sb-pcl::%class)) context)
    #-sbcl(not-yet-implemented)
    (write-symbol (closer-mop:slot-definition-name object) context))
  (announce-identity (find-slot (read-symbol context) (read-symbol context)) context))
