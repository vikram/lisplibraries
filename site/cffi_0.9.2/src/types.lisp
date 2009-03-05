;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; types.lisp --- User-defined CFFI types.
;;;
;;; Copyright (C) 2005-2006, James Bielman  <jamesjb@jamesjb.com>
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;;

(in-package #:cffi)

;;;# Built-In Types

(define-built-in-foreign-type :char)
(define-built-in-foreign-type :unsigned-char)
(define-built-in-foreign-type :short)
(define-built-in-foreign-type :unsigned-short)
(define-built-in-foreign-type :int)
(define-built-in-foreign-type :unsigned-int)
(define-built-in-foreign-type :long)
(define-built-in-foreign-type :unsigned-long)
(define-built-in-foreign-type :float)
(define-built-in-foreign-type :double)
(define-built-in-foreign-type :void)

#-cffi-features:no-long-long
(progn
  (define-built-in-foreign-type :long-long)
  (define-built-in-foreign-type :unsigned-long-long))

;;; Define the type parser for the :POINTER type.  If no type argument
;;; is provided, a void pointer will be created.
(define-type-spec-parser :pointer (&optional type)
  (if type
      (make-instance 'foreign-pointer-type :pointer-type (parse-type type))
      (make-instance 'foreign-pointer-type :pointer-type nil)))

;;; When some lisp other than SCL supports :long-double we should
;;; use #-cffi-features:no-long-double here instead.
#+(and scl long-float) (define-built-in-foreign-type :long-double)

;;;# Foreign Pointers

(define-modify-macro incf-pointer (&optional (offset 1)) inc-pointer)

(defun mem-ref (ptr type &optional (offset 0))
  "Return the value of TYPE at OFFSET bytes from PTR. If TYPE is aggregate,
we don't return its 'value' but a pointer to it, which is PTR itself."
  (let ((ptype (parse-type type)))
    (if (aggregatep ptype)
        (inc-pointer ptr offset)
        (let ((raw-value (%mem-ref ptr (canonicalize ptype) offset)))
          (if (translate-p ptype)
              (translate-type-from-foreign raw-value ptype)
              raw-value)))))

(define-compiler-macro mem-ref (&whole form ptr type &optional (offset 0))
  "Compiler macro to open-code MEM-REF when TYPE is constant."
  (if (constantp type)
      (let ((parsed-type (parse-type (eval type))))
        (if (aggregatep parsed-type)
            `(inc-pointer ,ptr ,offset)
            (expand-type-from-foreign
             `(%mem-ref ,ptr ,(canonicalize parsed-type) ,offset)
             parsed-type)))
      form))

(defun mem-set (value ptr type &optional (offset 0))
  "Set the value of TYPE at OFFSET bytes from PTR to VALUE."
  (let ((ptype (parse-type type)))
    (%mem-set (if (translate-p ptype)
                  (translate-type-to-foreign value ptype)
                  value)
              ptr (canonicalize ptype) offset)))

(define-setf-expander mem-ref (ptr type &optional (offset 0) &environment env)
  "SETF expander for MEM-REF that doesn't rebind TYPE.
This is necessary for the compiler macro on MEM-SET to be able
to open-code (SETF MEM-REF) forms."
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-expansion ptr env)
    (declare (ignore setter newval))
    ;; if either TYPE or OFFSET are constant, we avoid rebinding them
    ;; so that the compiler macros on MEM-SET and %MEM-SET work. 
    (with-unique-names (store type-tmp offset-tmp)
      (values
       (append (unless (constantp type)   (list type-tmp))
               (unless (constantp offset) (list offset-tmp))
               dummies)
       (append (unless (constantp type)   (list type))
               (unless (constantp offset) (list offset))
               vals)
       (list store)
       `(progn
          (mem-set ,store ,getter
                   ,@(if (constantp type)   (list type)   (list type-tmp))
                   ,@(if (constantp offset) (list offset) (list offset-tmp)))
          ,store)
       `(mem-ref ,getter
                 ,@(if (constantp type)   (list type)   (list type-tmp))
                 ,@(if (constantp offset) (list offset) (list offset-tmp)))))))

(define-compiler-macro mem-set
    (&whole form value ptr type &optional (offset 0))
  "Compiler macro to open-code (SETF MEM-REF) when type is constant."
  (if (constantp type)
      (let ((parsed-type (parse-type (eval type))))
        `(%mem-set ,(expand-type-to-foreign value parsed-type) ,ptr
                   ,(canonicalize parsed-type) ,offset))
      form))

;;;# Dereferencing Foreign Arrays

(defun mem-aref (ptr type &optional (index 0))
  "Like MEM-REF except for accessing 1d arrays."
  (mem-ref ptr type (* index (foreign-type-size type))))

(define-compiler-macro mem-aref (&whole form ptr type &optional (index 0))
  "Compiler macro to open-code MEM-AREF when TYPE (and eventually INDEX)."
  (if (constantp type)
      (if (constantp index)
          `(mem-ref ,ptr ,type
                    ,(* (eval index) (foreign-type-size (eval type))))
          `(mem-ref ,ptr ,type (* ,index ,(foreign-type-size (eval type)))))
      form))

(define-setf-expander mem-aref (ptr type &optional (index 0) &environment env)
  "SETF expander for MEM-AREF."
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-expansion ptr env)
    (declare (ignore setter newval))
    ;; we avoid rebinding type and index, if possible (and if type is not
    ;; constant, we don't bother about the index), so that the compiler macros
    ;; on MEM-SET or %MEM-SET can work.
    (with-unique-names (store type-tmp index-tmp)
      (values
       (append (unless (constantp type)
                 (list type-tmp))
               (unless (and (constantp type) (constantp index))
                 (list index-tmp))
               dummies)
       (append (unless (constantp type)
                 (list type))
               (unless (and (constantp type) (constantp index))
                 (list index))
               vals)
       (list store)
       ;; Here we'll try to calculate the offset from the type and index,
       ;; or if not possible at least get the type size early.
       `(progn
          ,(if (constantp type)
               (if (constantp index)
                   `(mem-set ,store ,getter ,type
                             ,(* (eval index) (foreign-type-size (eval type))))
                   `(mem-set ,store ,getter ,type
                             (* ,index-tmp ,(foreign-type-size (eval type)))))
               `(mem-set ,store ,getter ,type-tmp
                         (* ,index-tmp (foreign-type-size ,type-tmp))))
          ,store)
       `(mem-aref ,getter
                  ,@(if (constantp type)
                        (list type)
                        (list type-tmp))
                  ,@(if (and (constantp type) (constantp index))
                        (list index)
                        (list index-tmp)))))))

;;;# Foreign Structures

;;;## Foreign Structure Slots

(defgeneric foreign-struct-slot-pointer (ptr slot)
  (:documentation
   "Get the address of SLOT relative to PTR."))

(defgeneric foreign-struct-slot-pointer-form (ptr slot)
  (:documentation
   "Return a form to get the address of SLOT in PTR."))

(defgeneric foreign-struct-slot-value (ptr slot)
  (:documentation
   "Return the value of SLOT in structure PTR."))

(defgeneric (setf foreign-struct-slot-value) (value ptr slot)
  (:documentation
   "Set the value of a SLOT in structure PTR."))

(defgeneric foreign-struct-slot-value-form (ptr slot)
  (:documentation
   "Return a form to get the value of SLOT in struct PTR."))

(defgeneric foreign-struct-slot-set-form (value ptr slot)
  (:documentation
   "Return a form to set the value of SLOT in struct PTR."))

(defclass foreign-struct-slot ()
  ((name   :initarg :name   :reader   slot-name)
   (offset :initarg :offset :accessor slot-offset)
   (type   :initarg :type   :accessor slot-type))
  (:documentation "Base class for simple and aggregate slots."))

(defmethod foreign-struct-slot-pointer (ptr (slot foreign-struct-slot))
  "Return the address of SLOT relative to PTR."
  (inc-pointer ptr (slot-offset slot)))

(defmethod foreign-struct-slot-pointer-form (ptr (slot foreign-struct-slot))
  "Return a form to get the address of SLOT relative to PTR."
  (let ((offset (slot-offset slot)))
    (if (zerop offset)
        ptr
        `(inc-pointer ,ptr ,offset))))

(defun foreign-slot-names (type)
  "Returns a list of TYPE's slot names in no particular order."
  (loop for value being the hash-values
        in (slots (follow-typedefs (parse-type type)))
        collect (slot-name value)))

;;;### Simple Slots

(defclass simple-struct-slot (foreign-struct-slot)
  ()
  (:documentation "Non-aggregate structure slots."))

(defmethod foreign-struct-slot-value (ptr (slot simple-struct-slot))
  "Return the value of a simple SLOT from a struct at PTR."
  (mem-ref ptr (slot-type slot) (slot-offset slot)))

(defmethod foreign-struct-slot-value-form (ptr (slot simple-struct-slot))
  "Return a form to get the value of a slot from PTR."
  `(mem-ref ,ptr ',(slot-type slot) ,(slot-offset slot)))

(defmethod (setf foreign-struct-slot-value) (value ptr (slot simple-struct-slot))
  "Set the value of a simple SLOT to VALUE in PTR."
  (setf (mem-ref ptr (slot-type slot) (slot-offset slot)) value))

(defmethod foreign-struct-slot-set-form (value ptr (slot simple-struct-slot))
  "Return a form to set the value of a simple structure slot."
  `(setf (mem-ref ,ptr ',(slot-type slot) ,(slot-offset slot)) ,value))

;;;### Aggregate Slots

(defclass aggregate-struct-slot (foreign-struct-slot)
  ((count :initarg :count :accessor slot-count))
  (:documentation "Aggregate structure slots."))

;;; A case could be made for just returning an error here instead of
;;; this rather DWIM-ish behavior to return the address.  It would
;;; complicate being able to chain together slot names when accessing
;;; slot values in nested structures though.
(defmethod foreign-struct-slot-value (ptr (slot aggregate-struct-slot))
  "Return a pointer to SLOT relative to PTR."
  (foreign-struct-slot-pointer ptr slot))

(defmethod foreign-struct-slot-value-form (ptr (slot aggregate-struct-slot))
  "Return a form to get the value of SLOT relative to PTR."
  (foreign-struct-slot-pointer-form ptr slot))

;;; This is definitely an error though.  Eventually, we could define a
;;; new type of type translator that can convert certain aggregate
;;; types, notably C strings or arrays of integers.  For now, just error.
(defmethod (setf foreign-struct-slot-value) (value ptr (slot aggregate-struct-slot))
  "Signal an error; setting aggregate slot values is forbidden."
  (declare (ignore value ptr))
  (error "Cannot set value of aggregate slot ~A." slot))

(defmethod foreign-struct-slot-set-form (value ptr (slot aggregate-struct-slot))
  "Signal an error; setting aggregate slot values is forbidden."
  (declare (ignore value ptr))
  (error "Cannot set value of aggregate slot ~A." slot))

;;;## Defining Foreign Structures

(defun make-struct-slot (name offset type count)
  "Make the appropriate type of structure slot."
  ;; If TYPE is an aggregate type or COUNT is >1, create an
  ;; AGGREGATE-STRUCT-SLOT, otherwise a SIMPLE-STRUCT-SLOT.
  (if (or (> count 1) (aggregatep (parse-type type)))
      (make-instance 'aggregate-struct-slot :offset offset :type type
                     :name name :count count)
      (make-instance 'simple-struct-slot :offset offset :type type
                     :name name)))

;;; Regarding structure alignment, the following ABIs were checked:
;;;   - System-V ABI: x86, x86-64, ppc, arm, mips and itanium. (more?)
;;;   - Mac OS X ABI Function Call Guide: ppc32, ppc64 and x86.
;;;
;;; Rules used here:
;;;
;;;   1. "An entire structure or union object is aligned on the same boundary
;;;       as its most strictly aligned member."
;;;   2. "Each member is assigned to the lowest available offset with the
;;;       appropriate alignment. This may require internal padding, depending
;;;       on the previous member."
;;;   3. "A structure's size is increased, if necessary, to make it a multiple
;;;       of the alignment. This may require tail padding, depending on the last
;;;       member."
;;;
;;; Special case from darwin/ppc32's ABI:
;;; http://developer.apple.com/documentation/DeveloperTools/Conceptual/LowLevelABI/index.html
;;;
;;;   1. "The embedding alignment of the first element in a data structure is
;;;       equal to the element's natural alignment."
;;;   2. "For subsequent elements that have a natural alignment greater than 4
;;;       bytes, the embedding alignment is 4, unless the element is a vector."
;;;       (note: this applies for structures too)

;; FIXME: get a better name for this. --luis
(defun get-alignment (type alignment-type firstp)
  "Return alignment for TYPE according to ALIGNMENT-TYPE."
  (declare (ignorable firstp))
  (ecase alignment-type
    (:normal #-(and cffi-features:darwin cffi-features:ppc32)
             (foreign-type-alignment type)
             #+(and cffi-features:darwin cffi-features:ppc32)
             (if firstp
                 (foreign-type-alignment type)
                 (min 4 (foreign-type-alignment type))))))

(defun adjust-for-alignment (type offset alignment-type firstp)
  "Return OFFSET aligned properly for TYPE according to ALIGNMENT-TYPE."
  (let* ((align (get-alignment type alignment-type firstp))
         (rem (mod offset align)))
    (if (zerop rem)
        offset
        (+ offset (- align rem)))))

(defun notice-foreign-struct-definition (name-and-options slots)
  "Parse and install a foreign structure definition."
  (destructuring-bind (name &key size #+nil alignment)
      (mklist name-and-options)
    (let ((struct (make-instance 'foreign-struct-type :name name))
          (current-offset 0)
          (max-align 1)
          (firstp t))
      ;; determine offsets
      (dolist (slotdef slots)
        (destructuring-bind (slotname type &key (count 1) offset) slotdef
          (when (eq (canonicalize-foreign-type type) :void)
            (error "void type not allowed in structure definition: ~S" slotdef))
          (setq current-offset
                (or offset
                    (adjust-for-alignment type current-offset :normal firstp)))
          (let* ((slot (make-struct-slot slotname current-offset type count))
                 (align (get-alignment (slot-type slot) :normal firstp)))
            (setf (gethash slotname (slots struct)) slot)
            (when (> align max-align)
              (setq max-align align)))
          (incf current-offset (* count (foreign-type-size type))))
        (setq firstp nil))
      ;; calculate padding and alignment
      (setf (alignment struct) max-align) ; See point 1 above.
      (let ((tail-padding (- max-align (rem current-offset max-align))))
        (unless (= tail-padding max-align) ; See point 3 above.
          (incf current-offset tail-padding)))
      (setf (size struct) (or size current-offset))
      (notice-foreign-type struct))))

(defmacro defcstruct (name &body fields)
  "Define the layout of a foreign structure."
  (discard-docstring fields)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (notice-foreign-struct-definition ',name ',fields)))

;;;## Accessing Foreign Structure Slots

(defun get-slot-info (type slot-name)
  "Return the slot info for SLOT-NAME or raise an error."
  (let* ((struct (follow-typedefs (parse-type type)))
         (info (gethash slot-name (slots struct))))
    (unless info
      (error "Undefined slot ~A in foreign type ~A." slot-name type))
    info))

(defun foreign-slot-pointer (ptr type slot-name)
  "Return the address of SLOT-NAME in the structure at PTR."
  (foreign-struct-slot-pointer ptr (get-slot-info type slot-name)))

(defun foreign-slot-offset (type slot-name)
  "Return the offset of SLOT in a struct TYPE."
  (slot-offset (get-slot-info type slot-name)))

(defun foreign-slot-value (ptr type slot-name)
  "Return the value of SLOT-NAME in the foreign structure at PTR."
  (foreign-struct-slot-value ptr (get-slot-info type slot-name)))

(define-compiler-macro foreign-slot-value (&whole form ptr type slot-name)
  "Optimizer for FOREIGN-SLOT-VALUE when TYPE is constant."
  (if (and (constantp type) (constantp slot-name))
      (foreign-struct-slot-value-form
       ptr (get-slot-info (eval type) (eval slot-name)))
      form))

(define-setf-expander foreign-slot-value (ptr type slot-name &environment env)
  "SETF expander for FOREIGN-SLOT-VALUE."
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-expansion ptr env)
    (declare (ignore setter newval))
    (if (and (constantp type) (constantp slot-name))
        ;; if TYPE and SLOT-NAME are constant we avoid rebinding them
        ;; so that the compiler macro on FOREIGN-SLOT-SET works.
        (with-unique-names (store)
          (values
           dummies
           vals
           (list store)
           `(progn
              (foreign-slot-set ,store ,getter ,type ,slot-name)
              ,store)
           `(foreign-slot-value ,getter ,type ,slot-name)))
        ;; if not...
        (with-unique-names (store slot-name-tmp type-tmp)
          (values
           (list* type-tmp slot-name-tmp dummies)
           (list* type slot-name vals)
           (list store)
           `(progn
              (foreign-slot-set ,store ,getter ,type-tmp ,slot-name-tmp)
              ,store)
           `(foreign-slot-value ,getter ,type-tmp ,slot-name-tmp))))))

(defun foreign-slot-set (value ptr type slot-name)
  "Set the value of SLOT-NAME in a foreign structure."
  (setf (foreign-struct-slot-value ptr (get-slot-info type slot-name)) value))

(define-compiler-macro foreign-slot-set
    (&whole form value ptr type slot-name)
  "Optimizer when TYPE and SLOT-NAME are constant."
  (if (and (constantp type) (constantp slot-name))
      (foreign-struct-slot-set-form
       value ptr (get-slot-info (eval type) (eval slot-name)))
      form))

(defmacro with-foreign-slots ((vars ptr type) &body body)
  "Create local symbol macros for each var in VARS to reference
foreign slots in PTR of TYPE.  Similar to WITH-SLOTS."
  (let ((ptr-var (gensym "PTR")))
    `(let ((,ptr-var ,ptr))
       (symbol-macrolet
           ,(loop for var in vars
                  collect `(,var (foreign-slot-value ,ptr-var ',type ',var)))
         ,@body))))

;;;# Foreign Unions
;;;
;;; A union is a FOREIGN-STRUCT-TYPE in which all slots have an offset
;;; of zero.

;;; See also the notes regarding ABI requirements in
;;; NOTICE-FOREIGN-STRUCT-DEFINITION
(defun notice-foreign-union-definition (name-and-options slots)
  "Parse and install a foreign union definition."
  (destructuring-bind (name &key size)
      (mklist name-and-options)
    (let ((struct (make-instance 'foreign-struct-type :name name))
          (max-size 0)
          (max-align 0))
      (dolist (slotdef slots)
        (destructuring-bind (slotname type &key (count 1)) slotdef
          (when (eq (canonicalize-foreign-type type) :void)
            (error "void type not allowed in union definition: ~S" slotdef))
          (let* ((slot (make-struct-slot slotname 0 type count))
                 (size (* count (foreign-type-size type)))
                 (align (foreign-type-alignment (slot-type slot))))
            (setf (gethash slotname (slots struct)) slot)
            (when (> size max-size)
              (setf max-size size))
            (when (> align max-align)
              (setf max-align align)))))
      (setf (size struct) (or size max-size))
      (setf (alignment struct) max-align)
      (notice-foreign-type struct))))

(defmacro defcunion (name &body fields)
  "Define the layout of a foreign union."
  (discard-docstring fields)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (notice-foreign-union-definition ',name ',fields)))

;;;# Operations on Types

(defmethod foreign-type-alignment (type)
  "Return the alignment in bytes of a foreign type."
  (foreign-type-alignment (parse-type type)))

(defun foreign-alloc (type &key (initial-element nil initial-element-p)
                      (initial-contents nil initial-contents-p)
                      (count 1 count-p) null-terminated-p)
  "Allocate enough memory to hold COUNT objects of type TYPE. If
INITIAL-ELEMENT is supplied, each element of the newly allocated
memory is initialized with its value. If INITIAL-CONTENTS is supplied,
each of its elements will be used to initialize the contents of the
newly allocated memory."
  (let (contents-length)
    ;; Some error checking, etc...
    (when (and null-terminated-p
               (not (eq (canonicalize-foreign-type type) :pointer)))
      (error "Cannot use :NULL-TERMINATED-P with non-pointer types."))
    (when (and initial-element-p initial-contents-p)
      (error "Cannot specify both :INITIAL-ELEMENT and :INITIAL-CONTENTS"))
    (when initial-contents-p
      (setq contents-length (length initial-contents))
      (if count-p
          (assert (>= count contents-length))
          (setq count contents-length)))
    ;; Everything looks good.
    (let ((ptr (%foreign-alloc (* (foreign-type-size type)
                                  (if null-terminated-p (1+ count) count)))))
      (when initial-element-p
        (dotimes (i count)
          (setf (mem-aref ptr type i) initial-element)))
      (when initial-contents-p
        (dotimes (i contents-length)
          (setf (mem-aref ptr type i) (elt initial-contents i))))
      (when null-terminated-p
        (setf (mem-aref ptr :pointer count) (null-pointer)))
      ptr)))

;;; Stuff we could optimize here:
;;;   1. (and (constantp type) (constantp count)) => calculate size
;;;   2. (constantp type) => use the translators' expanders
#-(and)
(define-compiler-macro foreign-alloc
    (&whole form type &key (initial-element nil initial-element-p)
     (initial-contents nil initial-contents-p) (count 1 count-p))
  )

(defmacro with-foreign-object ((var type &optional (count 1)) &body body)
  "Bind VAR to a pointer to COUNT objects of TYPE during BODY.
The buffer has dynamic extent and may be stack allocated."
  `(with-foreign-pointer
       (,var ,(if (constantp type)
                  ;; with-foreign-pointer may benefit from constant folding:
                  (if (constantp count)
                      (* (eval count) (foreign-type-size (eval type)))
                      `(* ,count ,(foreign-type-size (eval type))))
                  `(* ,count (foreign-type-size ,type))))
     ,@body))

(defmacro with-foreign-objects (bindings &rest body)
  (if bindings
      `(with-foreign-object ,(car bindings)
         (with-foreign-objects ,(cdr bindings)
           ,@body))
      `(progn ,@body)))

;;;# User-defined Types and Translations.

(defmacro define-foreign-type (type lambda-list &body body)
  "Define a parameterized type."
  (discard-docstring body)
  `(progn
     (define-type-spec-parser ,type ,lambda-list
       (make-instance 'foreign-typedef :name ',type
                      :actual-type (parse-type (progn ,@body))))
     ',type))

(defmacro defctype (name base-type &key (translate-p t) documentation)
  "Utility macro for simple C-like typedefs. A similar effect could be
obtained using define-foreign-type."
  (declare (ignore documentation))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (notice-foreign-type
      (make-instance 'foreign-typedef :name ',name
                     :actual-type (parse-type ',base-type)
                     :translate-p ,translate-p))))

;;;## Anonymous Type Translators
;;;
;;; (:wrapper :to-c some-function :from-c another-function)
;;;
;;; TODO: We will need to add a FREE function to this as well I think.
;;; --james

(defclass foreign-type-wrapper (foreign-typedef)
  ((to-c   :initarg :to-c)
   (from-c :initarg :from-c))
  (:documentation "Class for the wrapper type."))

(define-type-spec-parser :wrapper (base-type &key to-c from-c)
  (make-instance 'foreign-type-wrapper 
                 :actual-type (parse-type base-type)
                 :to-c (or to-c 'identity)
                 :from-c (or from-c 'identity)))

(defmethod unparse (name (type foreign-type-wrapper))
  (declare (ignore name))
  `(:wrapper ,(name (actual-type type))
             :to-c ,(slot-value type 'to-c)
             :from-c ,(slot-value type 'from-c)))

(defmethod translate-type-to-foreign (value (type foreign-type-wrapper))
  (let ((actual-type (actual-type type)))
    (translate-type-to-foreign
     (funcall (slot-value type 'to-c) value) actual-type)))

(defmethod translate-type-from-foreign (value (type foreign-type-wrapper))
  (let ((actual-type (actual-type type)))
    (funcall (slot-value type 'from-c)
             (translate-type-from-foreign value actual-type))))

;;;# Other types

(define-foreign-type :boolean (&optional (base-type :int))
  "Boolean type. Maps to an :int by default. Only accepts integer types."
  (ecase (canonicalize-foreign-type base-type)
    ((:char
      :unsigned-char
      :int
      :unsigned-int
      :long
      :unsigned-long) base-type)))

(defmethod unparse ((name (eql :boolean)) type)
  "Unparser for the :BOOLEAN type."
  `(:boolean ,(name (actual-type type))))

(defmethod translate-to-foreign (value (name (eql :boolean)))
  (if value 1 0))

(defmethod translate-from-foreign (value (name (eql :boolean)))
  (not (zerop value)))

(defmethod expand-to-foreign (value (name (eql :boolean)))
  "Optimization for the :boolean type."
  (if (constantp value)
      (if (eval value) 1 0)
      `(if ,value 1 0)))

(defmethod expand-from-foreign (value (name (eql :boolean)))
  "Optimization for the :boolean type."
  (if (constantp value) ; very unlikely, heh
      (not (zerop (eval value)))
      `(not (zerop ,value))))

;;;# Typedefs for built-in types.

(defctype :uchar  :unsigned-char :translate-p nil)
(defctype :ushort :unsigned-short :translate-p nil)
(defctype :uint   :unsigned-int :translate-p nil)
(defctype :ulong  :unsigned-long :translate-p nil)

#-cffi-features:no-long-long
(progn
  (defctype :llong  :long-long :translate-p nil)
  (defctype :ullong :unsigned-long-long :translate-p nil))

;;; We try to define the :[u]int{8,16,32,64} types by looking at
;;; the sizes of the built-in integer types and defining typedefs.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (labels ((find-matching-size (size types)
             (car (member size types :key #'foreign-type-size)))
           (notice-foreign-typedef (type actual-type)
             (notice-foreign-type
              (make-instance 'foreign-typedef :name type
                             :actual-type (find-type actual-type)
                             :translate-p nil)))
           (match-types (sized-types builtin-types)
             (loop for (type . size) in sized-types do
                   (let ((match (find-matching-size size builtin-types)))
                     (when match
                       (notice-foreign-typedef type match))))))
    ;; signed
    (match-types '((:int8 . 1) (:int16 . 2) (:int32 . 4) (:int64 . 8))
                 '(:char :short :int :long
                   #-cffi-features:no-long-long :long-long))
    ;; unsigned
    (match-types '((:uint8 . 1) (:uint16 . 2) (:uint32 . 4) (:uint64 . 8))
                 '(:unsigned-char :unsigned-short :unsigned-int :unsigned-long
                   #-cffi-features:no-long-long :unsigned-long-long))))