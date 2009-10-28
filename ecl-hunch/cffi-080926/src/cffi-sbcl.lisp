;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; cffi-sbcl.lisp --- CFFI-SYS implementation for SBCL.
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

;;;# Administrivia

(defpackage #:cffi-sys
  (:use #:common-lisp #:sb-alien)
  (:import-from #:alexandria #:once-only #:with-unique-names #:when-let)
  (:export
   #:canonicalize-symbol-name-case
   #:foreign-pointer
   #:pointerp
   #:pointer-eq
   #:null-pointer
   #:null-pointer-p
   #:inc-pointer
   #:make-pointer
   #:pointer-address
   #:%foreign-alloc
   #:foreign-free
   #:with-foreign-pointer
   #:%foreign-funcall
   #:%foreign-funcall-pointer
   #:%foreign-type-alignment
   #:%foreign-type-size
   #:%load-foreign-library
   #:%close-foreign-library
   #:native-namestring
   #:%mem-ref
   #:%mem-set
   #:make-shareable-byte-vector
   #:with-pointer-to-vector-data
   #:%foreign-symbol-pointer
   #:%defcallback
   #:%callback))

(in-package #:cffi-sys)

;;;# Misfeatures

(pushnew 'flat-namespace *features*)

;;;# Symbol Case

(declaim (inline canonicalize-symbol-name-case))
(defun canonicalize-symbol-name-case (name)
  (declare (string name))
  (string-upcase name))

;;;# Basic Pointer Operations

(deftype foreign-pointer ()
  'sb-sys:system-area-pointer)

(declaim (inline pointerp))
(defun pointerp (ptr)
  "Return true if PTR is a foreign pointer."
  (sb-sys:system-area-pointer-p ptr))

(declaim (inline pointer-eq))
(defun pointer-eq (ptr1 ptr2)
  "Return true if PTR1 and PTR2 point to the same address."
  (declare (type system-area-pointer ptr1 ptr2))
  (sb-sys:sap= ptr1 ptr2))

(declaim (inline null-pointer))
(defun null-pointer ()
  "Construct and return a null pointer."
  (sb-sys:int-sap 0))

(declaim (inline null-pointer-p))
(defun null-pointer-p (ptr)
  "Return true if PTR is a null pointer."
  (declare (type system-area-pointer ptr))
  (zerop (sb-sys:sap-int ptr)))

(declaim (inline inc-pointer))
(defun inc-pointer (ptr offset)
  "Return a pointer pointing OFFSET bytes past PTR."
  (declare (type system-area-pointer ptr)
           (type integer offset))
  (sb-sys:sap+ ptr offset))

(declaim (inline make-pointer))
(defun make-pointer (address)
  "Return a pointer pointing to ADDRESS."
  ;; (declare (type (unsigned-byte 32) address))
  (sb-sys:int-sap address))

(declaim (inline pointer-address))
(defun pointer-address (ptr)
  "Return the address pointed to by PTR."
  (declare (type system-area-pointer ptr))
  (sb-sys:sap-int ptr))

;;;# Allocation
;;;
;;; Functions and macros for allocating foreign memory on the stack
;;; and on the heap.  The main CFFI package defines macros that wrap
;;; FOREIGN-ALLOC and FOREIGN-FREE in UNWIND-PROTECT for the common usage
;;; when the memory has dynamic extent.

(declaim (inline %foreign-alloc))
(defun %foreign-alloc (size)
  "Allocate SIZE bytes on the heap and return a pointer."
  ;; (declare (type (unsigned-byte 32) size))
  (alien-sap (make-alien (unsigned 8) size)))

(declaim (inline foreign-free))
(defun foreign-free (ptr)
  "Free a PTR allocated by FOREIGN-ALLOC."
  (declare (type system-area-pointer ptr))
  (free-alien (sap-alien ptr (* (unsigned 8)))))

(defmacro with-foreign-pointer ((var size &optional size-var) &body body)
  "Bind VAR to SIZE bytes of foreign memory during BODY.  The
pointer in VAR is invalid beyond the dynamic extent of BODY, and
may be stack-allocated if supported by the implementation.  If
SIZE-VAR is supplied, it will be bound to SIZE during BODY."
  (unless size-var
    (setf size-var (gensym "SIZE")))
  ;; If the size is constant we can stack-allocate.
  (if (constantp size)
      (let ((alien-var (gensym "ALIEN")))
        `(with-alien ((,alien-var (array (unsigned 8) ,(eval size))))
           (let ((,size-var ,(eval size))
                 (,var (alien-sap ,alien-var)))
             (declare (ignorable ,size-var))
             ,@body)))
      `(let* ((,size-var ,size)
              (,var (%foreign-alloc ,size-var)))
         (unwind-protect
              (progn ,@body)
           (foreign-free ,var)))))

;;;# Shareable Vectors
;;;
;;; This interface is very experimental.  WITH-POINTER-TO-VECTOR-DATA
;;; should be defined to perform a copy-in/copy-out if the Lisp
;;; implementation can't do this.

(declaim (inline make-shareable-byte-vector))
(defun make-shareable-byte-vector (size)
  "Create a Lisp vector of SIZE bytes can passed to
WITH-POINTER-TO-VECTOR-DATA."
  ; (declare (type sb-int:index size))
  (make-array size :element-type '(unsigned-byte 8)))

(defmacro with-pointer-to-vector-data ((ptr-var vector) &body body)
  "Bind PTR-VAR to a foreign pointer to the data in VECTOR."
  (let ((vector-var (gensym "VECTOR")))
    `(let ((,vector-var ,vector))
       (declare (type (sb-kernel:simple-unboxed-array (*)) ,vector-var))
       (sb-sys:with-pinned-objects (,vector-var)
         (let ((,ptr-var (sb-sys:vector-sap ,vector-var)))
           ,@body)))))

;;;# Dereferencing

;;; Define the %MEM-REF and %MEM-SET functions, as well as compiler
;;; macros that optimize the case where the type keyword is constant
;;; at compile-time.
(defmacro define-mem-accessors (&body pairs)
  `(progn
     (defun %mem-ref (ptr type &optional (offset 0))
       (ecase type
         ,@(loop for (keyword fn) in pairs
                 collect `(,keyword (,fn ptr offset)))))
     (defun %mem-set (value ptr type &optional (offset 0))
       (ecase type
         ,@(loop for (keyword fn) in pairs
                 collect `(,keyword (setf (,fn ptr offset) value)))))
     (define-compiler-macro %mem-ref
         (&whole form ptr type &optional (offset 0))
       (if (constantp type)
           (ecase (eval type)
             ,@(loop for (keyword fn) in pairs
                     collect `(,keyword `(,',fn ,ptr ,offset))))
           form))
     (define-compiler-macro %mem-set
         (&whole form value ptr type &optional (offset 0))
       (if (constantp type)
           (once-only (value)
             (ecase (eval type)
               ,@(loop for (keyword fn) in pairs
                       collect `(,keyword `(setf (,',fn ,ptr ,offset)
                                                 ,value)))))
           form))))

(define-mem-accessors
  (:char sb-sys:signed-sap-ref-8)
  (:unsigned-char sb-sys:sap-ref-8)
  (:short sb-sys:signed-sap-ref-16)
  (:unsigned-short sb-sys:sap-ref-16)
  (:int sb-sys:signed-sap-ref-32)
  (:unsigned-int sb-sys:sap-ref-32)
  (:long sb-sys:signed-sap-ref-word)
  (:unsigned-long sb-sys:sap-ref-word)
  (:long-long sb-sys:signed-sap-ref-64)
  (:unsigned-long-long sb-sys:sap-ref-64)
  (:float sb-sys:sap-ref-single)
  (:double sb-sys:sap-ref-double)
  (:pointer sb-sys:sap-ref-sap))

;;;# Calling Foreign Functions

(defun convert-foreign-type (type-keyword)
  "Convert a CFFI type keyword to an SB-ALIEN type."
  (ecase type-keyword
    (:char               'char)
    (:unsigned-char      'unsigned-char)
    (:short              'short)
    (:unsigned-short     'unsigned-short)
    (:int                'int)
    (:unsigned-int       'unsigned-int)
    (:long               'long)
    (:unsigned-long      'unsigned-long)
    (:long-long          'long-long)
    (:unsigned-long-long 'unsigned-long-long)
    (:float              'single-float)
    (:double             'double-float)
    (:pointer            'system-area-pointer)
    (:void               'void)))

(defun %foreign-type-size (type-keyword)
  "Return the size in bytes of a foreign type."
  (/ (sb-alien-internals:alien-type-bits
      (sb-alien-internals:parse-alien-type
       (convert-foreign-type type-keyword) nil)) 8))

(defun %foreign-type-alignment (type-keyword)
  "Return the alignment in bytes of a foreign type."
  #+(and darwin ppc (not ppc64))
  (case type-keyword
    ((:double :long-long :unsigned-long-long)
     (return-from %foreign-type-alignment 8)))
  ;; No override necessary for other types...
  (/ (sb-alien-internals:alien-type-alignment
      (sb-alien-internals:parse-alien-type
       (convert-foreign-type type-keyword) nil)) 8))

(defun foreign-funcall-type-and-args (args)
  "Return an SB-ALIEN function type for ARGS."
  (let ((return-type 'void))
    (loop for (type arg) on args by #'cddr
          if arg collect (convert-foreign-type type) into types
          and collect arg into fargs
          else do (setf return-type (convert-foreign-type type))
          finally (return (values types fargs return-type)))))

(defmacro %%foreign-funcall (name types fargs rettype)
  "Internal guts of %FOREIGN-FUNCALL."
  `(alien-funcall
    (extern-alien ,name (function ,rettype ,@types))
    ,@fargs))

(defmacro %foreign-funcall (name args &key library calling-convention)
  "Perform a foreign function call, document it more later."
  (declare (ignore library calling-convention))
  (multiple-value-bind (types fargs rettype)
      (foreign-funcall-type-and-args args)
    `(%%foreign-funcall ,name ,types ,fargs ,rettype)))

(defmacro %foreign-funcall-pointer (ptr args &key calling-convention)
  "Funcall a pointer to a foreign function."
  (declare (ignore calling-convention))
  (multiple-value-bind (types fargs rettype)
      (foreign-funcall-type-and-args args)
    (with-unique-names (function)
      `(with-alien ((,function (* (function ,rettype ,@types)) ,ptr))
         (alien-funcall ,function ,@fargs)))))

;;;# Callbacks

;;; The *CALLBACKS* hash table contains a direct mapping of CFFI
;;; callback names to SYSTEM-AREA-POINTERs obtained by ALIEN-LAMBDA.
;;; SBCL will maintain the addresses of the callbacks across saved
;;; images, so it is safe to store the pointers directly.
(defvar *callbacks* (make-hash-table))

(defmacro %defcallback (name rettype arg-names arg-types body
                        &key calling-convention)
  (declare (ignore calling-convention))
  `(setf (gethash ',name *callbacks*)
         (alien-sap
          (sb-alien::alien-lambda ,(convert-foreign-type rettype)
              ,(mapcar (lambda (sym type)
                         (list sym (convert-foreign-type type)))
                       arg-names arg-types)
            ,body))))

(defun %callback (name)
  (or (gethash name *callbacks*)
      (error "Undefined callback: ~S" name)))

;;;# Loading and Closing Foreign Libraries

(declaim (inline %load-foreign-library))
(defun %load-foreign-library (name path)
  "Load a foreign library."
  (declare (ignore name))
  (load-shared-object path))

(defun %close-foreign-library (handle)
  "Closes a foreign library."
  (sb-alien::dlclose-or-lose
   (find (sb-ext:native-namestring handle) sb-alien::*shared-objects*
         :key #'sb-alien::shared-object-file
         :test #'string=)))

(defun native-namestring (pathname)
  (sb-ext:native-namestring pathname))

;;;# Foreign Globals

(defun %foreign-symbol-pointer (name library)
  "Returns a pointer to a foreign symbol NAME."
  (declare (ignore library))
  (when-let (address (sb-sys:find-foreign-symbol-address name))
    (sb-sys:int-sap address)))
