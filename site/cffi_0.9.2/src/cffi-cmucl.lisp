;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; cffi-sbcl.lisp --- CFFI-SYS implementation for CMU CL.
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
  (:use #:common-lisp #:alien #:c-call #:cffi-utils)
  (:export
   #:canonicalize-symbol-name-case
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
   #:%mem-ref
   #:%mem-set
   #:make-shareable-byte-vector
   #:with-pointer-to-vector-data
   #:foreign-symbol-pointer
   #:%defcallback
   #:%callback
   #:finalize
   #:cancel-finalization))

(in-package #:cffi-sys)

;;;# Features

(eval-when (:compile-toplevel :load-toplevel :execute)
  (mapc (lambda (feature) (pushnew feature *features*))
        '(;; OS/CPU features.
          #+darwin  cffi-features:darwin
          #+unix    cffi-features:unix
          #+x86     cffi-features:x86
          #+(and ppc (not ppc64)) cffi-features:ppc32
          )))

;;; Symbol case.

(defun canonicalize-symbol-name-case (name)
  (declare (string name))
  (string-upcase name))

;;;# Basic Pointer Operations

(declaim (inline pointerp))
(defun pointerp (ptr)
  "Return true if PTR is a foreign pointer."
  (sys:system-area-pointer-p ptr))

(declaim (inline pointer-eq))
(defun pointer-eq (ptr1 ptr2)
  "Return true if PTR1 and PTR2 point to the same address."
  (sys:sap= ptr1 ptr2))

(declaim (inline null-pointer))
(defun null-pointer ()
  "Construct and return a null pointer."
  (sys:int-sap 0))

(declaim (inline null-pointer-p))
(defun null-pointer-p (ptr)
  "Return true if PTR is a null pointer."
  (zerop (sys:sap-int ptr)))

(declaim (inline inc-pointer))
(defun inc-pointer (ptr offset)
  "Return a pointer pointing OFFSET bytes past PTR."
  (sys:sap+ ptr offset))

(declaim (inline make-pointer)) 
(defun make-pointer (address)
  "Return a pointer pointing to ADDRESS."
  (sys:int-sap address))

(declaim (inline pointer-address))
(defun pointer-address (ptr)
  "Return the address pointed to by PTR."
  (sys:sap-int ptr))

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

;;;# Allocation
;;;
;;; Functions and macros for allocating foreign memory on the stack
;;; and on the heap.  The main CFFI package defines macros that wrap
;;; FOREIGN-ALLOC and FOREIGN-FREE in UNWIND-PROTECT for the common usage
;;; when the memory has dynamic extent.

(defun %foreign-alloc (size)
  "Allocate SIZE bytes on the heap and return a pointer."
  (declare (type (unsigned-byte 32) size))
  (alien-funcall
   (extern-alien
    "malloc"
    (function system-area-pointer unsigned))
   size))

(defun foreign-free (ptr)
  "Free a PTR allocated by FOREIGN-ALLOC."
  (declare (type system-area-pointer ptr))
  (alien-funcall
   (extern-alien
    "free"
    (function (values) system-area-pointer))
   ptr))

;;;# Shareable Vectors
;;;
;;; This interface is very experimental.  WITH-POINTER-TO-VECTOR-DATA
;;; should be defined to perform a copy-in/copy-out if the Lisp
;;; implementation can't do this.

(defun make-shareable-byte-vector (size)
  "Create a Lisp vector of SIZE bytes that can passed to
WITH-POINTER-TO-VECTOR-DATA."
  (make-array size :element-type '(unsigned-byte 8)))

(defmacro with-pointer-to-vector-data ((ptr-var vector) &body body)
  "Bind PTR-VAR to a foreign pointer to the data in VECTOR."
  `(sys:without-gcing
     (let ((,ptr-var (sys:vector-sap ,vector)))
       ,@body)))

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
  (:char sys:signed-sap-ref-8)
  (:unsigned-char sys:sap-ref-8)
  (:short sys:signed-sap-ref-16)
  (:unsigned-short sys:sap-ref-16)
  (:int sys:signed-sap-ref-32)
  (:unsigned-int sys:sap-ref-32)
  (:long sys:signed-sap-ref-32)
  (:unsigned-long sys:sap-ref-32)
  (:long-long sys:signed-sap-ref-64)
  (:unsigned-long-long sys:sap-ref-64)
  (:float sys:sap-ref-single)
  (:double sys:sap-ref-double)
  (:pointer sys:sap-ref-sap))

;;;# Calling Foreign Functions

(defun convert-foreign-type (type-keyword)
  "Convert a CFFI type keyword to an ALIEN type."
  (ecase type-keyword
    (:char               'char)
    (:unsigned-char      'unsigned-char)
    (:short              'short)
    (:unsigned-short     'unsigned-short)
    (:int                'int)
    (:unsigned-int       'unsigned-int)
    (:long               'long)
    (:unsigned-long      'unsigned-long)
    (:long-long          '(signed 64))
    (:unsigned-long-long '(unsigned 64))
    (:float              'single-float)
    (:double             'double-float)
    (:pointer            'system-area-pointer)
    (:void               'void)))

(defun %foreign-type-size (type-keyword)
  "Return the size in bytes of a foreign type."
  (/ (alien-internals:alien-type-bits
      (alien-internals:parse-alien-type
       (convert-foreign-type type-keyword))) 8))

(defun %foreign-type-alignment (type-keyword)
  "Return the alignment in bytes of a foreign type."
  (/ (alien-internals:alien-type-alignment
      (alien-internals:parse-alien-type
       (convert-foreign-type type-keyword))) 8))

(defun foreign-funcall-type-and-args (args)
  "Return an ALIEN function type for ARGS."
  (let ((return-type nil))
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

(defmacro %foreign-funcall (name &rest args)
  "Perform a foreign function call, document it more later."
  (multiple-value-bind (types fargs rettype)
      (foreign-funcall-type-and-args args)
    `(%%foreign-funcall ,name ,types ,fargs ,rettype)))

(defmacro %foreign-funcall-pointer (ptr &rest args)
  "Funcall a pointer to a foreign function."
  (multiple-value-bind (types fargs rettype)
      (foreign-funcall-type-and-args args)
    (with-unique-names (function)
      `(with-alien ((,function (* (function ,rettype ,@types)) ,ptr))
         (alien-funcall ,function ,@fargs)))))

;;;# Callbacks

(defvar *callbacks* (make-hash-table))

;;; Create a package to contain the symbols for callback functions.  We
;;; want to redefine callbacks with the same symbol so the internal data
;;; structures are reused.
(defpackage #:cffi-callbacks
  (:use))

;;; Intern a symbol in the CFFI-CALLBACKS package used to name the internal
;;; callback for NAME.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun intern-callback (name)
    (intern (format nil "~A::~A" (package-name (symbol-package name))
                    (symbol-name name))
            '#:cffi-callbacks)))

(defmacro %defcallback (name rettype arg-names arg-types &body body)
  (let ((cb-name (intern-callback name)))
    `(progn
       (def-callback ,cb-name
           (,(convert-foreign-type rettype)
             ,@(mapcar (lambda (sym type)
                         (list sym (convert-foreign-type type)))
                       arg-names arg-types))
         ,@body)
       (setf (gethash ',name *callbacks*) (callback ,cb-name)))))

(defun %callback (name)
  (multiple-value-bind (pointer winp)
      (gethash name *callbacks*)
    (unless winp
      (error "Undefined callback: ~S" name))
    pointer))

;;;# Loading and Closing Foreign Libraries

;;; Work-around for compiling ffi code without loading the
;;; respective library at compile-time.
(setf c::top-level-lambda-max 0)

(defun %load-foreign-library (name)
  "Load the foreign library NAME."
  (sys::load-object-file name))

;;; XXX: doesn't work on Darwin; does not check for errors. I suppose we'd
;;; want something like SBCL's dlclose-or-lose in foreign-load.lisp:66
(defun %close-foreign-library (name)
  "Closes the foreign library NAME."
  (let ((lib (find name sys::*global-table* :key #'cdr :test #'string=)))
    (sys::dlclose (car lib))
    (setf (car lib) (sys:int-sap 0))))

;;;# Foreign Globals

(defun foreign-symbol-pointer (name)
  "Returns a pointer to a foreign symbol NAME."
  (let ((address (sys:alternate-get-global-address
                  (vm:extern-alien-name name))))
    (if (zerop address)
        nil
        (sys:int-sap address))))

;;;# Finalizers

(defun finalize (object function)
  "Pushes a new FUNCTION to the OBJECT's list of
finalizers. FUNCTION should take no arguments. Returns OBJECT.

For portability reasons, FUNCTION should not attempt to look at
OBJECT by closing over it because, in some lisps, OBJECT will
already have been garbage collected and is therefore not
accessible when FUNCTION is invoked."
  (ext:finalize object function))

(defun cancel-finalization (object)
  "Cancels all of OBJECT's finalizers, if any."
  (ext:cancel-finalization object))
