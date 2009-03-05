;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-serializer)

(def macro prog1-bind (var ret &body forms)
  `(bind ((,var ,ret))
    ,@forms
    ,var))

(def function concatenate-symbol (&rest args)
  "Args are processed as parts of the result symbol with an exception: when a package is encountered then it is stored as the target package at intern."
  (let* ((package nil)
         (symbol-name (string-upcase
                       (with-output-to-string (str)
                         (dolist (arg args)
                           (typecase arg
                             (string (cl:write-string arg str))
                             (package (setf package arg))
                             (symbol (unless package
                                       (setf package (symbol-package arg)))
                                     (cl:write-string (symbol-name arg) str))
                             (integer (cl:write-string (princ-to-string arg) str))
                             (character (cl:write-char arg) str)
                             (t (error "Cannot convert argument ~S to symbol" arg))))))))
    (if package
        (intern symbol-name package)
        (intern symbol-name))))

(def macro ensure-simple-vector-size (vector size)
  `(let ((length (length ,vector)))
     (when (< length ,size)
       (let ((new-vector (make-array (max (1+ (* 2 length)) ,size) :element-type '(unsigned-byte 8))))
         (replace new-vector ,vector)
         (setf ,vector new-vector)))))

(def (function o) read-stream-into-vector (stream)
  (bind ((buffer-size 1024)
         (buffer (make-array buffer-size :element-type '(unsigned-byte 8)))
         (buffer-pointer 0)
         (read-buffer (make-array buffer-size :element-type '(unsigned-byte 8))))
    (loop for bytes-read = (read-sequence read-buffer stream)
       do (progn
            (ensure-simple-vector-size buffer (+ buffer-pointer bytes-read))
            (replace buffer read-buffer :start1 buffer-pointer :end2 bytes-read)
            (incf buffer-pointer bytes-read))
       while (= bytes-read buffer-size)
       finally (return
                 (prog1-bind result-buffer (make-array buffer-pointer :element-type '(unsigned-byte 8))
                   (replace result-buffer buffer :end2 buffer-pointer))))))
