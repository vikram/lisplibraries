;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLOUCHDB; Base: 10 -*-

;;; Copyright (c) 2007 Peter Eddy. All rights reserved.

;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:

;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.

;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

;; The decoder in the cl-json package didn't work the way I needed it
;; to, hence this code which is mostly stolen from that package.

(in-package :clouchdb)

(defvar *json-symbols-package* (find-package 'keyword) 
  "The package where json-symbols are interned. Default keyword, nil = current package")

;; (defun json-intern (string)
;;   (if *json-symbols-package*
;;       (intern (camel-case-to-lisp string) *json-symbols-package*)
;;       (intern (camel-case-to-lisp string))))

(defun json-intern (string)
  (as-keyword-symbol string))

(defparameter *json-rules* nil)

(defparameter *json-object-factory* #'(lambda () nil))
(defparameter *json-object-factory-add-key-value* #'(lambda (obj key value)
                                                      (push (cons (json-intern key) value)
                                                            obj)))
(defparameter *json-object-factory-return* #'(lambda (obj) (nreverse obj)))
(defparameter *json-make-big-number* #'(lambda (number-string) (format nil "BIGNUMBER:~a" number-string)))

(define-condition json-parse-error (error) ())

(defparameter *json-lisp-escaped-chars*
  `((#\" . #\")
    (#\\ . #\\)
    (#\/ . #\/)
    (#\b . #\Backspace)
    (#\f . ,(code-char 12))
    (#\n . #\Newline)
    (#\r . #\Return)
    (#\t . #\Tab)))

(defparameter *use-strict-json-rules* t)

(defun json-escaped-char-to-lisp(json-escaped-char)
  (let ((ch (cdr (assoc json-escaped-char *json-lisp-escaped-chars*))))
    (if *use-strict-json-rules*
        (or ch (error 'json-parse-error))
        (or ch json-escaped-char))))

(defun lisp-special-char-to-json(lisp-char)
    (car (rassoc lisp-char *json-lisp-escaped-chars*)))

(defun json-to-document (json-string)
  (with-input-from-string (stream json-string)
    (decode-json stream)))

(defun decode-json (&optional (stream *standard-input*))
  "Reads a json element from stream"
  (funcall (or (cdr (assoc (peek-char t stream) *json-rules*))
               #'read-json-number)
           stream))

(defun decode-json-strict (&optional (stream *standard-input*))
  "Only objects or arrays on top level, no junk afterwards."
  (assert (member (peek-char t stream) '(#\{ #\[)))
  (let ((object (decode-json stream)))
    (assert (eq :no-junk (peek-char t stream nil :no-junk)))
    object))

;;-----------------------


(defun add-json-dispatch-rule (character fn)
  (push (cons character fn) *json-rules*))

(add-json-dispatch-rule #\t #'(lambda (stream) (read-constant stream "true" t)))

(add-json-dispatch-rule #\f #'(lambda (stream) (read-constant stream "false" nil)))

(add-json-dispatch-rule #\n #'(lambda (stream) (read-constant stream "null" nil)))

(defun read-constant (stream expected-string ret-value)
  (loop for x across expected-string
        for ch = (read-char stream nil nil)
        always (char= ch x)
        finally (return ret-value)))

(defun read-json-string (stream)
  (read-char stream)
  (let ((val (read-json-chars stream '(#\"))))
    (read-char stream)
    val))

(add-json-dispatch-rule #\" #'read-json-string)

(defun read-json-object (stream)
  (read-char stream)
  (let ((obj (funcall *json-object-factory*)))
    (if (char= #\} (peek-char t stream))
        (read-char stream)
        (loop for skip-whitepace = (peek-char t stream)
              for key = (read-json-string stream)
              for separator = (peek-char t stream)
              for skip-separator = (assert (char= #\: (read-char stream)))
              for value = (decode-json stream)
              for terminator = (peek-char t stream)
              for skip-terminator = (assert (member (read-char stream) '(#\, #\})))
              do (setf obj (funcall *json-object-factory-add-key-value* obj key value))
              until (char= #\} terminator)))
    (funcall *json-object-factory-return* obj)))

(add-json-dispatch-rule #\{ #'read-json-object)

(defun read-json-array (stream)
  (read-char stream)
  (if (char= #\] (peek-char t stream))
      (progn (read-char stream) nil)
      (loop for first-in-element = (assert (not (member (peek-char t stream) '(#\, #\]))))
            for element = (decode-json stream)
            for terminator = (peek-char t stream)
            for skip-terminator = (assert (member (read-char stream) '(#\, #\])))
            collect element        
            until (char= #\] terminator))))

(add-json-dispatch-rule #\[ #'read-json-array)

(defparameter *digits* '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
(defparameter *json-number-valid-chars* (concatenate 'list *digits* '(#\e #\E #\. #\+ #\-)))

(defun read-json-number (stream)
  (let ((number-string (read-chars-until stream
                                         :terminator-fn #'(lambda (ch)
                                                            (not (member ch *json-number-valid-chars*))))))
    (assert (if (char= (char number-string 0) #\0)
                (or (= 1 (length number-string)) (char= #\. (char number-string 1)))
                t))
    (handler-case 
        (read-from-string number-string)
      (serious-condition (e)
        (let ((e-pos (or (position #\e number-string)
                         (position #\E number-string))))
          (if e-pos
              (handler-case
                  (read-from-string (substitute #\l (aref number-string e-pos) number-string))
                (serious-condition ()
                  (funcall *json-make-big-number* number-string)))
              (error "Unexpected error ~S" e)))))))
    
(defun read-chars-until(stream &key terminator-fn (char-converter #'(lambda (ch stream)
                                                                       (declare (ignore stream))
                                                                       ch)))
  (with-output-to-string (ostr)
    (loop
     (let ((ch (peek-char nil stream nil nil)))
       (when (or (null ch)
                 (funcall terminator-fn ch))
         (return))
       (write-char (funcall char-converter
                            (read-char stream nil nil)
                            stream)
                   ostr)))))
       
(defun read-n-chars (stream n)
  (with-output-to-string (ostr)
    (dotimes (x n)
      (write-char (read-char stream) ostr))))
  
(defun read-json-chars(stream terminators)
  (read-chars-until stream :terminator-fn #'(lambda (ch)
                                              (member ch terminators))
                    :char-converter #'(lambda (ch stream)
                                        (if (char= ch #\\)
                                            (if (char= #\u (peek-char nil stream))
                                                (code-char (parse-integer (read-n-chars stream 5) :start 1 :radix 16))
                                                (json-escaped-char-to-lisp (read-char stream)))
                                            ch))))