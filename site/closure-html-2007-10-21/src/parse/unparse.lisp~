;;; -*- show-trailing-whitespace: t; indent-tabs: nil -*-
;;; ---------------------------------------------------------------------------
;;;     Title: Unparse HTML
;;;   Created: 2007-10-14
;;;    Author: David Lichteblau <david@lichteblau.com>
;;;   License: BSD-style
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 2005-2007 David Lichteblau


(in-package :closure-html)


;;; SINK: an HTML output sink

(defclass sink (hax:abstract-handler)
    ((ystream :initarg :ystream :accessor sink-ystream)
     (stack :initform nil :accessor stack)))

#-rune-is-character
(defmethod hax:%want-strings-p ((handler sink))
  nil)

;; bisschen unschoen hier SCHON WIEDER die ganze api zu duplizieren, aber die
;; ystreams sind noch undokumentiert
(macrolet ((define-maker (make-sink make-ystream &rest args)
	     `(defun ,make-sink (,@args &rest initargs)
		(apply #'make-instance
		       'sink
		       :ystream (,make-ystream ,@args)
		       initargs))))
  (define-maker make-octet-vector-sink make-octet-vector-ystream)
  (define-maker make-octet-stream-sink make-octet-stream-ystream stream)
  (define-maker make-rod-sink make-rod-ystream)

  #+rune-is-character
  (define-maker make-character-stream-sink make-character-stream-ystream stream)

  #-rune-is-character
  (define-maker make-string-sink/utf8 make-string-ystream/utf8)

  #-rune-is-character
  (define-maker make-character-stream-sink/utf8
      make-character-stream-ystream/utf8
    stream))

#+rune-is-character
(defun make-string-sink (&rest args) (apply #'make-rod-sink args))


;;;; Events

(defmethod hax:start-document ((sink sink) name public-id system-id)
  (when (plusp (length system-id))
    (%write-rod #"<!DOCTYPE " sink)
    (%write-rod name sink)
    (cond
      ((plusp (length public-id))
       (%write-rod #" PUBLIC \"" sink)
       (unparse-string public-id sink)
       (%write-rod #"\" \"" sink)
       (unparse-string system-id sink)
       (%write-rod #"\"" sink))
      (t
       (%write-rod #" SYSTEM \"" sink)
       (unparse-string system-id sink)
       (%write-rod #"\"" sink)))
    (%write-rod #">" sink)
    (%write-rune #/U+000A sink)))

(defmethod hax:end-document ((sink sink))
  (close-ystream (sink-ystream sink)))

(defmethod hax:start-element ((sink sink) name attributes)
  (let* ((key (find-symbol (string-upcase (rod-string name)) :keyword))
	 (elt (and key (sgml::find-element closure-html::*html-dtd* key)))
	 (attlist (sgml::element-attlist elt)))
    (push (cons name elt) (stack sink))
    (%write-rune #/< sink)
    (%write-rod name sink)
    (dolist (a attributes)
      (let* ((aname (hax:attribute-name a))
	     (akey (find-symbol (string-upcase (string-rod aname)) :keyword))
	     (att (and akey (assoc akey attlist)))
	     (values (second att)))
	(%write-rune #/space sink)
	(%write-rod aname sink)
	(unless (and att (listp values) (eq (car att) (car values)))
	  (%write-rune #/= sink)
	  (%write-rune #/\" sink)
	  (unparse-string (hax:attribute-value a) sink)
	  (%write-rune #/\" sink))))
    (%write-rune #/> sink)))

(defmethod hax:end-element
    ((sink sink) name)
  (let* ((prev (pop (stack sink)))
	 (prev-name (car prev))
	 (elt (cdr prev)))
    (unless (rod= prev-name name)
      (error "output does not nest: expected ~A but got ~A"
             name prev-name))
    (unless (and elt (null (sgml::element-include elt)))
      (%write-rod '#.(string-rod "</") sink)
      (%write-rod name sink)
      (%write-rod '#.(string-rod ">") sink))))

(defmethod hax:characters ((sink sink) data)
  (let ((y (sink-ystream sink)))
    (loop for c across data do (unparse-datachar-readable c y))))

(defmethod hax:comment ((sink sink) data)
  ;; XXX signal error if body is unprintable?
  (%write-rod #"<!--" sink)
  (map nil (lambda (c) (%write-rune c sink)) data)
  (%write-rod #"-->" sink))

(defun unparse-string (str sink)
  (let ((y (sink-ystream sink)))
    (loop for rune across str do (unparse-datachar rune y))))

(defun unparse-datachar (c ystream)
  (cond ((rune= c #/&) (write-rod '#.(string-rod "&amp;") ystream))
        ((rune= c #/<) (write-rod '#.(string-rod "&lt;") ystream))
        ((rune= c #/>) (write-rod '#.(string-rod "&gt;") ystream))
        ((rune= c #/\") (write-rod '#.(string-rod "&quot;") ystream))
        ((rune= c #/U+0009) (write-rod '#.(string-rod "&#9;") ystream))
        ((rune= c #/U+000A) (write-rod '#.(string-rod "&#10;") ystream))
        ((rune= c #/U+000D) (write-rod '#.(string-rod "&#13;") ystream))
        (t
         (write-rune c ystream))))

(defun unparse-datachar-readable (c ystream)
  (cond ((rune= c #/&) (write-rod '#.(string-rod "&amp;") ystream))
        ((rune= c #/<) (write-rod '#.(string-rod "&lt;") ystream))
        ((rune= c #/>) (write-rod '#.(string-rod "&gt;") ystream))
        ((rune= c #/\") (write-rod '#.(string-rod "&quot;") ystream))
        ((rune= c #/U+000D) (write-rod '#.(string-rod "&#13;") ystream))
        (t
          (write-rune c ystream))))

(defun unparse-dtd-string (str sink)
  (let ((y (sink-ystream sink)))
    (loop for rune across str do (unparse-dtd-char rune y))))

(defun unparse-dtd-char (c ystream)
  (cond ((rune= c #/%) (write-rod '#.(string-rod "&#37;") ystream))
        ((rune= c #/&) (write-rod '#.(string-rod "&amp;") ystream))
        ((rune= c #/<) (write-rod '#.(string-rod "&lt;") ystream))
        ((rune= c #/>) (write-rod '#.(string-rod "&gt;") ystream))
        ((rune= c #/\") (write-rod '#.(string-rod "&quot;") ystream))
        ((rune= c #/U+0009) (write-rod '#.(string-rod "&#9;") ystream))
        ((rune= c #/U+000A) (write-rod '#.(string-rod "&#10;") ystream))
        ((rune= c #/U+000D) (write-rod '#.(string-rod "&#13;") ystream))
        (t
         (write-rune c ystream))))

(defun %write-rune (c sink)
  (write-rune c (sink-ystream sink)))

(defun %write-rod (r sink)
  (write-rod r (sink-ystream sink)))


;;;; convenience functions for PTless HTML serialization

(defvar *current-element*)
(defvar *sink*)

(defmacro with-html-output
    ((sink &key (name "HTML") public-id system-id) &body body)
  `(invoke-with-html-output (lambda () ,@body)
			    ,sink
			    ,name
			    ,public-id
			    ,system-id))

(defun invoke-with-html-output (fn sink name pubid sysid)
  (let ((*sink* sink)
        (*current-element* nil))
    (hax:start-document *sink* name pubid sysid)
    (funcall fn)
    (hax:end-document *sink*)))

;; fuer XML ist hier mehr zu tun, also gehen wir vorsichtshalber fuer HTML
;; erstmal auch diesen Weg
(defmacro with-output-sink ((var) &body body)
  `(invoke-with-output-sink (lambda (,var) ,@body)))
(defun invoke-with-output-sink (fn)
  (funcall fn *sink*))

(defmacro with-element (name &body body)
  `(invoke-with-element (lambda () ,@body) ,name))

(defun maybe-emit-start-tag ()
  (when *current-element*
    ;; starting child node, need to emit opening tag of parent first:
    (destructuring-bind (name &rest attributes) *current-element*
      (hax:start-element *sink* name (reverse attributes)))
    (setf *current-element* nil)))

(defun invoke-with-element (fn name)
  (setf name (rod name))
  (maybe-emit-start-tag)
  (let ((*current-element* (list name)))
    (multiple-value-prog1
        (funcall fn)
      (maybe-emit-start-tag)
      (hax:end-element *sink* name))))

(defgeneric unparse-attribute (value))
(defmethod unparse-attribute ((value string)) value)
(defmethod unparse-attribute ((value null)) nil)
(defmethod unparse-attribute ((value integer)) (write-to-string value))

(defun attribute (name value)
  (setf name (rod name))
  (setf value (unparse-attribute value))
  (push (hax:make-attribute name value t)
	(cdr *current-element*)))

(defun text (data)
  (maybe-emit-start-tag)
  (hax:characters *sink* (rod data))
  data)

(defun comment (data)
  (maybe-emit-start-tag)
  (hax:comment *sink* (rod data))
  data)
