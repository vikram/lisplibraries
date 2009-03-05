;;; ------------------------------------------------- -*- Mode: LISP -*-
;;; CL-HTML-DIFF -- A Lisp library for generating human readable diffs
;;; of HTML documents, using HTML.
;;;
;;; Copyright 2005 
;;; John Wiseman (jjwiseman@yahoo.com)
;;; $Id: html-diff.lisp,v 1.3 2005/02/24 20:27:53 wiseman Exp $
;;;
;;; Licensed under the MIT license--see the accompanying LICENSE.txt
;;; file.
;;;
;;; This code is based on Aaron Swartz' HTML Diff for python,
;;; <http://www.aaronsw.com/2002/diff/>.

(cl:defpackage "HTML-DIFF"
  (:use "COMMON-LISP" "DIFFLIB")
  (:export #:html-diff
	   #:html-diff-to-stream))

(in-package "HTML-DIFF")


(defun html-diff (a b &key (insert-class "diff") (delete-class "diff") (replace-class "diff"))
  (with-output-to-string (out)
    (html-diff-to-stream out a b
			 :insert-class insert-class
			 :delete-class delete-class
			 :replace-class replace-class)))


(defun html-diff-to-stream (stream a b &key (insert-class "diff") (delete-class "diff") (replace-class "diff"))
  "Takes two strings containing HTML and returns a human-readable
  HTML diff.  Uses <ins>, <del> and some css classes"
  (let ((a (tokenize-html a))
	(b (tokenize-html b)))
    (dolist (op (html-get-opcodes a b))
      (ecase (opcode-tag op)
	((:replace)
	 (format stream "<del class=\"~A\">" replace-class)
	 (join-to-stream stream (subseq a (opcode-i1 op) (opcode-i2 op)))
	 (format stream "</del><ins class=\"~A\">" replace-class)
	 (join-to-stream stream (subseq b (opcode-j1 op) (opcode-j2 op)))
	 (format stream "</ins>"))
	((:delete)
	 (format stream "<del class=\"~A\">" delete-class)
	 (join-to-stream stream (subseq a (opcode-i1 op) (opcode-i2 op)))
	 (format stream "</del>"))
	((:insert)
	 (format stream "<ins class=\"~A\">" insert-class)
	 (join-to-stream stream (subseq b (opcode-j1 op) (opcode-j2 op)))
	 (format stream "</ins>"))
	((:equal)
	 (join-to-stream stream (subseq b (opcode-j1 op) (opcode-j2 op))))))))


(defun join (seq)
  "Concatenates HTML tokens back into a single string."
  (with-output-to-string (out)
    (join-to-stream out seq)))


(defun join-to-stream (stream seq)
  "Concatenates HTML tokens back into a single string."
  (map nil
       #'(lambda (e) (format stream "~A" e))
       seq))


(defun tokenize-html (x)
  (let ((mode :char)
	(cur '())
	(out '()))
    (labels ((get-current ()
	       (coerce (reverse cur) 'string))
	     (save-current ()
	       (when cur
		 (push (get-current) out))))
      (dotimes (i (length x))
	(let ((c (char x i)))
	  (cond ((eq mode :tag)
		 (if (eql c #\>)
		     (progn
		       (push c cur)
		       (save-current)
		       (setf cur '())
		       (setf mode :char))
		     (push c cur)))
		((eq mode :char)
		 (if (eql c #\<)
		     (progn
		       (save-current)
		       (setf cur (list c))
		       (setf mode :tag))
		     (if (whitespace-p c)
			 (progn
			   (push c cur)
			   (save-current)
			   (setf cur '()))
			 (push c cur)))))))
      (save-current)
      (coerce (reverse out) 'vector))))


(defun tag-p (x)
  "Returns true if a string has the format of an HTML/XML open tag
(<p>, <applet>, <woo>, etc.)"
  (let ((length (length x)))
    (and (>= length 2)
	 (eql (elt x 0) #\<)
	 (eql (elt x (- length 1)) #\>))))


(defun whitespace-p (char)
  (member char '(#\space
		 #.(code-char 9)  ;; horizontal tab
		 #.(code-char 10) ;; linefeed
		 #.(code-char 11) ;; vertical tab
		 #.(code-char 12) ;; form feed
		 #.(code-char 13) ;; carriage return
		 )))


(defun html-get-opcodes (a b)
  (let ((m (make-instance 'sequence-matcher
			  :a a
			  :b b
			  :test-function #'equal)))
    (get-opcodes m)))


#|
;; Maybe later...
(defparameter *inline-tags*
  '("a" "abbr" "acronym" "b" "basefont" "bdo" "big" "br" "cite"
    "code" "dfn" "em" "font" "i" "img" "input" "kbd" "label" "q"
    "s" "samp" "select" "small" "span" "strike" "strong" "sub"
    "sup" "textarea" "tt" "u" "var"
    
    "applet" "button" "del" "iframe" "ins" "map" "object" "script"))

(defun inline-tag-p (elt)
  (and (tag-p elt) (member (subseq elt 1 (- (length elt) 1))
			   *inline-tags*
			   :test #'string-equal)))

|#

;; This is not yet ready for prime time.

(defun html-diff-2-col (a b &key (insert-class "diff") (delete-class "diff") (replace-class "diff"))
  (with-output-to-string (out)
    (with-open-stream (a-out (make-string-output-stream))
      (with-open-stream (b-out (make-string-output-stream))
	(let ((a (tokenize-html a))
	      (b (tokenize-html b)))
	  (dolist (op (html-get-opcodes a b))
	    (ecase (opcode-tag op)
	      ((:replace)
	       (format a-out "<del class=\"~A\">" replace-class)
	       (format a-out "~A" (join (subseq a (opcode-i1 op) (opcode-i2 op))))
	       (format a-out "</del>")
	       (format b-out "<ins class=\"~A\">" replace-class)
	       (format b-out "~A" (join (subseq b (opcode-j1 op) (opcode-j2 op))))
	       (format b-out "</ins>"))
	      ((:delete)
	       (format a-out "<del class=\"~A\">" delete-class)
	       (format a-out "~A" (join (subseq a (opcode-i1 op) (opcode-i2 op))))
	       (format a-out "</del>"))
	      ((:insert)
	       (format b-out "<ins class=\"~A\">" insert-class)
	       (format b-out "~A" (join (subseq b (opcode-j1 op) (opcode-j2 op))))
	       (format b-out "</ins>"))
	      ((:equal)
	       (format a-out "~A" (join (subseq a (opcode-i1 op) (opcode-i2 op))))
	       (format b-out "~A" (join (subseq b (opcode-j1 op) (opcode-j2 op)))))))
	  (format out "<table><tr>")
	  (format out "<td valign=\"top\">~A</td>" (get-output-stream-string b-out))
	  (format out "<td valign=\"top\">~A</td>" (get-output-stream-string a-out))
	  (format out "</tr></table>"))))))


