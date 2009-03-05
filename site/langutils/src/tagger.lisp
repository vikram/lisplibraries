;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: utils -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          tagger
;;;; Purpose:       Lisp version of the Brill tagger w/ WSJ/BROWN data
;;;;
;;;; Programmer:    Ian S. Eslick
;;;; Date Started:  October 2004
;;;;

(in-package :langutils)

;; ==========================================
;; TAGGER STATE

;; Variables to hold the datafiles
(defvar *tagger-lexical-rules* nil
  "Table to hold the lexical rule closures")
(defvar *tagger-contextual-rules* nil
  "Table to hold the contextual rule closures")
(defvar *tagger-bigrams* nil
  "Bigram hash (not implemented yet)")
(defvar *tagger-wordlist* nil
  "Wordlist hash (not implemented yet)")

;; ==========================================
;; STRING TO STRING TAGGING 
;; Speed: moderately fast
;; Size: Designed for strings on order of Dcache size
 
(defun tag ( string )
  (with-output-to-string (stream)
    (print-vector-document (vector-tag string) :stream stream)))

(defun tag-tokenized ( string )
  (with-output-to-string (stream)
    (print-vector-document (vector-tag-tokenized string) :stream stream)))

;; ===============================
;; FILE TAGGING - slow, small files
;; Speed: slow
;; Input size: Works on small files

(defun-exported read-file-as-tagged-document (file)
  (vector-tag (read-file-to-string file)))

(defun-exported read-and-tag-file (file)
  (tag (read-file-to-string file)))

;; =================================
;; STRING TO VECTOR-DOCUMENT TAGGING
;; Speed: optimal
;; Input size: < 100k strings

(defun vector-tag ( string )
  "Returns a 'document' which is a class containing a pair of vectors 
   representing the string in the internal token format.  Handles arbitrary data."
  (vector-tag-tokenized (mvbind (succ consumed data remainder) (tokenize-string string)
				(declare (ignore succ consumed remainder))
				data)))

(let ((temp-tokens (make-array 100000 :element-type 'fixnum :initial-element 0))
      (temp-tags (make-array 100000 :initial-element :NN)))
  (declare (type (simple-array fixnum) temp-tokens )
	   (type (simple-array symbol) temp-tags))
  (defun vector-tag-tokenized ( string &key (end-tokens nil))
    "Returns a document representing the string using the
     internal token dictionary; requires the string to be tokenized.
     Parses the string into tokens (whitespace separators) then populates 
     the two temp arrays above with token id's and initial tags.  Contextual
     rules are applied and a new vector document is produced which
     is a copy of the enclosed data.  This is all done at once so good
     compilers can open-code the array refs and simplify the calling 
     of the labels functions.
    "
    (declare (optimize speed (safety 0)))
    (let* ((stoks (append (extract-words string) end-tokens))
	   (elements (length stoks)))
      (assert (< elements 100000))
      (labels ((write-temp ( token tag pos )
			   (declare (type fixnum token)
				    (type symbol tag)
				    (type fixnum pos)
				    (inline aref)
				    (optimize speed (safety 0)))
			   (setf (aref temp-tokens pos) token)
			   (setf (aref temp-tags pos) tag))
	       (duplicate-from ( source start end )
			       (declare (inline aref)
					(type fixnum source start end)
					(optimize speed (safety 0)))
			       (loop for pos fixnum from start to (1- end) do
				 (setf (aref temp-tokens pos) (aref temp-tokens source))
				 (setf (aref temp-tags pos) (aref temp-tags source))))
	       (initial-tag-1 ( token pos )
			   (declare (type string token)
				    (optimize speed (safety 0))
				    (inline write-temp))
			   (mvbind (tokid tagid) (initial-tag token)
				   (write-temp tokid tagid pos)))
	       (initial-tag-all ()
			   (declare (inline populate-1)
				    (optimize speed (safety 0)))
			   (loop for token fixnum in stoks and
			     count fixnum from 0 and
			     pos fixnum from 3 do
			     (initial-tag-1 token pos)
			     (cond ((= count 0)
				    (duplicate-from 3 0 3))
				   ((= count (1- elements))
				    (duplicate-from (+ 3 elements) (+ 3 elements) (+ 6 elements)))))))
	;; Setup arrays and populate initial tags
	(initial-tag-all)
	;; Run contextual fixup rules
	(loop for pos fixnum from 3 upto (+ 3 elements) do
	  (loop for rule fixnum in *tagger-contextual-rules* do
	    (funcall rule temp-tokens temp-tags pos)))
	;; Fresh copy of the resulting data
	(make-instance 'vector-document 
		       :text (subseq temp-tokens 3 (+ elements 3))
		       :tags (subseq temp-tags 3 (+ elements 3)))))))

(defun initial-tag ( token )
  "Return an initial tag for a given token string using the langutils 
   lexicon and the tagger lexical rules (via guess-tag)"
  (aif (hash-get *lexicon* (id-for-token token))
       (values (lexicon-entry-id it) (lexicon-entry-tag it)) ;; token id, best tag
       (values 
	(id-for-token token) ;; register if new
	(guess-tag token (default-tag token) *tagger-lexical-rules*))))

(defun default-tag ( token )
  "Simple default tagging based on capitalization of token string"
  (if (and (> (char-code (char token 0)) (char-code #\A))
	   (< (char-code (char token 0)) (char-code #\Z)))
      :NNP
    :NN))

;; ==========================================
;; TAGGER INITIALIZATION

(defun-exported init-tagger (&optional lexical-rule-file contextual-rule-file)
  (write-log tagger-init "Initializing the tagger")
  ;; Handle vector tags and tokens
  (unless (and *tagger-lexical-rules* *tagger-contextual-rules*)
    ;; Load the files
    (load-tagger-files (aif lexical-rule-file it 
			    (translate-logical-pathname "think:data;lang;en;langutils;LEXICALRULEFILE-BROWN.txt"))
		       (aif contextual-rule-file it 
			    (translate-logical-pathname "think:data;lang;en;langutils;CONTEXTUALRULEFILE-BROWN.txt")))))

(defun load-tagger-files ( lexical-rules contextual-rules &key bigrams wordlist )
  (declare (ignore bigrams wordlist))

  ;; Load lexical rules
  (write-log langutils-tagger "Loading lexical rules.")
  (setf *tagger-lexical-rules* (load-lexical-rules lexical-rules *tagger-bigrams* *tagger-wordlist*))

  ;; Load contextual rules
  (write-log langutils-tagger "Loading contextual rules.")
  (setf *tagger-contextual-rules* (load-contextual-rules contextual-rules))
  nil)

(defun load-contextual-rules ( rule-file &aux rules )
  (write-log tagger-init "Loading contextual rules")
  (with-open-file ( s rule-file )
    (do-contentful-lines (line s ) ;; :count count)
      (let* ((context-rule (cl-ppcre:split "\\s+" line))
	     (closure (make-contextual-rule context-rule)))
	(if closure (push closure rules)))))
  (nreverse rules))

(defun load-lexical-rules ( rule-file &optional bigram-hash word-hash &aux (rule-list nil))
  "Return a list of closure implementing the lexical rules
   in rule-file to tag words not found in the lexicon"
  (write-log tagger-init "Loading lexical rules")
  (with-open-file ( s rule-file )
    (do-contentful-lines (line s)
      (let ((lex-rule (cl-ppcre:split "\\s+" line)))
	(let ((rule (make-lexical-rule lex-rule *lexicon* bigram-hash word-hash)))
	  (if rule (push rule rule-list))))))
  (nreverse rule-list))

(defun-exported clean-tagger ()
  (clean-lexicon)
  (setf *tagger-lexical-rules* nil)
  (setf *tagger-contextual-rules* nil)
  (setf *tagger-bigrams* nil)
  (setf *tagger-wordlist* nil))



