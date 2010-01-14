;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: utils -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          tokens
;;;; Purpose:       Abstractions for word tokens, provides for unique textual token identity
;;;;                will include all 'words' including abbreviations, misspellings, etc.
;;;;
;;;; Programmer:    Ian S. Eslick
;;;; Date Started:  October 2004


(in-package :langutils)

;; =============================
;; TOKEN REPRESENTATION
;;
;; Create a counter to track all novel terms, including
;; capitalizations.

(defvar *token-counter* 0)
(defvar *token-table* nil)
(defvar *token-dirty-bit* nil)
(defvar *tokens-load-file* nil)
(defvar *id-table* nil)

(defun-exported get-token-count ()
  "Return the current token counter"
  *token-counter*)

(defun ensure-token-counts ()
  "Reset token count if not already set"
  (when (null *token-table*)
    (reset-token-counts)))

(defun save-tokens ()
  "Save the current token hash to disk if and only if
   it has changed since it was loaded."
  (when (and *token-dirty-bit* *tokens-load-file*)
    (save-token-map *tokens-load-file*)))

(defun reset-token-counts ()
  "Reset all the token datastructures to an initialized but empty state"
  (write-log lexicon-init "Resetting token counts")
  (save-tokens)
  (setf *token-counter* 0)
  (setf *token-table* (make-hash-table :size 200000 :rehash-size 1.2 :rehash-threshold 0.5))
  (setf *id-table* (make-hash-table :test #'equal :size 100000 :rehash-size 1.3 :rehash-threshold 0.8)))

;; API

(defconstant *whitespace-chars* '(#\space #\tab #\return #\linefeed
				  #+allegro #\%space
				  #+lispworks #\No-Break-Space))

(defun id-for-token ( token )
  "This takes string 'tokens' and 
   returns a unique id for that character
   sequence - beware of whitespace, etc."
  (declare (type hash-table *id-table* *token-table* *suspicious-words*)
	   (optimize speed (safety 0)))
  (if (null token) 
      0
    (etypecase token
      (string 
       (let ((basic-token (string-trim *whitespace-chars* token)))
	 (aif (hash-get *id-table* basic-token)
	      it
	      (let ((id (incf *token-counter*)))
		(if (suspicious-string? basic-token)
		    (hash-put *suspicious-words* id t))
		(hash-put *token-table* id basic-token)
		(hash-put *id-table* basic-token id)
		(setf *token-dirty-bit* t)
		id))))
      (number
       token))))

(defun token-for-id ( id )
  "Return a string token for a given token id"
  (error-on-null
   (hash-get *token-table* id)
   "Unknown token for id: ~A." id))

(defun tokens-for-ids ( ids )
  "Return a list of string tokens for each id in ids"
  (mapcar #'token-for-id ids))

;; Loading map from file

(defparameter *default-token-map-file-int* 
  (translate-logical-pathname "think:data;lang;en;langutils;initial-token-map.sexp"))

(defun save-token-map (filename)
  "This uses the serialize-sexp library to save the token 
   database hashes and counter to a disk file"
  (write-log lexicon-init "Saving token map to: ~A" filename)
  (let ((save-to
	 (cond ((not (null filename)) filename)
	       (t *default-token-map-file-int*))))
    (with-open-file (s save-to :direction :output :if-exists :supersede)
      (serialize-sexp 
       (list 
	*token-counter*
	*token-table*
	*id-table*)
       s))
    t))

(defun load-token-map (filename)
  "Load an s-serialized version of the token map data 
   structures from a file"
  (write-log lexicon-init "Loading and initializing token map from: ~A" filename)
  (cond ((and (null filename) (null *default-token-map-file-int*))
	 (reset-token-counts))
	((null filename)
	 (if (probe-file *default-token-map-file-int*)
	     (progn (setf *tokens-load-file* filename)
		    (load-token-map *default-token-map-file-int*))
	   (reset-token-counts)))
	(t (with-open-file (s filename )
	     (setf *tokens-load-file* filename)
	     (let ((list (deserialize-sexp s)))
	       (setf *token-counter* (first list))
	       (setf *token-table* (second list))
	       (setf *id-table* (third list))))
	   t)))
	     

;; ========================================
;; FISHY TOKENS

(defconstant *max-token-nums* 7
  "The maximum number of numbers allowed in a valid token")
(defconstant *max-token-others* 1
  "The maximum number of non alpha-numeric characters in a valid token")
(defvar *suspicious-words* (make-hash-table :size 10000 :rehash-size 1.5 :rehash-threshold 0.7)
  "Memoize known suspicious words that have been tokenized in this hash")

(defmethod suspicious-word? ((word fixnum))
  "Find a suspicious word using it's token id"
  (hash-get *suspicious-words* word))

(defun suspicious-string? (string)
  "Determine if the alpha-num and number balance is reasonable
   for lingustic processing or if non-alpha-nums are present"
  (let ((nums 0)
	(others 0))
    (declare (type fixnum nums others it)
	     (inline alpha-char-p digit-char-p)
	     (optimize speed (safety 1)))
    (loop for it across string do
      (cond ((alpha-char-p it) nil)
	    ((digit-char-p it) (incf nums))
	    (t (incf others))))
    (or (> nums *max-token-nums*)
	(> others *max-token-others*))))
  
