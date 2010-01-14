;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: utils -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          tagger-data
;;;; Purpose:       Lisp version of the Brill tagger w/ WSJ/BROWN data; implements
;;;;                the data file loading and representation
;;;;
;;;; Programmer:    Ian S. Eslick
;;;; Date Started:  October 2004
;;;;

(in-package :langutils)

;; -----------------------------------
;; Define a logging heirarchy
(deflog tagger-contextual (tagger))
(deflog tagger-lexical (tagger))

;; -----------------------------------
;; Contextual rule files

;;
;; The parser builds rules from tokenized rule expressions of the form:
;; <oldtag> <newtag> RULE-TYPE <arg1> <arg2>
;;
;; Where <arg1> can be tag1 or word1 with semantics as described
;; in *contextual-rule-args* above; similarly for <arg2>
;;
(def-contextual-rule-parser make-contextual-rule
  ("SURROUNDTAG" (match (0 oldtag) (-1 tag1) (+1 tag2)) => newtag)
  ("NEXTTAG" (match (0 oldtag) (+1 tag1)) => newtag)
  ("CURWD" (match (0 oldtag) (0 word1)) => newtag)
  ("NEXTWD" (match (0 oldtag) (+1 word1)) => newtag)
  ("RBIGRAM" (match (0 oldtag) (0 word1) (+1 word2)) => newtag)
  ("WDNEXTTAG" (match (0 oldtag) (0 word1) (+1 tag2)) => newtag)
  ("WDAND2AFT" (match (0 oldtag) (0 word1) (+2 word2)) => newtag)
  ("WDAND2TAGAFT" (match (0 oldtag) (0 word1) (+2 tag2)) => newtag)
  ("NEXT2TAG" (match (0 oldtag) (+2 tag1)) => newtag)
  ("NEXT2WD" (match (0 oldtag) (+2 word1)) => newtag)
  ("NEXTBIGRAM" (match (0 oldtag) (+1 tag1) (+2 tag2)) => newtag)
  ("NEXT1OR2TAG" (match (0 oldtag) (or (+1 tag1) (+2 tag1))) => newtag)
  ("NEXT1OR2WD" (match (0 oldtag) (or (+1 word1) (+2 word1))) => newtag)
  ("NEXT1OR2OR3TAG" (match (0 oldtag) (or (+1 tag1) (+2 tag1) (+3 tag1))) => newtag)
  ("PREVTAG" (match (0 oldtag) (-1 tag1)) => newtag)
  ("PREVWD" (match (0 oldtag) (-1 word1)) => newtag)
  ("LBIGRAM" (match (0 oldtag) (0 word1) (-1 word2)) => newtag)
  ("WDPREVTAG" (match (0 oldtag) (0 word1) (-1 tag2)) => newtag)
  ("WDAND2BFR" (match (0 oldtag) (0 word1) (-2 word2)) => newtag)
  ("WDAND2TAGBFR" (match (0 oldtag) (0 word1) (-2 tag2)) => newtag)
  ("PREV2TAG" (match (0 oldtag) (-2 tag1)) => newtag)
  ("PREV2WD" (match (0 oldtag) (-2 word1)) => newtag)
  ("PREV1OR2TAG" (match (0 oldtag) (or (-1 tag1) (-2 tag1))) => newtag)
  ("PREV1OR2WD" (match (0 oldtag) (or (-1 word1) (-2 word1))) => newtag)
  ("PREV1OR2OR3TAG" (match (0 oldtag) (or (-1 tag1) (-2 tag1) (-3 tag1))) => newtag)
  ("PREV1OR2OR3WD" (match (0 oldtag) (or (-1 word1) (-2 word1) (-3 word1))) => newtag)
  ("PREVBIGRAM" (match (0 oldtag) (-1 tag2) (-2 tag1)) => newtag))

;; -----------------------------------------
;; LEXICAL RULE FILE INITIALIZATION AND I/F

(let ((pair (cons nil nil)))
  (defun guess-tag ( token initial-tag rule-list )
    "Using rules in rule-table guess the tag of the token 'token'"
    (setf (car pair) token)
    (setf (cdr pair) initial-tag)
    (aif (apply-rules pair rule-list)
	 (cdr it)
	 initial-tag)))

;; Expanded implementation, not great but was fast and easy...
;;
;; Example of lexical rule file format:
;;   NN would fgoodright VB x
;;   NN 0 fchar CD x
;;   NN be fgoodright JJ x
(defun make-lexical-rule ( list lh bh wh )
  "Look through list for rule name"
  (declare (ignore bh)
;;	   (inline strncmp list eq equal char subseq strncmp-end2 length concatenate)
	   (optimize speed (safety 0)))
  (let ((name (second list))
	(fname (third list)))
    (cond ((string= name "char")
	   (let ((prefix (char (first list) 0)) 
		 (new-tag (mkkeysym (third list))))
	     (declare (type character prefix)
		      (type symbol new-tag)
		      (symbol pair))
	     #'(lambda (pair)
		 (declare (type cons pair)
			  (optimize speed (safety 0))
			  (inline schar))
		 (let ((token (car pair))
		       (tag (cdr pair)))
		   (declare (type string token) (type symbol tag)
			    (ignore tag))
		   (if (eq prefix (schar token 0))
		       (progn (setf (cdr pair) new-tag) pair)
		     pair)))))
	  ((string= fname "fchar")
	   (let ((old-tag (mkkeysym (first list))) 
		 (prefix (char (second list) 0)) 
		 (new-tag (mkkeysym (fourth list))))
	     (declare (type symbol old-tag new-tag)
		      (type character prefix))
	     #'(lambda (pair)
		 (declare (type cons pair)
			  (optimize speed (safety 0))
			  (inline schar))
		 (let ((token (car pair))
		       (tag (cdr pair)))
		   (declare (type string token) (type symbol tag))
		   (if (and (eq tag old-tag)
			    (eq prefix (schar token 0)))
		       (progn (setf (cdr pair) new-tag) pair)
		     pair)))))
	  ((string= name "deletepref")
	   (let ((prefix (first list))
		 (count (read-from-string (third list)))
		 (new-tag (mkkeysym (fourth list))))
	     (declare  #-mcl (type symbol new-tag)
		       #-mcl (type fixnum count)
		       #-mcl (type string prefix))
	     #'(lambda (pair)
		 (declare #-mcl (type cons pair)
			  (optimize speed (safety 0)))
		 (let ((token (car pair))
		       (tag (cdr pair)))
		   (declare (type string token) (type symbol tag)
			    (ignore tag))
		   (if (strncmp prefix token count 0)
		       (let ((root (subseq token count)))
		       (if (or (hash-get lh root)
			       (and wh (hash-get wh root)))
			   (progn (setf (cdr pair) new-tag) pair)
			 pair))
		   pair)))))
	  ((string= fname "fdeletepref")
	   (let ((old-tag (mkkeysym (first list)))
		 (prefix (second list))
		 (count (read-from-string (fourth list)))
		 (new-tag (mkkeysym (fifth list))))
	     (declare  #-mcl (type symbol old-tag new-tag)
		       #-mcl (type fixnum count)
		       #-mcl (type string prefix))
	     #'(lambda (pair)
		 (declare  #-mcl (type cons pair)
			   (optimize speed (safety 0)))
		 (let ((token (car pair))
		       (tag (cdr pair)))
		   (declare (type string token) (type symbol tag))
		   (if (and (eq tag old-tag)
			    (strncmp prefix token count 0))
		     (let ((root (subseq token count)))
		       (if (or (hash-get lh root)
			       (and wh (hash-get wh root)))
			   (progn (setf (cdr pair) new-tag) pair)
			 pair))
		   pair)))))
	  ((string= name "haspref")
	   (let ((prefix (first list))
		 (count (read-from-string (third list)))
		 (new-tag (mkkeysym (fourth list))))
	     (declare  #-mcl (type symbol new-tag)
		       #-mcl (type fixnum count)
		       #-mcl (type string prefix))
	     #'(lambda (pair)
		 (declare #-mcl (type cons pair)
			  (optimize speed (safety 0)))
		 (let ((token (car pair))
		       (tag (cdr pair)))
		   (declare  #-mcl (type string token) 
			     #-mcl (type symbol tag)
			     (ignore tag))
		   (if (strncmp prefix token count 0)
		       (progn (setf (cdr pair) new-tag) pair)
		   pair)))))
	  ((string= fname "fhaspref")
	   (let ((old-tag (mkkeysym (first list)))
		 (prefix (second list))
		 (count (read-from-string (fourth list)))
		 (new-tag (mkkeysym (fifth list))))
	     (declare  #-mcl (type symbol new-tag old-tag)
		       #-mcl (type fixnum count)
		       #-mcl (type string prefix))
	     #'(lambda (pair)
		 (declare (type cons pair)
			  (optimize speed (safety 0)))
		 (let ((token (car pair))
		       (tag (cdr pair)))
		   (declare #-mcl (type string token) 
			    #-mcl (type symbol tag))
		   (if (and (eq tag old-tag)
			    (strncmp prefix token count 0))
		     (progn (setf (cdr pair) new-tag) pair)
		   pair)))))
	  ((string= name "deletesuf")
	   (let ((suffix (first list))
		 (count (read-from-string (third list)))
		 (new-tag (mkkeysym (fourth list))))
	     (declare  #-mcl (type symbol new-tag)
		       #-mcl (type fixnum count)
		       #-mcl (type string suffix))
	     #'(lambda (pair)
		 (declare  #-mcl (type cons pair)
			   (optimize speed (safety 0)))
		 (let ((token (car pair))
		       (tag (cdr pair)))
		   (declare #-mcl (type string token) 
			    #-mcl (type symbol tag)
			    (ignore tag))
		   (if (strncmp-end2 suffix token count (- (length token) count))
		       (let ((root (subseq token 0 (- (length token) count))))
		       (if (or (hash-get lh root)
			       (and wh (hash-get wh root)))
			   (progn (setf (cdr pair) new-tag) pair)
			 pair))
		   pair)))))
	  ((string= fname "fdeletesuf")
	   (let ((old-tag (mkkeysym (first list)))
		 (suffix (second list))
		 (count (read-from-string (fourth list)))
		 (new-tag (mkkeysym (fifth list))))
	     (declare #-mcl (type symbol new-tag old-tag)
		      #-mcl (type fixnum count)
		      #-mcl (type string suffix))
	     #'(lambda (pair)
		 (declare #-mcl (type cons pair)
			  (optimize speed (safety 0)))
		 (let ((token (car pair))
		       (tag (cdr pair)))
		   (declare #-mcl (type string token) 
			    #-mcl (type symbol tag))
		   (if (and (eq tag old-tag)
			    (strncmp-end2 suffix token count (- (length token) count)))
		     (let ((root (subseq token 0 (- (length token) count))))
		       (if (or (hash-get lh root)
			       (and wh (hash-get wh root)))
			   (progn (setf (cdr pair) new-tag) pair)
			 pair))
		   pair)))))
	  ((string= name "hassuf")
	   (let ((suffix (first list))
		 (count (read-from-string (third list)))
		 (new-tag (mkkeysym (fourth list))))
	     (declare #-mcl (type symbol new-tag)
		      #-mcl (type fixnum count)
		      #-mcl (type string suffix))
	     #'(lambda (pair)
		 (declare #-mcl (type cons pair)
			  (optimize speed (safety 0)))
		 (let ((token (car pair))
		       (tag (cdr pair)))
		   (declare #-mcl (type string token) 
			    #-mcl (type symbol tag)
			    (ignore tag))
		   (if (strncmp-end2 suffix token count (- (length token) count))
		       (progn (setf (cdr pair) new-tag) pair)
		   pair)))))
	  ((string= fname "fhassuf")
	   (let ((old-tag (mkkeysym (first list)))
		 (suffix (second list))
		 (count (read-from-string (fourth list)))
		 (new-tag (mkkeysym (fifth list))))
	     (declare #-mcl (type symbol new-tag old-tag)
		      #-mcl (type fixnum count)
		      #-mcl (type string suffix))
	     #'(lambda (pair)
		 (declare  #-mcl (type cons pair)
			   (optimize speed (safety 0)))
		 (let ((token (car pair))
		       (tag (cdr pair)))
		   (declare #-mcl (type string token) 
			    #-mcl (type symbol tag))
		   (if (and (eq tag old-tag)
			    (strncmp-end2 suffix token count (- (length token) count)))
		       (progn (setf (cdr pair) new-tag) pair)
		     pair)))))
	  ((string= name "addsuf") 
	   (let ((suffix (first list))
;;		 (count (read-from-string (third list)))
		 (new-tag (mkkeysym (fourth list))))
	     (declare  #-mcl (type symbol new-tag)
;                      #-mcl (type fixnum count)
		       #-mcl (type string suffix))
	     #'(lambda (pair)
		 (declare #-mcl (type cons pair)
			  (optimize speed (safety 0)))
		 (let ((token (car pair))
		       (tag (cdr pair)))
		   (declare  #-mcl (type string token) 
			     #-mcl (type symbol tag)
			     (ignore tag))
		   (let ((new-word (concatenate 'string token suffix)))
		     (if (or (hash-get lh new-word)
			   (and wh (hash-get wh new-word)))
			 (progn (setf (cdr pair) new-tag) pair) 
		       pair))))))
	  ((string= fname "faddsuf")
	   (let ((old-tag (mkkeysym (first list)))
		 (suffix (second list))
;;		 (count (read-from-string (fourth list)))
		 (new-tag (mkkeysym (fifth list))))
	     (declare #-mcl (type symbol new-tag old-tag)
;;		      #-mcl (type fixnum count)
		      #-mcl (type string suffix))
	     #'(lambda (pair)
		 (declare #-mcl (type cons pair)
			  (optimize speed (safety 0)))
		 (let ((token (car pair))
		       (tag (cdr pair)))
		   (declare #-mcl (type string token) 
			    #-mcl (type symbol tag))
		   (if (eq tag old-tag)
		       (let ((new-word (concatenate 'string token suffix)))
		       (if (or (hash-get lh new-word)
			       (and wh (hash-get wh new-word)))
			   (progn (setf (cdr pair) new-tag) pair)
			 pair))
		     pair)))))
;;	  ((string= name "addpref")
;;	   (let ((prefix (first list))
;;		 (count (read-from-string (third list)))
;;		 (new-tag (mkkeysym (fourth list))))
;;	     #'(lambda (token tag)
;;	  ((string= fname "faddpref")
;;	   (let ((old-tag (mkkeysym (first list)))
;;		 (prefix (second list))
;;		 (count (read-from-string (fourth list)))
;;		 (new-tag (mkkeysym (fifth list))))
;;	     #'(lambda (token tag)
;; NOTE: NO BIGRAM SUPPORT YET
;; "goodleft" "fgoodleft" "goodright" "fgoodright"
	  (t (write-log tagger-contextual "No rule found for: ~A." list)))))


(defun apply-rules ( datum rule-list )
  "Apply rules to the values in values presuming that
   the returned list is also a list of values that can
   be passed to the next rule"
  (declare 
   (optimize speed (safety 0))
   (type list rule-list)
   (type cons datum))
  (cond ((null rule-list)
	 datum)
	((null datum)
	 (error "Null datum in rule prior to ~A.~%" (car rule-list)))
	(t (apply-rules (funcall (car rule-list) datum) (cdr rule-list)))))
  



