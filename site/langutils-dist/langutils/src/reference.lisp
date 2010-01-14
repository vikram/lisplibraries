;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: utils -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          reference
;;;; Purpose:       A wrapper around vector representations of text
;;;;
;;;; Programmer:    Ian S. Eslick
;;;; Date Started:  October 2004
;;;;

(in-package :langutils)

;; ---------------------------
;; Document wrapper

(defclass vector-document ()
  ((text ;; text vector
    :accessor document-text
    :initarg :text
    :type (array fixnum))
   (tags  ;; tag vector
    :accessor document-tags
    :initarg :tags
    :type (array symbol))
   (annotations
    :accessor document-annotations
    :initarg :annotations
    :initform nil
    :type list)))

(defmethod length-of ((doc vector-document))
  (length (document-text doc)))

(defun make-vector-document (text &optional tags)
  (make-instance 'vector-document
		 :text text
		 :tags tags))

;; Document accessors

(declaim (inline get-tag get-word))
(defmethod get-token-id ((doc vector-document) offset)
  (svref (document-text doc) offset))
(defmethod get-tag ((doc vector-document) offset)
  (svref (document-tags doc) offset))

(defmethod get-annotation ((doc vector-document) key)
  "First returned value is the association value
   or null if none.  The second is true if the
   key exists, nil otherwise"
  (aif (assoc key (document-annotations doc))
       (values (cdr it) t)
       (values nil nil)))

(defmethod set-annotation ((doc vector-document) key value &key (method :override))
  "Add an annotation to object using method :override, :push, :duplicate-key"
  (flet ((set-on-empty () (setf (document-annotations doc) (acons key value nil))))
    (case method
      (:override 
	(aif (assoc key (document-annotations doc))
	     (rplacd it value)
	     (set-on-empty)))
      (:push
	(aif (assoc key (document-annotations doc))
	     (push value (cdr it))
	     (set-on-empty)))
      (:duplicate-key
	(setf (document-annotations doc) (acons key value (document-annotations doc))))
      (t (error "Invalid method ~A for add-annotation" method)))
    t))

(defmethod unset-annotation ((doc vector-document) key)
  (remove key (document-annotations doc) :key #'car))

;; Create a vector document from a file or text

(defun vector-document (input)
  (typecase input 
    (string (vector-tag input))
    (pathname (read-vector-document input))
    (vector-document input)
    (t (error "No handler for input type: ~A" (type-of input)))))

;; Documents to/from strings

(defun string-tag ( string &optional (stream t))
  "Tokenizes and tags the string returning
   a standard tagged string using '/' as a separator"
  (string-tag-tokenized
   (mvretn 3 (tokenize-string string))
   stream))

(defun string-tag-tokenized ( string &optional (stream t))
  (print-vector-document (vector-tag string) :stream stream))

(defmethod print-vector-document ( (doc vector-document) &key (stream t) (with-tags t))
  (with-slots (text tags) doc
    (loop for token fixnum across (document-text doc) and
	    tag   symbol across (document-tags doc) do
	(if with-tags
	    (format stream "~A/~A " (token-for-id token) tag)
	  (format stream "~A " (token-for-id token))))
    (format stream "~%")))

(defmethod vector-document-words ( (doc vector-document) )
  (token-array->words (document-text doc)))

;; Documents to/from files in native form

(defmethod vector-doc-as-ids ( (doc vector-document) )
  "Converts the word array to ids with shared structure
   for the other elements; keeps the data 'in the family'
   so the source or destination documents should be short lived"
  (let* ((word-array (document-text doc))
	 (id-array (make-array (length word-array) :element-type 'fixnum)))
    (make-instance 'vector-document
		   :text (map-into id-array #'id-for-token word-array)
		   :tags (document-tags doc)
		   :annotations (document-annotations doc))))

(defmethod vector-doc-as-words ( (doc vector-document) )
  (let* ((id-array (document-text doc))
	 (word-array (make-array (length id-array) :element-type 'string)))
    (make-instance 'vector-document
		   :text (map-into word-array #'token-for-id id-array)
		   :tags (document-tags doc)
		   :annotations (document-annotations doc))))

(defmethod write-vector-document ((doc vector-document) filename &key (with-tags t) (if-exists :supersede))
  (with-open-file (s filename :direction :output :if-exists if-exists)
    (print-vector-document doc :stream s :with-tags with-tags)))

(defmethod read-vector-document (filename)
  (vector-tag (read-file-to-string filename)))

(defmethod read-vector-document-to-string ((doc vector-document) &key (with-tags t))
  (with-output-to-string (s)
    (print-vector-document doc :with-tags with-tags :stream s)))



;; ========================================================
;; Word wrapper
;; ========================================================

(defclass+ word ()
  ((id nil)       ;; surface form id, can index lexicon
   (document nil) ;; pointer or id if in registry
   (offset nil))
  (:prefix "word-"))

(defmethod word-tag ((w word))
  (get-tag (word-document w) (word-offset w)))

(defmethod word-name ((w word))
  (id-for-token (word-id w)))

;; ========================================================
;; Phrase wrapper
;; ========================================================

(defclass+ phrase ()
  ((type nil)     ;; phrase type
   (document nil) ;; pointer or id if in registry
   (start nil)    ;; offset in doc
   (end nil)      ;; end in doc
   (annotations nil))
  (:prefix "phrase-"))

(defmethod print-object ((p phrase) stream)
  (with-output-to-string (pstr)
    (print-phrase p :stream pstr :with-tags nil :newline nil)
    (format stream "#<~A:~A \"~A\">" 
	    (class-name (class-of p))
	    (phrase-type p)
	    (get-output-stream-string pstr))))

(defun make-phrase-from-sentence (tok-string &optional tag-array)
  (let ((words (extract-words tok-string)))
    (make-phrase (map-into (make-array (length words)) #'id-for-token words)
		 tag-array)))

(defun make-phrase (concept)
  "Take two arrays of test and tags and create a phrase 
   that points at a vdoc created from the two arrays"
  (make-phrase-from-vdoc
   (vector-doc-as-ids 
    (make-vector-document text-array tag-array))
   (length text-array)))

(defmethod find-phrase ((p phrase) (doc vector-document) &key (match :all) (start 0) (ignore-start nil) (ignore-end nil))
  (if (eq (phrase-document p) doc)
      (values (phrase-start p) (phrase-end p))
    (let ((ptext (document-text (phrase-document p)))
	  (dtext (document-text doc))
	  (ptags (document-tags (phrase-document p)))
	  (dtags (document-tags doc)))
    (labels ((match-tokens (doc-offset phrase-offset)
			   (if (and ignore-start ignore-end
				    (>= doc-offset ignore-start)
				    (<= doc-offset ignore-end))
			       nil
			     (case match
			       (:all (and (match-text doc-offset phrase-offset)
					  (match-tags doc-offset phrase-offset)))
			       (:words (match-text doc-offset phrase-offset))
			       (:pos (match-tags doc-offset phrase-offset))
			       (t nil))))
	     (match-text (d p) (eq (aref ptext p) (aref dtext d)))
	     (match-tags (d p) (eq (aref ptags p) (aref dtags d))))
      (loop for offset from start upto (1- (length dtext)) 
	    finally (return nil) do
	    (when (match-tokens offset 0)
	      (when (loop for pindex from 0 
			  for dindex from offset 
			  while (and (< pindex (length ptext))
				     (< dindex (length dtext))) 
			  finally (return t) do
			  (unless (match-tokens dindex pindex) (return nil)))
		(return (values offset (+ offset (1- (length ptext))))))))))))
	       

(defmethod get-annotation ((p phrase) key)
  "First returned value is the association value
   or null if none.  The second is true if the
   key exists, nil otherwise"
  (aif (assoc key (phrase-annotations p))
       (values (cdr it) t)
       (values nil nil)))

(defmethod set-annotation ((p phrase) key value &key (method :override))
  "Add an annotation to object using method :override, :push, :duplicate-key"
  (flet ((set-on-empty () (setf (phrase-annotations p) (acons key value nil))))
    (case method
      (:override 
	(aif (assoc key (phrase-annotations p))
	     (rplacd it value)
	     (set-on-empty)))
      (:push
	(aif (assoc key (phrase-annotations p))
	     (push value (cdr it))
	     (set-on-empty)))
      (:duplicate-key
	(setf (phrase-annotations p) (acons key value (phrase-annotations p))))
      (t (error "Invalid method ~A for add-annotation" method)))
    t))

(defmethod unset-annotation ((p phrase) key)
  (remove key (phrase-annotations p) :key #'car))

(defun make-phrase-from-vdoc (doc start len &optional type)
  (make-instance 'phrase 
		 :type type
		 :document doc
		 :start start
		 :end (+ start len (- 1))))

(defmethod get-token-id ((p phrase) offset)
  (get-token-id (document-text (phrase-document p))
	     (+ (phrase-start p) offset)))

(defmethod get-tag ((p phrase) offset)
  (get-token-id (document-tags (phrase-document p))
		(+ (phrase-start p) offset)))

(defmethod phrase-length ((p phrase))
  (with-slots (end start) p
    (- end start -1)))

(defun print-token-array (tokens start stop &key pos pos-start stream with-tags newline)
  (let ((offset (- pos-start start)))
    (loop for index from start upto stop do
      (if with-tags
	  (format stream "~A/~A " (token-for-id (aref tokens index)) (aref pos (+ index offset)))
	(format stream "~A " (token-for-id (aref tokens index)))))
    (when newline (format stream "~%"))))

(defmethod print-phrase ((p phrase) &key (stream t) (with-tags t) (with-info nil) (newline t))
  (with-slots (text tags) (phrase-document p)
    (when with-info (format stream "~A phrase: " (phrase-type p)))
    (print-token-array text (phrase-start p) (phrase-end p)
		       :pos (if with-tags tags nil)
		       :pos-start (if with-tags (phrase-start p) 0)
		       :with-tags with-tags
		       :stream stream
		       :newline newline)))

(defmethod print-window ((p phrase) wsize &key (stream t) (with-tags t) (with-info nil) (newline t))
  (with-slots (text tags) (phrase-document p)
    (let ((start (limit-min 0 (- (phrase-start p) wsize)))
	  (end (limit-max (1- (length-of (phrase-document p)))
			  (+ (phrase-end p) wsize))))
      (loop for index from start upto end do
	(when with-info (format stream "~A phrase: " (phrase-type p)))
	(if with-tags
	    (format stream "~A/~A " (token-for-id (aref text index)) (aref tags index))
	  (format stream "~A " (token-for-id (aref text index)))))
      (when newline (format stream "~%")))))

(defun token-array->words (tokens)
  (on-array (cons it rec) nil tokens))

(defun phrase-words (phrase &optional index)
  (cond ((null index)
         (phrase-words phrase (phrase-start phrase)))
        ((>= index (1+ (phrase-end phrase)))
         nil)
        (t (cons (aref (document-text (phrase-document phrase)) index)
		 (phrase-words phrase (1+ index))))))

;; Phrase operations

(defmethod copy-phrase ((p phrase) &optional (annotations t))
  (make-instance 'phrase
		 :type (phrase-type p)
		 :document (phrase-document p)
		 :start (phrase-start p)
		 :end (phrase-end p)
		 :annotations (when annotations (copy-list (phrase-annotations p)))))

(defmethod phrase-distance ((p1 phrase) (p2 phrase))
  "Distance between the nearest end of two phrases"
  (let* ((p1-start (slot-value p1 'start))
         (p1-end (slot-value p1 'end))
         (p2-start (slot-value p2 'start))
         (p2-end (slot-value p2 'end)))
    (cond ((> p2-start p1-end)
	   (- p2-start p1-end))
	  ((> p1-start p2-end)
           (- p1-start p2-end))
	  ((or (> p2-end p1-start) 
	       (> p1-end p2-start))
	   0) 
	  (t (error "I didn't consider a case in phrase-distance")))))

(defmethod phrase-overlap ((ph1 phrase) (ph2 phrase))
  (not (or (< (slot-value ph1 'end) (slot-value ph2 'start))
	   (< (slot-value ph2 'end) (slot-value ph2 'start)))))

(defmethod phrase-equal ((ph1 phrase) (ph2 phrase))
  (and (eq (phrase-length ph1) (phrase-length ph2))
       (loop 
	 with text1 = (document-text (phrase-document ph1)) and
	      text2 = (document-text (phrase-document ph2))
	 for i from (phrase-start ph1) upto (phrase-end ph1) 
	 for j from (phrase-start ph2) upto (phrase-end ph2) 
	 finally (return t) 
	 do
	 (when (neq (aref text1 i) (aref text2 j))
	   (return nil)))))

(defmethod phrase-lemmas ((ph phrase))
  "Returns the lemmatized phrase represented by the underlying phrase"
  (mapcar #'get-lemma-for-id (phrase-words ph)))

(defmethod print-phrase-lemmas ((ph phrase))
  (apply #'concatenate (cons 'string
   (shuffle (mapcar #'token-for-id (phrase-lemmas ph))
	  (repeat " " (1- (phrase-length ph)))))))


;; =========================================================
;; Altered phrase - keep doc refs, but change active content
;; =========================================================

;; Allows us to lemma the original phrase but still
;; perform ops on it as phrases; means lots of generic
;; functions though...
(defclass+ altered-phrase (phrase)
  ((custom-document nil))
  (:prefix "altered-phrase-"))

(defmethod make-document-from-phrase ((p phrase))
  "Copy referenced phrase data into it's own document"
  (let ((start (phrase-start p))
	(end (phrase-end p))
	(text (document-text (phrase-document p)))
	(tags (document-tags (phrase-document p))))
    (make-instance 'vector-document
		   :text (subseq text start (1+ end))
		   :tags (subseq tags start (1+ end))
		   :annotations nil)))

(defmethod make-alterable-phrase ((p phrase))
  (change-class (copy-phrase p) 'altered-phrase
		:custom-document (make-document-from-phrase p)))

;; Spoof the altered document as the original for most calls
(defmethod phrase-document ((p altered-phrase)) (altered-phrase-custom-document p))
(defmethod phrase-start ((p altered-phrase)) 0)
(defmethod phrase-end ((p altered-phrase)) (1- (length-of (altered-phrase-custom-document p))))
(defmethod phrase-length ((p altered-phrase)) (length-of (altered-phrase-custom-document p)))

;; Mutate the 'phrase' data
(defmethod change-word ((p phrase) index new-token &optional new-pos)
  (let ((start (phrase-start p)))
    (change-word (change-class p 'altered-phrase
			       :custom-document (make-document-from-phrase p))
		 (- index start) new-token new-pos)))

;;(defmethod remove-word ((p phrase) index)
;;  (remove-word (change-class p 'altered-phrase
;;			     :custom-document (make-document-from-phrase p))
;;	       index))

(defmethod change-word ((p altered-phrase) index new-token &optional new-pos)
  (let ((text (document-text (altered-phrase-custom-document p)))
	(tags (document-tags (altered-phrase-custom-document p))))
    (setf (aref text index) new-token)
    (when new-pos
      (setf (aref tags index) new-pos))
    p))


;; TODO: add these methods
;;(defmethod remove-word ((p altered-phrase) index))
;;(defmethod add-word ((p altered-phrase) index))


;; =================================
;; Handle destructive lemmatization
;; =================================

(defmethod lemmatize-phrase ((p phrase) &optional (offset 0))
  "Destructive lemmatization of a phrase"
  (let ((doc (phrase-document p))
	(start (phrase-start p)))
    (when (<= offset (phrase-end p))
      (loop for idx from (+ offset start) upto (phrase-end p) do
	(let ((token (get-token-id doc idx)))
	  (awhen2 (get-lemma-for-id token 
				    :pos (get-tag doc idx) 
				    :noun nil)
		  (when (not (eq it token))
		    (return (lemmatize-phrase (change-word p idx it) (1+ (- idx start))))))))))
  p)

(defparameter *test* nil)
(defmethod lemmatize-phrase ((p altered-phrase) &optional (offset 0))
  "Destructive lemmatization of a phrase"
  (let ((doc (phrase-document p)))
    (setf *test* p)
    (when (<= offset (phrase-end p))
      (loop for idx from (+ offset (phrase-start p)) upto (phrase-end p) do
	(let ((token (get-token-id doc idx)))
	  (awhen2 (get-lemma-for-id token 
				    :pos (get-tag doc idx) 
				    :noun nil)
		  (when (not (eq it token))
		    (change-word p idx it)))))))
  p)
  
  
  
		       
      

