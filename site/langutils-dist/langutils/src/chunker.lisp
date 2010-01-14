;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: utils -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          chunker
;;;; Purpose:       A regex verb and noun phrase chunker using the 
;;;;                array matching utility infrastructure
;;;;
;;;; Programmer:    Ian S. Eslick
;;;; Date Started:  October 2004
;;;;

(in-package :langutils)

;; ===============================
;; STRING CHUNKING TOP LEVEL
;; Speed: slow (extra conversion)
;; Input size: < 100k strings

(defun chunk (text)
  "Returns a phrase-list for the provided text"
  (get-all-chunks (vector-tag text)))

(defun chunk-tokenized (text)
  "Returns a phrase-list for the provided tokenized string"
  (get-all-chunks (vector-tag-tokenized text)))

;; ===============================
;; VECTOR-DOCUMENT INTERFACE
;; Speed: optimal
;; Input size: unlimited

(defmethod get-all-chunks ((doc vector-document))
  "Returns a list of PHRASEs referencing 'doc' for
   all supported primitive phrase types"
  (let ((nxs (get-noun-chunks doc))
	(vxs (get-verb-chunks doc))
	(axs (get-adverb-chunks doc))
        (ps (get-p-chunks doc)))
    (append nxs vxs axs ps)))

(defmethod get-verb-arg-chunks ((doc vector-document))
  (do-collect-vector-matches (start end #.(localize-expression (list :AND verb-pattern p-pattern noun-pattern) :package 'keyword))
			     ((document-tags doc))
       (write-log chunker "Found verb-arg: ~A (~A : ~A)" (subseq (document-tags doc) start (1+ end)) start end)
       (make-instance 'phrase
		      :type :verb-arg
		      :document doc
		      :start start
		      :end end)))

(defmethod get-event-chunks ((doc vector-document))
  "Return vx+nx (simple verb arg) phrase objects"
  (do-collect-vector-matches (start end #.(localize-expression (list :AND verb-pattern noun-pattern) :package 'keyword))
			     ((document-tags doc))
       (write-log chunker "Found event: ~A (~A : ~A)" (subseq (document-tags doc) start (1+ end)) start end)
       (make-instance 'phrase
		      :type :event
		      :document doc
		      :start start
		      :end end)))
  

(defmethod get-noun-chunks ((doc vector-document))
  "Return a list of all nx phrases"
  (do-collect-vector-matches (start end #.(localize-expression noun-pattern :package 'keyword))
			     ((document-tags doc))
       (write-log chunker "Found np: ~A (~A : ~A)" (subseq (document-tags doc) start (1+ end)) start end)
       (make-instance 'phrase
		      :type :np
		      :document doc
		      :start start
		      :end end)))

(defmethod get-verb-chunks ((doc vector-document))
  "Return a list of all primitive vx phrases - no arguments"
  (do-collect-vector-matches (start end #.(localize-expression verb-pattern :package 'keyword)) 
			     ((document-tags doc))
      (write-log chunker "Found vp: ~A (~A : ~A)" (subseq (document-tags doc) start (1+ end)) start end)
      (make-instance 'phrase
		     :type :vp
		     :document doc
		     :start start
		     :end end)))

(defmethod get-adverb-chunks ((doc vector-document))
  "Return a list of all adverbial phrases"
  (do-collect-vector-matches (start end #.(localize-expression adv-pattern :package 'keyword))
			     ((document-tags doc))
      (write-log chunker "Found ap: ~A (~A : ~A)" (subseq (document-tags doc) start (1+ end)) start end)
      (make-instance 'phrase
		     :type :advp
		     :document doc
		     :start start
		     :end end)))

(defmethod get-p-chunks ((doc vector-document))
  "Return a list of all prepositions as phrases"
  (do-collect-vector-matches (start end #.(localize-expression p-pattern :package 'keyword))
			     ((document-tags doc))
      (write-log chunker "Found prep: ~A (~A : ~A)" (subseq (document-tags doc) start (1+ end)) start end)
      (make-instance 'phrase
		     :type :prep
		     :document doc
		     :start start
		     :end end)))

;; ====================================
;; Simple tagging interactive function

(defun test-phrase (text)
  "Prints all the phrases found in the text for simple
   experimenting"
  (let ((doc (vector-tag text)))
    (format t "Tagged: ~A~%" (print-vector-document doc))
    (mapcar #'print-phrase (get-all-chunks doc))))
    
;; =======================================
;; Experiment with higher order structure 

(defun all-vx+nx-phrases (phrases)
  "Overly hairy function for finding all vx phrases that
   are followed by nx.  Get event chunks is a better way 
   to do this."
  (declare (optimize speed (safety 1))
           (type list phrases))
  (let ((pairs nil))
    (declare (type list pairs))
    (labels ((following-noun (start phrases count)
			     (cond ((or (null phrases) (= count 2))
				    nil)
				   ((= start (phrase-start (car phrases)))
				    (car phrases))
				   (t (following-noun start (cdr phrases) (1+ count)))))
	     (rec (cp phrases)
		  (cond ((null phrases)
			 (nreverse pairs))
			((eq (phrase-type cp) :verb)
			 (aif (following-noun (1+ (phrase-end cp)) phrases 0)
			      (push (make-instance 'phrase
						   :type :event
						   :document (phrase-document cp)
						   :start (phrase-start cp)
						   :end (phrase-end it))
				    pairs))
			 (rec (car phrases) (cdr phrases)))
			(t (rec (car phrases) (cdr phrases))))))
      (declare (inline following-noun))
      (rec (car phrases) (cdr phrases)))))


