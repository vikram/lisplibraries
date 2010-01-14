;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: utils -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          vechash.lisp
;;;; Purpose:       Fast vector element hash implementation
;;;; Programmer:    Ian S. Eslick
;;;; Date Started:  August 2004
;;;;

(in-package :utils)

;; ----------------------------------------------------------
;; VECHASH -- Vector Keyed Hash Trees
;;
;; Tree of hashes for fast lookup of integer simple-vectors keys
;; over very large vector spaces
;;
;; Assumptions:
;; * Vector elements are positive integers 1...n
;; * You are loading thousands of elements (expensive otherwise)
;; * Sparsely populated tree 
;; ----------------------------------------------------------

;; Notes: add parameters for hashtable inits
(defvar *vechash-threshold* 10)
(defun-exported make-vechash (&key (depth nil))
  (declare (ignorable depth))
  (make-hash-table :test #'eq :size 1000 :rehash-size 1.5 :rehash-threshold 0.7))

;; Internal functions to reduce # of total hashtables using alists
(defun-exported vechash-int-get (key hash)
  (if (consp hash)
    (cdr (assoc key hash))
    (gethash key hash)))

(defun-exported vechash-int-put (key val hash)
  (cond ((and (consp hash) (assoc key hash)) ;; replace existing
	 (setf (cdr (assoc key hash)) val))
	((consp hash) ;; add new behind head to preserve validity of caller pointer to head of alist
	 (setf (cdr hash) (acons key val (cdr hash))))
	(t            ;; it's a hashtable
	 (setf (gethash key hash) val))))

(defmacro-exported vechash-upgrade-hash (vh-pair)
  `(if (and (consp (cdr ,vh-pair))
	    (> (length (cdr ,vh-pair)) *vechash-threshold*))
     (let ((new-hash (make-hash-table :test #'eq :size (* 2 *vechash-threshold*) :rehash-size 1.5 :rehash-threshold 0.7)))
       (dolist (a (cdr ,vh-pair))
	 (setf (gethash (car a) new-hash) (cdr a)))
       (setf (cdr ,vh-pair) new-hash))))

;; Depth limited vechash get
;;(defun-exported vechash-get-depth (vec-key vechash depth)
;;  (let ((last (min (1- (length vec-key)) depth)

;; Get from the hash tree, not depth limited
(defun-exported vechash-get-nodepth (vec-key vechash)
  (let ((last (1- (length vec-key))))
    (labels ((vhget (count ht)
	       (let* ((key (elt vec-key count))
		      (pdata (vechash-int-get key ht)))
		 (cond ((null pdata)       ;; early termination or no item
			nil) 
		       ((= count last)     ;; last index, pdata non-null return car (null or val)
			(car pdata))
		       ((null (cdr pdata)) ;; if not last index, but continuation is nil, terminate
			nil)
		       (t                  ;; otherwise, continue on...
			(vhget (1+ count) (cdr pdata)))))))
      (vhget 0 vechash))))

(defun-exported vechash-get (vec-key vechash)
;;  (if (car vechash)
;;      (vechash-get-depth vec-key (cdr vechash) (car vechash))
    (vechash-get-nodepth vec-key vechash))
  
		       
;; Destructive put
(defun-exported vechash-put (vec-key put-value vechash)
  (let ((last (1- (length vec-key))))
    (labels (;; End of sequence, put value in car of curr vechash entry
	     (vhset (key pdata ht)
	       (if (null pdata)
		 (progn (vechash-int-put key (cons put-value nil) ht) put-value)
		 (setf (car pdata) put-value)))
	     ;; Middle of sequence, extend current location by adding new ht & recurse
	     (vhextend (count pdata)
	       (let ((new-ht (acons -1 nil nil)))
		 (setf (cdr pdata) new-ht)
		 (vhput (1+ count) new-ht)))
	     ;; Recursive put, walk through sequence extending as necessary
	     (vhput (count ht)
	       (let* ((key (aref vec-key count))
		      (pdata (vechash-int-get key ht)))
		 (cond ((= count last) ;; done
			(vhset key pdata ht))
		       ((null pdata) ;; empty entry
			(let ((datum (cons nil nil)))
			  (vechash-int-put key datum ht)
			  (vhextend count datum)))
		       ((null (cdr pdata)) ;; value, no hashtable
			(vhextend count pdata))
		       (t  ;; recurse to next entry
			(vechash-upgrade-hash pdata)
			(vhput (1+ count) (cdr pdata)))))))
      (vhput 0 vechash))))

;; Map all elements in my funky vector hash
(defun-exported mapvechash (f vhash)
  (declare (:explain :tailmerging))
  (if (consp vhash)
      (dolist (asoc vhash)
	(let ((pair (cdr asoc)))
	  (when (car pair) (funcall f (car pair)))
	  (when (cdr pair) (mapvechash f (cdr pair)))))
    (loop for pair being the hash-value of vhash do 
	  (when (car pair) (funcall f (car pair)))
	  (when (cdr pair) (mapvechash f (cdr pair))))))
