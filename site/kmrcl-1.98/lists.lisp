;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          lists.lisp
;;;; Purpose:       Functions for lists for KMRCL package
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Apr 2000
;;;;
;;;; $Id$
;;;;
;;;; This file, part of KMRCL, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;;
;;;; KMRCL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:kmrcl)

(defun mklist (obj)
  "Make into list if atom"
  (if (listp obj) obj (list obj)))

(defun map-and-remove-nils (fn lst)
  "mao a list by function, eliminate elements where fn returns nil"
  (let ((acc nil))
    (dolist (x lst (nreverse acc))
      (let ((val (funcall fn x)))
        (when val (push val acc))))))

(defun filter (fn lst)
  "Filter a list by function, eliminate elements where fn returns nil"
  (let ((acc nil))
    (dolist (x lst (nreverse acc))
      (when (funcall fn x)
        (push x acc)))))

(defun appendnew (l1 l2)
  "Append two lists, filtering out elem from second list that are already in first list"
  (dolist (elem l2 l1)
    (unless (find elem l1)
      (setq l1 (append l1 (list elem))))))

(defun remove-from-tree-if (pred tree &optional atom-processor)
  "Strip from tree of atoms that satistify predicate"
  (if (atom tree)
      (unless (funcall pred tree)
        (if atom-processor
            (funcall atom-processor tree)
          tree))
    (let ((car-strip (remove-from-tree-if pred (car tree) atom-processor))
          (cdr-strip (remove-from-tree-if pred (cdr tree) atom-processor)))
      (cond
       ((and car-strip (atom (cadr tree)) (null cdr-strip))
        (list car-strip))
       ((and car-strip cdr-strip)
        (cons car-strip cdr-strip))
       (car-strip
        car-strip)
       (cdr-strip
        cdr-strip)))))

(defun find-tree (sym tree)
  "Finds an atom as a car in tree and returns cdr tree at that positions"
  (if (or (null tree) (atom tree))
      nil
    (if (eql sym (car tree))
        (cdr tree)
      (aif (find-tree sym (car tree))
          it
        (aif (find-tree sym (cdr tree))
            it
            nil)))))

(defun flatten (lis)
  (cond ((atom lis) lis)
        ((listp (car lis))
         (append (flatten (car lis)) (flatten (cdr lis))))
        (t (append (list (car lis)) (flatten (cdr lis))))))

;;; Keyword functions

(defun remove-keyword (key arglist)
  (loop for sublist = arglist then rest until (null sublist)
        for (elt arg . rest) = sublist
        unless (eq key elt) append (list elt arg)))

(defun remove-keywords (key-names args)
  (loop for ( name val ) on args by #'cddr
        unless (member (symbol-name name) key-names
                       :key #'symbol-name :test 'equal)
        append (list name val)))

(defun mapappend (func seq)
  (apply #'append (mapcar func seq)))

(defun mapcar-append-string-nontailrec (func v)
  "Concatenate results of mapcar lambda calls"
  (aif (car v)
       (concatenate 'string (funcall func it)
                    (mapcar-append-string-nontailrec func (cdr v)))
       ""))


(defun mapcar-append-string (func v &optional (accum ""))
  "Concatenate results of mapcar lambda calls"
  (aif (car v)
       (mapcar-append-string
        func
        (cdr v)
        (concatenate 'string accum (funcall func it)))
       accum))

(defun mapcar2-append-string-nontailrec (func la lb)
  "Concatenate results of mapcar lambda call's over two lists"
  (let ((a (car la))
        (b (car lb)))
    (if (and a b)
      (concatenate 'string (funcall func a b)
                   (mapcar2-append-string-nontailrec func (cdr la) (cdr lb)))
      "")))

(defun mapcar2-append-string (func la lb &optional (accum ""))
  "Concatenate results of mapcar lambda call's over two lists"
  (let ((a (car la))
        (b (car lb)))
    (if (and a b)
        (mapcar2-append-string func (cdr la)  (cdr lb)
                               (concatenate 'string accum (funcall func a b)))
      accum)))

(defun append-sublists (list)
  "Takes a list of lists and appends all sublists"
  (let ((results (car list)))
    (dolist (elem (cdr list) results)
      (setq results (append results elem)))))


;; alists and plists

(defun alist-elem-p (elem)
  (and (consp elem) (atom (car elem)) (atom (cdr elem))))

(defun alistp (alist)
  (when (listp alist)
    (dolist (elem alist)
      (unless (alist-elem-p elem)
        (return-from alistp nil)))
    t))

(defmacro update-alist (akey value alist &key (test '#'eql) (key '#'identity))
  "Macro to support below (setf get-alist)"
  (let ((elem (gensym "ELEM-"))
        (val (gensym "VAL-")))
    `(let ((,elem (assoc ,akey ,alist :test ,test :key ,key))
           (,val ,value))
       (cond
        (,elem
         (setf (cdr ,elem) ,val))
        (,alist
         (setf (cdr (last ,alist)) (list (cons ,akey ,val))))
         (t
          (setf ,alist (list (cons ,akey ,val)))))
       ,alist)))

(defun get-alist (key alist &key (test #'eql))
  (cdr (assoc key alist :test test)))

(defun (setf get-alist) (value key alist &key (test #'eql))
  "This won't work if the alist is NIL."
  (update-alist key value alist :test test)
  value)

(defun alist-plist (alist)
  (apply #'append (mapcar #'(lambda (x) (list (car x) (cdr x))) alist)))

(defun plist-alist (plist)
  (do ((alist '())
       (pl plist (cddr pl)))
      ((null pl) alist)
    (setq alist (acons (car pl) (cadr pl) alist))))

(defmacro update-plist (pkey value plist &key (test '#'eql))
  "Macro to support below (setf get-alist)"
  (let ((pos (gensym)))
    `(let ((,pos (member ,pkey ,plist :test ,test)))
       (if ,pos
           (progn
             (setf (cadr ,pos) ,value)
             ,plist)
         (setf ,plist (append ,plist (list ,pkey ,value)))))))


(defun unique-slot-values (list slot &key (test 'eql))
  (let ((uniq '()))
    (dolist (item list (nreverse uniq))
      (let ((value (slot-value item slot)))
        (unless (find value uniq :test test)
          (push value uniq))))))



