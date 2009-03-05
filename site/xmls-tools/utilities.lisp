;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; utilities.lisp: utilities for xmls and general use
;;;; Copyright (C) 2004 Robert Marlow <bobstopper@bobturf.org>
;;;;
;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Library General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 2 of the License, or (at your option) any later version.
;;;;
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Library General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Library General Public
;;;; License along with this library; if not, write to the
;;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;;; Boston, MA  02111-1307, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :xmls-tools)


(defun get-attr-value (attr-name xml)
  (second (assoc attr-name (node-attrs xml) :test #'equal)))


(defun find-subtree (xml-tree path-spec)
  "Finds the node requested by PATH-SPEC by traversing XML-TREE in the
manner specified by PATH-SPEC.
PATH-SPEC is a list of nodes to visit after the root node to reach the
final destination listed in the order they are to be visited. Each node
is either an XMLS:NODE-NAME or a cons of an XMLS:NODE-NAME and an integer
specifying which of a number of possible matches to return (starting
from 0)

For example, to find the second c element in the following XMLS tree:
(\"a\" NIL
       ((\"b\" . \"foo\") NIL
	((\"c\" . \"foo\") NIL \"Hello\")
	((\"c\" . \"foo\") NIL \"World\")))

The following PATH-SPEC could be used:
((\"b\" . \"foo\") ((\"c\" . \"foo\") 1))

Note: FIND-SUBTREE supports FIND-CHILD's optional namespace, so the
following is equally permissible:
(\"b\" (\"c\" 1))"
  (if (null path-spec)
      xml-tree
    (find-subtree (find-child xml-tree (if (spec-enumeration
                                            (car path-spec))
                                           (caar path-spec)
                                         (car path-spec))
                              :skip-matches (let ((it (spec-enumeration
						       (car path-spec))))
					      (if it
						  it
						  0)))
		  (rest path-spec))))


(defun spec-enumeration (spec)
  (and (listp spec)
       (or (and (integerp (cdr spec)) (cdr spec))
	   (and (listp (cdr spec))
		(integerp (cadr spec))
		(cadr spec)))))


(defun find-child (xml-tree child-name &key (skip-matches 0) (skip-children 0)
		   child-namespace)
  (let ((child-position 0)
	(match-position 0))
    (dolist (child (nthcdr skip-children (node-children xml-tree)))
      (when (and (listp child)
		 (equal child-name (node-name child))
		 (or (null child-namespace)
		     (equal child-namespace (node-ns child))))
	(incf match-position)
	(when (> match-position skip-matches)
	  (return-from find-child (values child child-position))))
      (incf child-position))))


(defun find-all-children (xml-tree child-name &key child-namespace)
  (do* ((i 0 (1+ i))
        (children (list (find-child xml-tree child-name 
				    :child-namespace child-namespace))
                  (cons (find-child xml-tree child-name :skip-matches i
				    :child-namespace child-namespace)
                        children)))
       ((null (first children))
	(nreverse (rest children)))))


(defun search-children (element-name xml-children)
  (if (null xml-children)
      nil
      (if (and (listp (car xml-children))
	       (equal (node-name (car xml-children)) element-name))
	  (car xml-children)
	  (search-children element-name (cdr xml-children)))))


(defun get-pcdata (xml-children &optional (out-string ""))
  (if (null xml-children)
      out-string
      (if (stringp (first xml-children))
	  (get-pcdata (rest xml-children)
		      (format nil "~A~A" out-string 
			      (remove #\return (first xml-children))))
	  (get-pcdata (cdr xml-children) out-string))))
