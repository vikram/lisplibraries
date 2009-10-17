;;; -*- package: WILBUR; Syntax: Common-lisp; Base: 10 -*-

;;;
;;;;  rdf-parser.lisp
;;;
;;;
;;; --------------------------------------------------------------------------------------
;;;
;;;   The contents of this file are subject to the NOKOS License Version 1.0a (the
;;;   "License"); you may not use this file except in compliance with the License. 
;;;
;;;   Software distributed under the License is distributed on an "AS IS" basis, WITHOUT
;;;   WARRANTY OF ANY KIND, either express or implied. See the License for the specific
;;;   language governing rights and limitations under the License. 
;;;
;;;   The Original Software is 
;;;     WILBUR2: Nokia Semantic Web Toolkit for CLOS
;;;
;;;   Copyright (c) 2001-2005 Nokia and others. All Rights Reserved.
;;;   Portions Copyright (c) 1989-1992 Ora Lassila. All Rights Reserved.
;;;
;;;   Contributor(s): Ora Lassila (mailto:ora.lassila@nokia.com)
;;;                   Louis Theran <theran@pobox.com>
;;;
;;; --------------------------------------------------------------------------------------
;;;
;;;
;;;   Version: $Id: rdf-parser.lisp,v 1.11 2004/11/28 23:14:10 ora Exp $
;;;
;;;   Purpose: This file contains an implementation of an RDF parser, using a
;;;   "near streaming" algorithm based on a simple state machine. The parser
;;;   implements all of RDF M+S excluding "aboutEachPrefix" (what, are you
;;;   surprised?) as well as RDFCore.
;;;


(in-package "WILBUR")


;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS RDF-SYNTAX-NORMALIZER
;;;

(defclass rdf-syntax-normalizer (nox:sax-filter)
  ())

(defmethod nox:sax-consumer-mode ((self rdf-syntax-normalizer))
  (nox:sax-consumer-mode (nox:sax-producer-consumer self)))

(defmethod nox:start-element ((self rdf-syntax-normalizer)
                              (tag nox:open-tag)
                              mode)
  (let ((attributes (nox:tag-attributes tag))
        (properties nil)
        (consumer (nox:sax-producer-consumer self))
        (namespaces (nox:tag-namespaces tag)))
    (nox:do-string-dict (key value attributes)
      (cond ((null (find key -rdf-attrs- :test #'string=))
             (setf properties (nox:string-dict-add properties key value)
                   attributes (nox:string-dict-del attributes key)))
            ((string= key -rdf-abouteachprefix-uri-)
             (cerror "Ignore" 'feature-not-supported :thing "aboutEachPrefix")
             (setf attributes (nox:string-dict-del attributes key)))))
    (setf (nox:tag-attributes tag) attributes)
    (nox:start-element consumer tag mode)
    (nox:do-string-dict (key value properties)
      (unless (or (string-equal key -xml-lang-attr-)
                  (string-equal key "xml:space")
                  (string-equal key "xml:base"))
        (let ((new-tag (make-instance 'nox:open-tag
                         :string key
                         :base (nox::tag-base tag)
                         :namespaces namespaces)))
          (nox:start-element consumer new-tag (nox:sax-consumer-mode self))
          (nox:char-content consumer value (nox:sax-consumer-mode self))
          (nox:end-element consumer new-tag (nox:sax-consumer-mode self)))))))

(defmethod nox:maybe-use-namespace ((self rdf-syntax-normalizer) prefix uri)
  (nox:maybe-use-namespace (nox:sax-producer-consumer self) prefix uri))


;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS RDF-PARSER
;;;

(defclass rdf-parser (nox:sax-consumer)
  ((base
    :initform nil
    :accessor parser-base)
   (db
    :initarg :db
    :initform (make-instance 'indexed-db :emptyp t)
    :reader parser-db)
   (states
    :initform nil
    :accessor parser-states)
   (literal
    :accessor parser-literal)
   (rdfcorep 
    :initarg rdfcorep
    :initform t
    :accessor parser-rdfcore-p)
   (harvest-namespaces-p
    :initarg :harvest-namespaces-p
    :initform t
    :reader parser-harvest-namespaces-p)
   (initial-state
    :initarg :initial-state
    :initform :scan
    :reader parser-initial-state))
  (:default-initargs
    :producer (make-instance 'nox:xml-parser
			     :consumer (make-instance 'rdf-syntax-normalizer))))

(define-condition close-rdf-element (condition)
  ())

(defstruct (state (:constructor make-state (mode
                                            &optional node
                                                      property
                                                      statement-id
                                                      language
                                                      datatype)))
  mode
  node
  property
  (statement-id nil)
  (language nil)
  (datatype nil)
  (index 0)
  task-queue)

#+:junk
(defmethod print-object ((state state) stream)
  (print-unreadable-object (state stream :type t)
    (format stream "~A ~S/~S/~A"
            (let ((mode (state-mode state)))
              (case mode
                ((:description :property :scan :literal)
                 (char (string mode) 0))
                (t mode)))
            (state-node state)
            (state-property state)
            (state-statement-id state))))

(defmethod add-state ((parser rdf-parser) mode &rest args)
  (declare (dynamic-extent args))
  (push (apply #'make-state mode args) (parser-states parser)))

(defmethod parser-task-state ((parser rdf-parser))
  (find :description (parser-states parser) :key #'state-mode))

(defmethod nox:sax-consumer-mode ((parser rdf-parser))
  (state-mode (first (parser-states parser))))

(defstruct (task (:constructor make-task (type node &rest parameters)))
  type
  node
  parameters)

(defmacro task-parameter (task parameter)
  `(getf (task-parameters ,task) ,parameter))

(defmethod defer-task ((parser rdf-parser) type node &rest args)
  (declare (dynamic-extent args))
  (pushnew (apply #'make-task type node args)
           (state-task-queue (parser-task-state parser))
           :test #'(lambda (p q)
                     (and (eq (task-type p) (task-type q))
                          (eq (task-node p) (task-node q))))))

(defmethod make-container ((parser rdf-parser)
                           elements
                           &optional container-uri
                                     (container-type-uri -rdf-bag-uri-))
  (let ((node (ensure-node parser container-uri nil))
	(i 0))
    (add-as-triple parser node
		   (ensure-node parser -rdf-type-uri- t)
		   (ensure-node parser container-type-uri t))
    (dolist (element elements)
      (add-as-triple parser node (index-uri (incf i)) element))
    node))

(defmethod initialize-instance :after ((parser rdf-parser) &key &allow-other-keys)
  (let ((normalizer (nox:sax-producer-consumer (nox:sax-consumer-producer parser))))
    (setf (nox:sax-producer-consumer normalizer) parser)))

(defmethod uri ((parser rdf-parser) uri should-exist-p)
  (let* ((base (first (parser-base parser)))
         (ends-in-hash-p (char= (char base (1- (length base))) #\#)))
    (cond ((or (null uri) (find #\: uri :test #'char=) (char= (char uri 0) #\/))
           uri)
          ((char= (char uri 0) #\#)
           (concatenate 'string base (if ends-in-hash-p (subseq uri 1) uri)))
          ((not should-exist-p)
           (concatenate 'string base (if ends-in-hash-p nil "#") uri))
          (t
           uri))))

(defmethod ensure-node ((parser rdf-parser) uri should-exist-p)
  (if (and (stringp uri) (zerop (length uri)))
    (node (first (parser-base parser))) ; should it be something that does not change?
    (node (uri parser uri should-exist-p))))

(defmethod nox:parse ((parser rdf-parser) stream locator)
  (catch :terminate-rdf-parser
    (nox:parse (nox:find-first-producer parser) stream locator))
  (node (first (parser-base parser))))

(defmethod add-as-triple ((parser rdf-parser)
                          (subject node)
                          (predicate string)
                          object
                          &optional statement-id)
  (add-as-triple parser subject (ensure-node parser predicate t) object statement-id))

(defmethod add-as-triple ((parser rdf-parser)
                          (subject node)
                          (predicate node)
                          object
                          &optional statement-id)
  (let* ((db (parser-db parser))
         (source (node (first (parser-base parser))))
	 (triple (db-make-triple db subject predicate object source)))
    (db-add-triple db triple)
    (dolist (state (parser-states parser))
      ;; for higher order statements
      (dolist (task (state-task-queue state))
        (when (and (eq (task-type task) :bagid)
                   (eq subject (task-node task)))
          (push (cons triple statement-id) (task-parameter task :statements))
          (return-from add-as-triple triple))))
    (when statement-id
      ;; no bagid but statement-id exists
      (db-reify triple db (uri parser statement-id nil) source))
    triple))

(defun new-index-uri (parser)
  (index-uri (incf (state-index (first (parser-states parser))))))

(defun parse-db-from-stream (stream locator
                             &rest options
                             &key (parser-class 'rdf-parser)
                             &allow-other-keys)
  (declare (dynamic-extent options))
  (remf options :parser-class)
  (multiple-value-bind (source-node parser)
		       (apply #'nox:parse-from-stream
			      stream locator parser-class options)
    (values (parser-db parser) source-node)))

(defmethod nox:maybe-use-namespace ((self rdf-parser) prefix uri)
  (when (and (parser-harvest-namespaces-p self)
             (not (nox:string-dict-get (dictionary-namespaces *nodes*) prefix)))
    (add-namespace prefix uri)))


;;; --------------------------------------------------------------------------------------
;;;
;;;   RDF PARSER STATE MACHINE TRANSITIONS
;;;

(defmethod nox:start-document ((parser rdf-parser)
                               locator)
  (setf (parser-base parser) (list locator))
  (add-state parser (parser-initial-state parser)))

(defmethod nox:end-document ((parser rdf-parser)
                             mode)
  (declare (ignore mode))
  nil)

(defmethod nox:start-element :before ((parser rdf-parser)
                                      (tag nox:open-tag)
                                      mode)
  (declare (ignore mode))
  (push (nox::tag-base tag) (parser-base parser)))

(defmethod nox:end-element :after ((parser rdf-parser)
                                   (tag nox:open-tag)
                                   mode)
  (declare (ignore mode))
  (pop (parser-base parser)))

(defmethod nox:start-element ((parser rdf-parser)
                              (tag nox:open-tag)
                              (mode (eql :scan)))
  (cond ((string= (nox:token-string tag) -rdf-rdf-uri-)
         (add-state parser :description))
        ((string= (nox:token-string tag) -rdf-description-uri-)
         (nox:start-element parser tag :description))))

(defmethod nox:start-element ((parser rdf-parser)
                              (tag nox:open-tag)
                              (mode (eql :description)))
  (let* ((each  (nox:tag-attribute tag -rdf-abouteach-uri-))
         (about (nox:tag-attribute tag -rdf-about-uri-))
         (id    (nox:tag-attribute tag -rdf-id-uri-))
         (type  (nox:token-string tag))
         (node  (ensure-node parser
                             (and (null each) (or about id))
                             (and (null each) (null id)))))
    (when (and about id)
      (cerror "Use \"about\"" 'about-and-id-both-present))
    (if each
      (if (parser-rdfcore-p parser)
        (cerror "Ignore \"aboutEach\"" 'feature-disabled :feature "aboutEach")
        (defer-task parser :abouteach node :target (ensure-node parser each t)))
      (let* ((bagid (nox:tag-attribute tag -rdf-bagid-uri-))
             (state (first (parser-states parser)))
             (parent (state-node state)))
        (when bagid
          (defer-task parser :bagid node :bagid bagid :statements nil))
        (when parent
          (attach-to-parent parser parent node (state-statement-id state)))))
    (unless (string= type -rdf-description-uri-)
      (add-as-triple parser node -rdf-type-uri- (ensure-node parser type t)))
    (add-state parser :property node)))

(defmethod attach-to-parent ((parser rdf-parser)
                             (parent node)
                             (child node)
                             &optional statement-id)
  (let ((state (first (parser-states parser))))
    (cond ((not (eq (state-mode state) :collection))
           (add-as-triple parser parent
                          (state-property (first (parser-states parser)))
                          child statement-id))
          (t
           (setf parent (state-node state))
           (add-as-triple parser parent -rdf-first-uri- child)
           (add-as-triple parser parent -rdf-rest-uri-
                          (setf (state-node state) (ensure-node parser nil t)))))))
    
(defmethod nox:start-element ((parser rdf-parser)
                              (tag nox:open-tag)
                              (mode (eql :property)))
  (let* ((state (first (parser-states parser)))
         (node (state-node state))
         (property-uri (nox:token-string tag))
         (property (ensure-node parser
                                (cond ((string= property-uri -rdf-li-uri-)
                                       ;; (defer-task parser :container node)
                                       (new-index-uri parser))
                                      (t
                                       property-uri))
                                t))
         (resource-uri (nox:tag-attribute tag -rdf-resource-uri-))
         (statement-id (nox:tag-attribute tag -rdf-id-uri-)))
    (if resource-uri
      (let ((value (ensure-node parser resource-uri t)))
        (setf (state-property state) property)
        (attach-to-parent parser node value statement-id)
        (add-state parser :property value))
      (parse-using-parsetype parser node property
                             (nox:tag-attribute tag -rdf-parsetype-uri-)
                             statement-id
                             (nox:tag-attribute tag -xml-lang-attr-)
                             (nox:tag-attribute tag -rdf-datatype-uri-)))))

(defmethod parse-using-parsetype ((parser rdf-parser) node property parsetype
                                  &optional statement-id language datatype)
  (cond ((null parsetype)
         (add-state parser :description node property statement-id language datatype))
        ((string= parsetype "Literal")
         (setf (parser-literal parser) nil)
         (add-state parser :literal node property))
        ((string= parsetype "Resource")
         (add-as-triple parser
                        node property (setf node (ensure-node parser nil t))
                        statement-id)
         (add-state parser :property node))
        ((string= parsetype "Collection")       ; adapted from daml-parser
         (let ((list-node (ensure-node parser nil t)))
           (add-as-triple parser node property list-node)
           (add-state parser :collection list-node)))
        (t
         (cerror "Ignore parseType" 'unknown-parsetype :thing parsetype))))
  
(defmethod nox:start-element ((parser rdf-parser)
                              (tag nox:open-tag)
                              (mode (eql :literal)))
  (add-state parser :literal)
  (push tag (parser-literal parser)))

(declaim (special *db*))

(defmethod nox:end-element ((parser rdf-parser)
                            (tag nox:open-tag)
                            (mode (eql :literal)))
  (let ((state (first (parser-states parser))))
    (call-next-method)
    (cond ((not (null (state-node state)))
	   (let ((string (with-output-to-string (s)
			   (nox:replay (make-instance 'nox:xml-formatter :stream s)
				       (nreverse (parser-literal parser))))))
	     (add-as-triple parser
			    (state-node state)
			    (state-property state)
			    (db-make-literal (or *db* (parser-db parser)) string))))
	  ((not (nox:tag-empty-p tag))
	   (push (nox:tag-counterpart tag) (parser-literal parser))))))

(defmethod nox:end-element ((parser rdf-parser)
                            (tag nox:open-tag)
                            (mode (eql :scan)))
  nil)

(defmethod nox:end-element :after ((parser rdf-parser)
                                   (tag nox:open-tag)
                                   (mode (eql :property)))
  (let ((state (parser-task-state parser)))
    (when state
      (dolist (task (shiftf (state-task-queue state) nil))
	(execute-deferred-task parser task (task-type task))))))

(defmethod execute-deferred-task ((parser rdf-parser) task type)
  (let ((db (parser-db parser))
	(source (node (first (parser-base parser)))))
    (ecase type
      ;; (:container
      ;;  (is-container-p db (task-node task) t)
      ;;  t)
      (:abouteach
       (let ((target (task-parameter task :target))
	     (index-predicates nil))
	 (is-container-p db target t)
	 (dolist (triple (db-query db target nil nil))
	   (let ((uri (node-uri (triple-predicate triple))))
	     (when (find uri *index-uris* :test #'string=)
	       (push (ensure-node parser uri t) index-predicates))))
	 (dolist (triple (db-query db (task-node task) nil nil))
	   (db-del-triple db triple)
	   (dolist (p index-predicates)
	     (add-as-triple parser
			    (triple-object (first (db-query db target p nil)))
			    (triple-predicate triple)
			    (triple-object triple)
			    source)))))
      (:bagid
       (let ((statements (task-parameter task :statements)))
	 (when statements
	   (make-container parser
			   (mapcar #'(lambda (s)
				       (destructuring-bind (triple . id) s
					 (db-reify triple db
						   (and id (uri parser id nil))
						   source)))
				   statements)
			   (uri parser (task-parameter task :bagid) nil))))))))

(defmethod nox:end-element :after ((parser rdf-parser)
                                   (tag nox:open-tag)
                                   (mode (eql :description)))
  (when (string= (nox:token-string tag) -rdf-rdf-uri-)
    (signal 'close-rdf-element)))

(defmethod nox:end-element ((parser rdf-parser)
                            (tag nox:open-tag)
                            mode)
  (declare (ignore mode))
  (pop (parser-states parser)))

(defmethod nox:char-content ((parser rdf-parser)
                             (content string)
                             (mode (eql :description)))
  (let* ((state (first (parser-states parser)))
         (datatype (state-datatype state)))
    (add-as-triple parser
                   (state-node state)
                   (state-property state)
                   (db-make-literal (or *db* (parser-db parser)) content
				    :language (state-language state)
				    :datatype (and datatype (node datatype)))
                   (state-statement-id state))))

(defmethod nox:char-content ((parser rdf-parser)
                             (content string)
                             (mode (eql :literal)))
  (push content (parser-literal parser)))

(defmethod nox:char-content ((parser rdf-parser)
                             (content string)
                             (mode (eql :scan)))
  ;; ignore character content in :scan mode
  nil)

(defmethod nox:char-content ((parser rdf-parser)
                             (content string)
                             mode)
  (declare (ignore mode))
  (cerror "Ignore" 'illegal-character-content :thing content))

(defmethod nox:start-element ((parser rdf-parser)
                              (tag nox:open-tag)
                              (mode (eql :collection)))
  (nox:start-element parser tag :description))

(defmethod nox:end-element :before ((parser rdf-parser)
                                    (tag nox:open-tag)
                                    (mode (eql :collection)))
  (let* ((db (parser-db parser))
         (node (state-node (first (parser-states parser))))
         (triple (db-del-triple db (first (db-query db nil nil node)))))
    (add-as-triple parser
                   (triple-subject triple)
                   (triple-predicate triple)
                   (ensure-node parser -rdf-nil-uri- t))))
