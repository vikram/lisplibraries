;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLOUCHDB; Base: 10 -*-

;;; Copyright (c) 2007 Peter Eddy. All rights reserved.

;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:

;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.

;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

(in-package :clouchdb)

(defvar *host* "localhost" "CouchDb server host name")
(defvar *port* "5984" 
  "As of version 7.2, the IANA assigned CouchDb port (was 8888)")
(defvar *db-name* "default" "Default database name")
(defvar *protocol* "http" "http or https")
(defvar *document-update-fn* nil)
(defvar *document-fetch-fn* nil)

(defvar *text-types* 
  '(("text" . nil) 
    (nil . "json") 
    (nil . "xml"))
  "Defined to instruct Drakma to treat json responses as text")

(defparameter *temp-db-counter* 0 "Used in the creation of temporary databases")

(defmacro define-constant (name value &optional doc)
  "A version of DEFCONSTANT for /strict/ CL implementations."
  ;; See <http://www.sbcl.org/manual/Defining-Constants.html>
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

(define-constant +utf-8+ (make-external-format :utf-8 :eol-style :lf)
  "Default external format for document content.")

(defun document-update-notify (fn doc)
  "Optionally invoke specified function with supplied document, used
  to invoke user-specified hook functions."
  (if fn (funcall fn doc) doc))

;;
;; URL Parameter helpers
;;

(defun true-if-true (value)
  "Return \"true\" if value is non-nil, otherwise nil"
  (when value "true"))

(defun false-if-false (value)
  "Return \"false\" if value is nil, otherwise nil"
  (unless value "false"))

(defvar *view-options*
  '((:key . ((:name . "key") (:fn . doublequote)))
    (:start-key . ((:name . "startkey") (:fn . doublequote)))
    (:start-key-docid . ((:name . "startkey_docid") (:fn . doublequote)))
    (:end-key . ((:name . "endkey") (:fn . doublequote)))
    (:count . ((:name . "count") (:fn . value-as-string)))
    (:update . ((:name . "update") (:fn . false-if-false)))
    (:descending . ((:name . "descending") (:fn . true-if-true)))
    (:skip . ((:name . "skip") (:fn . value-as-integer))))
  "Definitions for how invoke-view keyword parameters are translated
  into CouchDb parameters")

(defun transform-param (param value table)
  "Use a keyword transformation table to traslate between function
  keyword parameter names and values, and URL parameter names and
  values."
  (let ((transf (cdr (assoc param table))))
    (when transf
      (let ((value (funcall (cdr (assoc :fn transf)) value)))
        (when value
          (cons (cdr (assoc :name transf)) value))))))

(defun transform-params (keyword-params options)
  "Transform each keyword parameter using the specified set of
options, use only those transformations that return a non-nil result."
  (loop for param on keyword-params by #'cddr
     when (transform-param (first param) (second param) options)
     collect it))

;;
;; Conditions
;;

(define-condition db-existential-error (error)
  ((text :initarg :uri :reader uri)
   (db-name :initarg :db-name :reader db-name)
   (result :initarg :result :reader result)))

(define-condition db-does-not-exist (db-existential-error) 
  ()
  (:report (lambda (condition stream)
	     (format stream "Database \"~A\" at \"~A\" does not exist" 
		     (db-name condition)
		     (uri condition)))))

(define-condition db-already-exists (db-existential-error) 
  ()
  (:report (lambda (condition stream)
	     (format stream "Database \"~A\" at \"~A\" already exists" 
		     (db-name condition)
		     (uri condition)))))

(define-condition doc-error (error) 
  ((text :initarg :uri :reader text)
   (reason :initarg :reason :reader reason)
   (id :initarg :id :reader id))
  (:report (lambda (condition stream)
	     (format stream "Reason \"~A\", Document ID: \"~A\""
		     (reason condition)
		     (id condition)))))
  
(define-condition id-or-revision-conflict (doc-error) 
  ()
  (:report (lambda (condition stream)
	     (format stream "ID or Revsion Conflict. ID=\"~A\", Reason=~S"
		     (id condition) (reason condition)))))

(define-condition id-missing (doc-error)
  ()
  (:report (lambda (condition stream) 
             (declare (ignore condition))
             (format stream "No ID specified"))))

(define-condition document-missing (doc-error) 
  ()
  (:report (lambda (condition stream)
	     (format stream "No document found for ID=\"~A\""
		     (id condition))))
  (:documentation "Error raised when no document matching ID is found"))

;;
;; Unexported utility functions
;;

(defun value-as-string (value)
  (cond ((numberp value)
	 (write-to-string value))
	((stringp value)
	 value)))

(defun value-as-integer (value)
  (cond ((numberp value)
         value)
        ((stringp value)
         (parse-integer value))))

(defun string-join (list &optional (delim #\,)) 
  "Join a bunch of strings into one with a delimiter. Implementation
by Russel McManus on c.l.l."
  (unless list (return-from string-join "")) 
  (let ((out (make-array (1- (loop for s in list sum (1+ (length s)))) 
                         :element-type 'character)) 
        (start (length (car list)))) 
    (setf (subseq out 0 (length (car list))) (car list)) 
    (dolist (s (cdr list)) 
      (setf (aref out start) delim 
            (subseq out (+ 1 start) (+ 1 start (length s))) s 
            start (+ 1 start (length s)))) 
    out))

(defmacro cat (&rest rest)
  "Shorthand for (concatenate 'string)"
  `(concatenate 'string ,@rest))

(defun doublequote (value)
  "Wrap specified value in double quotes."
  (cat "\"" value "\""))

(defun convert-encoding (string encoding)
  "Convert string to specified encoding. This may be totally wrong and
probably way too inefficient, but it seems to work."
  (octets-to-string (string-to-octets string :external-format encoding)))

(defun url-encode (string &key (external-format +utf-8+))
  "URL-encode a string."
  (with-output-to-string (s)
    (loop for c across (convert-encoding string external-format)
          do (cond ((or (char<= #\0 c #\9)
                        (char<= #\a c #\z)
                        (char<= #\A c #\Z)
                        (find c "$-_.!*'()," :test #'char=))
                     (write-char c s))
                   ((char= c #\Space)
                     (write-string "%20" s))
                   (t (format s "%~2,'0x" (char-code c)))))))

(defun make-uri (&rest rest)
  "Return a URI containing *protocol*://*host*:*port*/ and the
concatenation of the remaining parameters."
  (concatenate 'string *protocol* "://" *host* ":" *port* "/"
	       (apply #'concatenate 'string rest)))

(defun keyword-to-special (key)
  "Convert a keyword symbol to a special symbol. For example,
  convert :db-name to *db-name*"
  (intern (cat "*" (string-upcase (symbol-name key)) "*")))

(defmacro ensure-db ((&key (db-name nil db-name-p)) &body body)
  "Warp request in code to check for errors due to non-existant data
bases. Since in a document operation, CouchDb does not distinguish
between an error due to a missing document and a missing database."
  (let ((result (gensym)))
    `(let ((,result (progn ,@body)))
       (when (equal "not_found" (document-property :|error| ,result))
	 (let ((dbn (if ,db-name-p ,db-name *db-name*)))
	   (if (document-property :|error|  (get-db-info :db-name dbn))
	       (error 'db-does-not-exist
		      :result ,result :db-name dbn
		      :uri (make-uri dbn)))))
       ,result)))

(defun document-as-hash (doc)
  "Convert a document to a hashtable if it isn't one already. Document
  should be in the form of an associative list."
  (cond ((not (hash-table-p doc))
	 (let ((new-doc (make-hash-table)))
	   (dolist (e doc)
	     (setf (gethash (car e) new-doc) (cdr e)))
	   new-doc))
	(t doc)))

(defun doc-as-alist (doc)
  "Convert a document in the form of a hash table into an associative
  list"
  (cond ((hash-table-p doc)
	 (let ((new-doc))
	   (maphash #'(lambda (k v) (push (cons k v)  new-doc)) doc)
	   new-doc))
	(t doc)))

(defun as-keyword-symbol (value)
  "Return value in a form that would be used to identify the car of a
value in a document. For example, a value"
  (cond ((stringp value)
         (intern value "KEYWORD"))
        ((keywordp value)
         value)
        ((symbolp value)
         (as-keyword-symbol (intern (symbol-name value) "KEYWORD")))
        (t value)))

(defun as-field-name-string (value)
  "Convert a case-encoded symbol to a potentially mixed case string."
  (cond ((symbolp value)
         (symbol-name value))
        (t value)))

(defun document-property (name doc)
  "Get the value associated with the document property or nil if there
is no associated value. Note that name may be either a keyword symbol,
a regular symbol or a string."
  (let ((name (as-keyword-symbol name)))
    (cond ((hash-table-p doc)
           (gethash name doc))
          (t (cdr (assoc name doc))))))

(defun (setf document-property) (value name doc)
  "Allows setting of existing document properties in
place (destructively)."
  (let ((name (as-keyword-symbol name)))
    (cond ((hash-table-p doc)
           (setf (gethash name doc) value))
          (t (rplacd (assoc name doc) value)))
    doc))

;; (defun set-document-property (doc name value)
;;   "Set a property of a document. If the named property does not exist,
;; add it to the document, otherwise change the existing value.  Does not
;; destructively modify input document, so be sure to use return value."
;;   (let ((doc (copy-tree doc)))
;;     (if (assoc name doc)
;;         (setf (document-property name doc) value)
;;         (cons `(,(as-keyword-symbol name) . ,value) doc))))

(defun set-document-property (doc &rest args)
  "Set a property of a document. If the named property does not exist,
add it to the document, otherwise change the existing value.  Does not
destructively modify input document, so be sure to use return value."
  (let ((doc (copy-tree doc)))
    (loop for (name value) on args by #'cddr
       do (if (assoc name doc)
              (setf (document-property name doc) value)
              (setf doc (cons `(,(as-keyword-symbol name) . ,value) doc))))
    doc))

(defun document-id (doc)
  "Shortcut for getting the ID from the specified document. First
  checks for :|_id| property, then :|id|"
  (or (document-property :|_id| doc)
      (document-property :|id| doc)))

(defun query-document (query doc)
  "Return a list of all values in the document matching the query. For
example, given the document:

  ((:values (((:a . 1) (:b . 2)) ((:a . 3) (:b . 4)))))

the query string '(:values :a) will return (3 1), i.e. the value of
both :a associations. 

One special query input value is :* which is a 'wildcard'. With the
document described above the query '(:values :*) will return (4 3 2
1), or the values of all associations directly below :values. The
query '(:* :*) on this document will also return (4 3 2 1).

Another special query input value is :**, which recursively matches
the next query input. For example, with the following document:

  ((:level1 . ((:level2 . (((:level3 . 1)))))))

The query '(:** :level3) will return (1), the value
of :level3. Finally, functions can specified in the query. Functions
are called with the portion of the document matched to the previous
query element and can either return the document, return a different
document or null."
  (let ((res))
    (labels ((q (query doc rec)
;;               (format t "~%test: r=~s, query=~s doc=~s~%" rec query doc)
               (cond ((null doc)
                      nil)
                     ((null query)
                      (push doc res))
                     ((eq :** (car query))
                      (q (cdr query) doc t))
                     ((and (listp query) (eq :** (car query)))
;;                      (format t "action: :**~%")
                      (q (cdr query) doc t))
                     ((assoclp doc)
;;                      (format t "action: assoclp doc=~s ~%" doc)
                      (dolist (e doc)
                        (q query e rec)))
                     ((functionp (car query))
;;                      (format t "action: functionp~%")
                      (q (cdr query) (funcall (car query) doc) rec))
                     ((keyword-assocp doc) 
;;                       (format t "action: keyword-assocp doc=~S~%" doc)
                       (cond ((or (eq (car query) (car doc)) (eq :* (car query)))
;;                              (format t "action: keyword asscoc=t~%" doc)
                              (q (cdr query) (cdr doc) nil))
                             ((and rec (listp (cdr doc)))
                              (q query (cdr doc) t))))
                     ((listp doc)
;;                      (format t "action: listp~%")
                      (dolist (e doc)
                        (q query e rec)))
                     (t nil))))
      (q query doc nil)
      res)))

;;
;;
;;

(defun db-request (uri &rest args &key &allow-other-keys)
  "Used by all Couchdb APIs to make the actual REST request."
  ;;(format t "uri: ~S~% args: ~S~%" (make-uri uri) args)
  (let ((*text-content-types* *text-types*))
    (multiple-value-bind (body status headers uri stream must-close reason-phrase)
	(apply #'drakma:http-request (make-uri uri) args)
      (declare (ignore reason-phrase stream uri headers status))
;;      (format t "  -> uri: ~S~%" uri)
;;      (format t "  -> headers: ~S~%" headers)
      (cond (must-close
;;             (format t "body: ~S~%" body)
             (json-to-document body))
            (t nil)))))

;; (defun cached-db-request (cache uri &rest args &key parameters &allow-other-keys)
;;   "If a cache is supplied try it first before reqesting from
;; server. Cache result if cache is not nil."
;;   (cond (cache
;;          (let ((cache-key (if parameters (cons uri parameters) uri)))
;;            (format t "cache key: ~s~%" cache-key)
;;            (let ((cached (get-cached cache-key cache)))
;;              (cond (cached
;;                     cached)
;;                    (t
;;                     (setf (get-cached cache-key cache) (apply #'db-request uri args)))))))
;;         (t (apply #'db-request uri args))))

;;
;;
;;

(defun set-connection (&key (host nil host-p) (db-name nil db-name-p) 
		       (protocol nil protocol-p) (port nil port-p)
                       (document-update-fn nil document-update-fn-p)
                       (document-fetch-fn nil document-fetch-fn-p))
  "Set top-level connection information. The port may be specified as
a string or number. As of CouchDb version 7.2 the default port is
5984, prior to that it was 8888."
  (when host-p (setf *host* host))
  (when db-name-p (setf *db-name* db-name))
  (when port-p (setf *port* (value-as-string port)))
  (when protocol-p (setf *protocol* protocol))
  (when document-update-fn-p (setf *document-update-fn* document-update-fn))
  (when document-fetch-fn-p (setf *document-fetch-fn* document-fetch-fn))
  (values))

(defmacro with-connection ((&rest args &key db-name port protocol host
                                  document-update-fn document-fetch-fn)
			   &body body)
  "Execute body in the context of the optionally specified host,
db-name, port or protocol. Port may be a string or a number, protocol
is http or https. As of CouchDb version 7.2 the default port is 5984,
prior to that it was 8888."
  (declare (ignore db-name port protocol host document-update-fn document-fetch-fn))
  `(let (,@(loop for var on args 
              by #'cddr collect (list (keyword-to-special (car var)) (second var))))
     ,@body))

(defun document-properties (document)
  "Return the document properties, filtering out any couchdb reserved
properties (properties that start with an underscore)."
  (let ((props))
    (dolist (p document)
      (unless (equal "_" (subseq (symbol-name (car p)) 0 1))
	(push p props)))
    props))

;;
;; CouchDB Database Management API
;;

(defun list-dbs ()
  "Return a list of all databases for the current host and port."
  (db-request "_all_dbs" :method :get))

(defun create-db (&key (db-name nil db-name-p) (if-exists :fail))
  "Create database. If db-name is unspecified, uses *db-name*. If
database already exists an error condition is raised. This condition
can be avoided by specifying :ingore for if-exists. In this case no
error condition is generated. Specify :recreate to potentially delete
and create a new database."
  (let* ((name (if db-name-p db-name *db-name*))
	 (res (db-request (cat (url-encode name) "/") 
                          :method :put)))
    (if (equal "database_already_exists" (document-property :|error| res))
      (ecase if-exists
	((:ignore) (list (cons :|ok| t) (cons :|ignored| t)))
	((:recreate) 
	 (delete-db :db-name name) 
	 (create-db :db-name name))
	((:fail)
	 (restart-case
	     (error 'db-already-exists
		    :result res :db-name name
		    :uri (make-uri name))
	   (ignore () :report "Ignore error and continue" nil))))
      res)))

(defun delete-db (&key (db-name nil db-name-p) if-missing)
  "Delete database. If db-name is unspecified, deletes database named
in *db-name*. Normally deletion of non-existent databases generates an
error condition, but this can be avoided by specifying :ignore in the
if-missing parameter."
  (let* ((name (if db-name-p db-name *db-name*))
	 (res (db-request (cat (url-encode name) "/") :method :delete)))
    (if (and (document-property :|error| res) (not (eq :ignore if-missing)))
	(restart-case 
	    (error 'db-does-not-exist
		   :result res :db-name name
		   :uri (make-uri name))
	  (ignore () :report "Ignore error and continue" nil)))
    res))

(defun get-db-info (&key (db-name nil db-name-p))
  "Get information for named database, or couchdb server if no
database specified."
  (let ((dbn (if db-name-p db-name *db-name*)))
    (db-request (if dbn (cat (url-encode dbn) "/"))
		:method :get)))

(defun create-temp-db-name ()
  "Return a database name that's probably unique."  
  (concatenate 'string 
	       "temp-" 
	       (write-to-string (get-universal-time)) "-"
	       (write-to-string (incf *temp-db-counter*))))

(defun create-temp-db (&key (db-name-creator #'create-temp-db-name))
  "Create a temporary database."
  (let ((db-name (funcall db-name-creator)))
    (let ((res (create-db :db-name db-name)))
      (if (document-property :|error| res)
	  (error (format t "Error ~S creating database: ~A"
		       (document-property :|error| res) db-name))))
    db-name))

(defmacro with-temp-db (&body body)
  "Execute body in context of newly created, temporary
database. Delete database before return."
  (let ((temp-db-name (gensym))
	(result (gensym)))
    `(let* ((,temp-db-name (create-temp-db))
	    (,result (with-connection (:db-name ,temp-db-name)
		      ,@body)))
       (delete-db ,temp-db-name)
       ,result)))

;;
;; CouchDB Document Management API
;;

(defun get-all-documents (&key (descending nil))
  "Get a listing of all documents in a database. Listing is in
ascending ID order by default, or descending order of descending
parameter is non-nil."
  (ensure-db ()
    (db-request (cat (url-encode *db-name*) "/_all_docs") 
		:method :get
		:parameters (if descending
				;; ?descending=false causes error ATM
				`(("descending" . "true"))))))

(defun get-document (id &key revision revisions revision-info (if-missing nil if-missing-p))
  "Get a document by ID. Returns nil if the document does not exist.
The revision property specifies an optional revision number, if
unspecified, the most recent revision is returned. The revisions and
revision-info parameters, if non-nil, request revision information
about the document instead of the actual document contents. The
revision-info option contains more revision information than
revisions. All revision* options are mutually exclusive, specify only
one."
  (unless id
    (error 'id-missing))
  (let ((parameters))
    (when revision
      (push (cons "rev" (value-as-string revision)) parameters))
    (when revisions
      (push (cons "revs" "true") parameters))
    (when revision-info
      (push (cons "revs_info" "true") parameters))
    (let ((res (ensure-db () (db-request (cat (url-encode *db-name*) "/" 
                                              (url-encode id))
					 :method :get 
					 :parameters parameters))))
      (if (document-property :|error| res)
          (progn
            (cond ((eq if-missing :ignore)
                   nil)
                  ((and if-missing-p (not (eq if-missing :error)))
                   if-missing)
                  (t (error 'document-missing :id id))))
	  (document-update-notify *document-fetch-fn* res)))))
      
		      
(defun encode-file (file)
  ""
  (with-output-to-string (out)
    (with-open-file (in file)
      (let ((data (make-array (file-length in) :element-type '(unsigned-byte 8))))
        (with-open-file (stream file :element-type '(unsigned-byte 8))
          (read-sequence data stream)
          (s-base64:encode-base64-bytes data out nil))))))

(defun encode-attachements (attachments)
  (let ((encoded))
    (when attachments
      (dolist (a attachments)
        (format t "file name: ~S~%" (car a))
        (let ((e (encode-file (car a))))
          (when e
            (push `(,(as-keyword-symbol (second a)) . 
                     ((:|type| . "base64")
                      (:|data| . ,e)))
                  encoded))))
      `(:|_attachments| . ,encoded))))

;; (defun update-document-cache (url)
;;   "Called when a document has been updated on the server. Used for
;; clearing associated cache data and firing notification functions."
;;   (when *document-cache*
;;     (format t "removing cached document: ~s~%" url)
;;     (remove-cached url *document-cache*)))

(defun put-document (doc &key id attachments)
  "Create a new document or update and existing one. If the document
is new an ID must be specified (but see post-document). If the
document has been fetched from the server (and still has its :_id
property) then no ID need be specified. If an ID is specified and it
differs from the existing :_id value, then a new document is created
with the new ID and the non-special properties of the specified
document, since the latter would generate a CouchDb error."
  (let ((current-id (document-property :|_id| doc)))
    (cond ((not (or current-id id))
	   (error 'id-missing))
	  ;; If an ID was specified and that ID does not match the
	  ;; :_id property of the current document, strip the document
	  ;; of all special (CouchDb managed) properties, since these
	  ;; are specific to the current document. The presence of the
	  ;; ID parameter in this situation means 'create a new
	  ;; document with the same contents as the old one'.
	  ((and id current-id (not (equal current-id id)))
	   (setf doc (document-properties doc))))
    (when attachments
      (setf doc (cons (encode-attachements attachments) doc)))
    (let ((res (ensure-db () (db-request (cat (url-encode *db-name*) "/" 
                                              (url-encode (if id id current-id)))
                                         :content-type "text/javascript"
                                         :external-format-out +utf-8+
                                         :content-length nil
                                         :content (document-to-json 
                                                   (document-update-notify 
                                                    *document-update-fn* doc))
                                         :method :put))))
      (when (document-property :|error| res)
        (error (if (equal "conflict" (document-property :|error| res)) 
                   'id-or-revision-conflict 'doc-error)
               :id (if id id current-id)
               :reason (document-property :|reason| res)))
      res)))

(defun post-document (doc)
  "Post the document to the server, creating a new document. An
existing _id in the document will be ignored, the server will create a
new document and assign a new ID. Therefore this is an easy method for
copying documents. The return value includes the document ID in
the :ID property."
  (let* ((url (cat (url-encode *db-name*) "/"))
         (res (ensure-db ()
                (db-request url
                           :content-type "text/javascript"
                           :external-format-out +utf-8+
                           :content-length nil
                           :content (document-to-json 
                                     (document-update-notify *document-update-fn* doc))
                           :method :post))))
    (when (document-property :|error| res)
      (error 'doc-error) :id nil :reason (document-property :|reason| res))
    res))

(defun create-document (doc &key id attachments)
  "Create a new document, optionally specifying the new document
ID."
  (if id
      (put-document doc :id id :attachments attachments)
      (post-document doc)))

(defun bulk-document-update (docs)
  "Update multiple documents in a single request. The docs parameter
should be a list of documents."
  (ensure-db () 
    (db-request (cat (url-encode *db-name*) "/_bulk_docs")
		:method :post
                :content-type "application/xml"
                :external-format-out +utf-8+
                :content-length nil
		:content 
                (cat "[ " 
		     (string-join (mapcar #'document-to-json docs)) 
		     " ] "))))

(defun delete-document (&key document id revision if-missing)
  "Delete a revision of a document. If the id parameter is provided
but not the revision, the current document will be fetched and it's
revision number will be used for the delete. If specified, the
document parameter must include the CouchDb special properties :|_id|
and :|_rev|. At most one revision of the document will be deleted."
  (labels ((del (id rev)
             (let ((res (ensure-db () 
                          (db-request 
                           (cat (url-encode *db-name*) "/" (url-encode id)
                                "?rev=" (url-encode (value-as-string rev)))
                           :method :delete))))
               (when (document-property :|error| res)
                 (error 'doc-error) :id id :reason (document-property :|reason| res))
               res)))
    (cond ((not (null document))
	   (delete-document :id (document-property :|_id| document)
			    :revision (document-property :|_rev| document)
                            :if-missing if-missing))
	  ((not id)
	   (error 'id-missing))
	  ((not revision)
           (let ((doc (get-document id :if-missing (if (eq if-missing :ignore) 
                                                       :ignore
                                                       :error))))
             (when doc
               (del id (document-property :|_rev| doc)))))
	  (t (del id revision)))))

;;
;; Views API
;;

(defun ad-hoc-view (view &rest options &key key start-key start-key-docid
                    end-key count update descending skip)
  "Execute query using an ad-hoc view."
  (declare (ignore key start-key start-key-docid end-key count 
                   update descending skip))
  (ensure-db ()
    (db-request (cat (url-encode *db-name*) "/_temp_view")
		:method :post
                :external-format-out +utf-8+
		:content-type "text/javascript"
                :content-length nil
                :parameters (transform-params options *view-options*)
		:content view)))

(defun create-view (id &rest view-defs)
  "Create one or more views in the specified view document ID."
  (labels ((mk-view-js (views)
	     (cond ((null views)
		    nil)
		   (t
		    (let ((v (car views)))
		      (cat "\"" (car v) "\" : \"" (cdr v) "\""
			   (if (not (null (cdr views))) ", ")
			   (mk-view-js (cdr views))))))))
    (ensure-db ()
      (db-request (cat (url-encode *db-name*) "/_design/" (url-encode id))
		  :method :put
                  :external-format-out +utf-8+
                  :content-type "text/javascript"
                  :content-length nil
		  :content
		  (cat "{\"language\" : \"text/javascript\"," 
		       "\"views\" : {" (mk-view-js view-defs) "}}")))))

(defun delete-view (id &key revision if-missing)
  "Delete identified view document"
  (ensure-db ()
    (delete-document :id (cat "_design/" (url-encode id)) 
                     :revision revision :if-missing if-missing)))

(defun invoke-view (id view &rest options &key key start-key start-key-docid
                    end-key count update descending skip)
  "Invoke a view by specifiying the document ID that contains the view
and the name of the contained view. The key parameter specifies an
optional value to match against the view's mapped field. The start-key
and end-key values specify the optional begin and end range of the
mapped field(s) of each document to return. If descending is t,
returns results in reverse order. If update is t, does not refresh
view for query, use for higher performance but possible data
inconsistency."
  (declare (ignore key start-key start-key-docid end-key count 
                   update descending skip))
  (ensure-db ()
    (db-request (cat (url-encode *db-name*) "/_view/" 
                     (url-encode id) "/" (url-encode view))
		:method :get
                :parameters (transform-params options *view-options*))))
