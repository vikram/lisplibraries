(in-package :araneida)

;;;; Handle URLs in a CLOS object way, after the pattern of PATHNAMEs
;;;; See also url-class.lisp

;;; TODO:
;;; - clean it up - it's a mess
;;; - print URLs in #u format
;;; - Tidy for non-HTTP schemes someday too

;;; This could usefully all be seriously revisited in the light of
;;; rfc2396, which I haven't read yet but looks at a glance to be
;;; impressively more sensible than 1738

(defvar *default-url-defaults* nil
  "Default URL to use as context when PARSE-URL is given a relative urlstring")

(defmethod make-load-form ((url url) &optional environment)
  (make-load-form-saving-slots url :environment environment))

;;; basic URL about which we know little

(defgeneric url-p (url)
  (:documentation "Predicate determining if url is a url or not."))
(defmethod url-p ((url url)) #+nil (declare (ignore url)) t)
(defmethod url-p ((anything t)) #+nil (declare (ignore anything)) nil)

(defgeneric parse-url (url)
  (:documentation "Creates a url instance from a general url string, such as 'http://example.com:8080'"))
(defmethod parse-url ((url url))
  (destructuring-bind (scheme rest) (split (url-unparsed url) 2 '(#\:))
    (setf (url-scheme url) scheme
          (url-unparsed url) rest)))

(defmethod parse-url ((url mailto-url))
  (let ((unparsed (second (split (url-unparsed url) 2 '(#\:)))))
    (destructuring-bind (user host) (split unparsed 2 '(#\@))
      (setf (url-username url) user
            (url-host url) host)
      url)))

(defgeneric copy-url (url &optional extra-slots)
  (:documentation "Makes a copy of a URL object"))
(defmethod copy-url ((url url) &optional extra-slots)
  (let* ((class (class-of url))
         (new (make-instance class)))
    (loop for i in (append extra-slots '(string unparsed scheme))
          when (slot-boundp url i)
            do (setf (slot-value new  i) (slot-value url i)))
    new))

(defmethod copy-url ((url mailto-url) &optional extra-slots)
  (call-next-method url (append extra-slots '(username host))))

(defmethod copy-url ((url internet-url) &optional extra-slots)
  (call-next-method url (append extra-slots '(username password host port))))

(defmethod copy-url ((url httplike-url) &optional extra-slots)
  (call-next-method url (append extra-slots '(path query fragment))))

(defgeneric url-equal-p (url1 url2)
  (:documentation "Checks if url1 equals url2."))
(defmethod url-equal-p ((url1 url) (url2 url))
  (and (eql (class-of url1) (class-of url2))
       (let ((class (class-of url1)))
         (loop for slot in  (class-slots class)
               for name = (slot-definition-name slot)
               always
               (or (eql name 'string) (eql name 'unparsed)
                   (equal (slot-value url1 (slot-definition-name slot))
                          (slot-value url2 (slot-definition-name slot))))))))

(defmethod url-equal-p ((u1 t) (u2 t))
  (and (eq u1 :wild) (eq u2 :wild)))

;;; internet-url

(defmethod parse-url ((url internet-url))
  (call-next-method url)                    ;parse bits that the parent knows
  (let* ((string (subseq (url-unparsed url) 2)) ; skip "//"
         (dir-s (position #\/ string))
         (at-s (position #\@ string :end dir-s))
         (colon1-s (position #\: string :end at-s))
         (colon2-s (if at-s (position #\: string :start (1+ at-s)) nil))
         (colon-s (if at-s colon2-s colon1-s)))
    (setf (url-unparsed url) (if dir-s (subseq string  dir-s) nil)
          (url-host url) (subseq string (1+ (or at-s -1)) (or colon-s dir-s))
          (url-port url)
            (if colon-s (parse-integer (subseq string (1+ colon-s) dir-s)))
          (url-username url) (if at-s (subseq string 0 (or colon1-s at-s)) nil)
          (url-password url) (if (and at-s colon1-s)
                                 (subseq string (1+ colon1-s) at-s) nil))))

(defgeneric url-endpoint (url)
  (:documentation "Returns \`hostname:port\' for this URL.  The colon and port
number are omitted if the port is the default for this URL class (not true in practice for HTTPS)"))
(defmethod url-endpoint ((url internet-url))
  (let ((default-port (url-port (make-instance (class-of url)))))
    (if (eql (url-port url) default-port)
        (url-host url)
      (s. (url-host url) ":" (princ-to-string (url-port url))))))

;;; httplike-url

(defun parse-http-path (url string)
  (let* ((frag-s (position #\# string :from-end t))
         (query-s (position #\? string :end frag-s :from-end t)))
    (setf (url-query url) (if query-s (subseq string (1+ query-s) frag-s) nil)
          (url-port url) (or (url-port url)
                             (url-port (make-instance (class-of url))))
          (url-fragment url) (if frag-s (subseq string (1+ frag-s)) nil)
          (url-path url)
          (or (subseq string 0 (or query-s frag-s)) "/"))))

  
(defmethod parse-url ((url httplike-url))
  (call-next-method url)                    ;parse bits that the parent knows
  (parse-http-path url (url-unparsed url))
  (setf (url-unparsed url) nil))        ;we've finished parsing

(defgeneric urlstring (url &key query-parameters)
  (:documentation "Returns a URL string from a url object (ie something like 'http://example.com')"))

;;; watch us assemble a URL backwards ...
;;; XXX half of this should be in the internet-url method

(defmethod urlstring ((url httplike-url) &key (query-parameters t))
  (let ((out '()))
    (awhen (url-fragment url) (push it out) (push "#" out))
    (and query-parameters
	 (awhen (url-query url) (push it out) (push "?" out)))
    (aif (url-path url) (push it out) (push "/" out))
    (let ((default-port (url-port (make-instance (class-of url)))))
      (unless (eql (url-port url) default-port)
	(push (princ-to-string (url-port url)) out) (push ":" out)))
    (awhen (url-host url) (push it out))
    (awhen (url-username url)
           (push "@" out)
           (awhen (url-password url)
                  (push it out)
                  (push ":" out))
           (push it out))
    (push "://" out)
    (push (url-scheme url) out)
    (apply #'concatenate 'string out)))

(defmethod urlstring ((url mailto-url) &key (query-parameters t))
  (declare (ignore query-parameters))
  (format nil "mailto:~A@~A" (url-username url)
          (url-host url)))

(defgeneric merge-url (url string)
  (:documentation "Merge a string onto a url. FIXME: needs serious clarification"))
(defmethod merge-url ((template httplike-url) string)
  (let ((url (copy-url template)))
    ;; Find the 'leftmost' bit present in string, and replace everything in
    ;; url to the right of that
    (cond ((zerop (length string)) url)
	  ((let ((colon (position #\: string))
		 (slash (position #\/ string)))
	     (and colon
		  (or (not slash)
		      (< colon slash))))
		 ;; XXX this is probably wrong if STRING is
		 ;; e.g. "foo.bar.com:80/" but I can't be bothered to
		 ;; figure it out now
	   (parse-urlstring string))
	  ((eql (elt string 0) #\/)
           (parse-http-path url string)
           url)
          ((eql (elt string 0) #\?)
           (let ((hash (position #\# string)))
             (setf (url-query url) (subseq string 1 hash))
             (setf (url-fragment url) (if hash (subseq string (1+ hash)) nil)))
	   url)
          ((eql (elt string 0) #\#)
           (setf (url-fragment url) (subseq string 1))
           url)
          (t ;; it's a partial path, then
           (let* ((p (url-path url))
                  (c (reverse (split p nil '(#\/)))))
             (rplaca c string)
             ;; we should really check for .. components too
             (merge-url template (join "/" (reverse c))))))))

(defgeneric append-url (url string)
  (:documentation "Appends a string onto the end of a url - only 'intelligence' is merging leading and trailing /'s"))
(defmethod append-url (url string)
  (let ((urlstring (urlstring url)))
    (parse-urlstring (concatenate 'string
				  (if (and (eql (elt urlstring (1- (length urlstring))) #\/)
					   (eql (elt string 0) #\/))
				      (subseq urlstring 0 (1- (length urlstring)))
				      urlstring)
				  string))))

(define-condition using-untainted-values (warning)
  ((with-url :initarg :with-url
	     :reader using-untainted-values-with-url)
   (with-call :initarg :with-call
	      :reader using-untainted-values-with-call))
  (:report (lambda (condition stream)
	     (format stream "Using untainted values with URL ~A, in function ~A"
		     (using-untainted-values-with-url condition)
		     (using-untainted-values-with-call condition)))))

(defvar *warn-when-using-untainted-values* nil
  "When set to a true value, will signal using-untainted-values (a warning) when functions
that return parameters from the outside are called without tainting.

See CL-TAINT for more about tainting and untainting.")

(defgeneric url-query-alist (url &key prefix case-sensitive)
  (:documentation "Return the URL query segment as a ( NAME VALUE ) alist.  NAME=VALUE pairs may be separated by ampersand or semicolons.  If PREFIX is supplied, select only the parameters that start with that text, and remove it from the keys before returning the list"))

(defmethod url-query-alist ((url t) &key prefix case-sensitive )
  (when *warn-when-using-untainted-values*
    (warn 'using-untainted-values :with-url url :with-call "url-query-alist"))
  (_url-query-alist url :prefix prefix :case-sensitive case-sensitive))

(defgeneric _url-query-alist (url &key prefix case-sensitive)
  (:documentation "Call that doesn't warn about using untainted values"))

(defmethod _url-query-alist ((url httplike-url) &key prefix case-sensitive )
  (let ((s-eq (if case-sensitive #'string= #'string-equal)))
    (remove-if-not
     (if prefix
         (lambda (x)
           (destructuring-bind (k v) x
             (if (and (>= (length k) (length prefix))
                      (funcall s-eq prefix k :end2 (length prefix)))
                 (list (subseq k (length prefix)) v)
               nil)))
       #'identity)
     (mapcar (lambda (x)
               (destructuring-bind (k &optional (v "")) (split x 2 '(#\=) )
                 (list (urlstring-unescape k) (urlstring-unescape v ))))
             (split (url-query url) nil '(#\& #\;))))))

(defun tainted-url-query-alist (url &key prefix case-sensitive)
  "Return tainted values (see documentation for CL-TAINT), otherwise exactly the same as url-query-alist"
  (loop for (key value) in (_url-query-alist url :prefix prefix :case-sensitive case-sensitive)
	collect (list key (taint value))))

(defgeneric url-query-param (url parameter-name &key case-sensitive)
  (:documentation "Return the values of the query parameter NAME, or NIL if not present"))
(defmethod url-query-param ((url t) name &key case-sensitive )
  (when *warn-when-using-untainted-values*
    (warn 'using-untainted-values :with-url url :with-call "url-query-param"))
  (_url-query-param url name :case-sensitive case-sensitive))

(defgeneric _url-query-param (url parameter-name &key case-sensitive)
  (:documentation "Call that doesn't warn about using untainted values"))
(defmethod _url-query-param ((url httplike-url) name &key case-sensitive )
  (mapcar #'cadr
          (remove-if-not
           (lambda (x) (funcall (if case-sensitive #'string= #'string-equal)
                                name (car x)))
           (url-query-alist url))))

(defun tainted-url-query-param (url parameter-name &key case-sensitive)
  "Return the parameter value tainted (see CL-TAINT), otherwise the same as url-query-param"
  (mapcar #'taint (url-query-param url parameter-name :case-sensitive case-sensitive)))

;;; How to choose the right URL class: add its scheme here

(defparameter *url-schemes*
  '(("HTTP" http-url)
    ("FTP" ftp-url)
    ("HTTPS" https-url)
    ("MAILTO" mailto-url)))

(defun url-class-for-scheme (scheme)
  (aif (cadr (assoc scheme *url-schemes* :test #'string-equal))
       (find-class it)
       nil))

(defun make-url (&rest rest &key scheme &allow-other-keys)
  (apply #'make-instance (url-class-for-scheme scheme) rest))

(defun parse-urlstring (string &optional (error-if-unparseable-p t))
  (let* ((scheme (string-upcase (car (split string 2 '(#\:)))))
         (class (url-class-for-scheme scheme)))
    (if (and scheme class)
        (let ((url (make-instance (url-class-for-scheme scheme)
                                  :string string :unparsed string)))
          (parse-url url)
          url)
	;; no scheme.  maybe it's relative.  We can try and merge it
	;; onto our default *default-url-defaults*
	(if (and (boundp '*default-url-defaults*)
		 *default-url-defaults*
		 (url-p *default-url-defaults*))
	    (merge-url *default-url-defaults* string)
	    (if error-if-unparseable-p
		(error "Relative URL and no ~A" '*default-url-defaults*))))))

;; This can be used to set up #u as a reader macro to read in a URL
;; object.  Note that this only works for absolute URLs

(defun url-reader (stream subchar arg)
  (declare (ignore subchar arg))
  (list 'parse-urlstring (read stream t nil t)))
(set-dispatch-macro-character #\# #\u #'url-reader)

(defun urlstring-unescape (url-string)
  (do* ((n 0 (+ n 1))
        (out '()))
      ((not (< n (length url-string))) (coerce (reverse out) 'string ))
    (let ((c (elt url-string n)))
      (setf out 
            (cond ((eql c #\%)
                   (progn (setf n (+ 2 n))
                          (cons (code-char
				 (or (parse-integer
				      url-string :start (- n 1)
				      :end (+ n 1)
				      :junk-allowed t
				      :radix 16) 32))
                                out)))
                  ((eql c #\+)
                   (cons #\Space out))
                  (t (cons c out)))))))


;;; This escapes URIs according to the generic URI syntax described in 
;;; rfc2396, or at least it does if the character set in use on this host
;;; is close enough to US-ASCII

;;; Allowed characters are
;;;         unreserved = alphanum | mark 
;;;         mark        = "-" | "_" | "." | "!" | "~" | "*" | "'" | "(" | ")"
;;; All others must be escaped

(defparameter +allowed-url-symbols+
  (let* ((lowercase "abcdefghijklmnopqrstuvwxyz")
	 (uppercase (string-upcase lowercase))
	 (numerical "0123456789")
	 (extrasyms "-_.!~*'()"))
    (map 'list #'identity (concatenate 'string lowercase uppercase numerical extrasyms)))
  "List of allowed characters in URLs")

(defun urlstring-reserved-p (c)
  (not (member c +allowed-url-symbols+)))

;; Someone reported some speed issues with urlstring-escape. This version seems to work
;; much faster on the normal case and deals better with large strings.
;; Of course, I have no idea if that helps anything at all.
;; -- Alan Shields [15 November 2005]
(defun urlstring-escape (to-be-encoded)
  (declare (type string to-be-encoded))
  (if (every (complement #'urlstring-reserved-p) to-be-encoded)
      to-be-encoded
      (urlstring-escape/guts (coerce to-be-encoded 'cons) nil)))

(defun urlstring-escape/guts (input output)
  (declare (type (or null (cons character)) input output)
	   (optimize (speed 3)))
  (if (null input)
      (coerce (nreverse output) 'string)
      (let ((c (car input)))
	(urlstring-escape/guts (cdr input) (if (urlstring-reserved-p c)
					       (append (nreverse (coerce (format nil "%~2,'0X" (char-code c)) 'cons))
						       output)
					       (cons c output))))))

(defun link (url &rest attribs)
  "Returns a string from a url with options attribs being passed.
Example:
(let ((url (parse-urlstring \"http://localhost\")))
  (link url :a 7 :b \"squid\"))
would be:
\"http://localhost/?a=7&b=squid\"

If the URL already has parameters, they are appended as well
If a parameter value is nil, the parameter is skipped."
  (declare (type araneida:url url))
  (if (null attribs)
      (urlstring url)
      (format nil "~A?~A=~A~{~A~}"
	      (urlstring url :query-parameters nil)
	      (urlstring-escape (string-downcase (symbol-name (first attribs))))
	      (urlstring-escape (princ-to-string (second attribs)))
              (let ((current-params (when (url-query url)
				      (loop for (attr val . ignore) in (url-query-alist url)
					    appending (list attr val)))))
		(loop for (attrib val . rest) on (append (cddr attribs) current-params) by #'cddr
		      when (not (null val))
		      collect (format nil "&~A=~A"
				      (urlstring-escape (string-downcase (princ-to-string attrib)))
				      (urlstring-escape (princ-to-string val))))))))

(defun url-query-string-from-alist (alist)
  "Creates a properly-url-escaped query string from an alist as from URL-QUERY-ALIST.
This is a good function to use when you need to, say, take a url, modify a few parameters, then re-assemble the url.

For example:
(defun strip-from-url (url &rest params-to-strip)
  (let ((current-query (url-query-alist url))
	(new-url (copy-url url)))
    (setf (url-query new-url)
	  (araneida:url-query-string-from-alist
	   (remove-if (lambda (pair)
			(member (car pair) params-to-strip :test #'string-equal))
		      current-query)))
    new-url))

Also fun at parties."
  (join "&" (mapcar (lambda (x)
		      (format nil "~A=~A" (urlstring-escape (first x)) (urlstring-escape (second x))))
		    alist)))

;;; perhaps this should be a setf method on url-query-param
#+nil (defun update-query-param (name value query-string)
  "Return a new query string based on QUERY-STRING but with the additional or updated parameter NAME=VALUE"
  (let ((pairs (mapcar (lambda (x) (split x 2 '(#\=) ))
                       (split query-string nil '(#\& #\;)))))
    (aif (assoc name pairs :test #'string=)
         (rplacd it (list value))
         (setf pairs (acons name (list value) pairs)))
    (join "&" (mapcar (lambda (x) (s. (car x)  "=" (cadr x))) pairs))))

(defmethod print-object ((u url) stream)
  (if *print-escape*
      (print-unreadable-object (u stream :type t :identity t)
	(format stream "\"~A\"" (urlstring u)))
      (princ (urlstring u) stream)))

(defmacro with-url-params ((&rest parameters) url-place &body body)
  "binds parameters to the values of the url parameters.
See WITH-TAINTED-URL-PARAMETERS for the tainted variety.
Non-present values will be bound to nil.
NB: matching is case insensitive."
  (once-only (url-place)
    (with-gensyms (alist)
      `(let ((,alist (araneida:url-query-alist ,url-place)))
	(let (,@(mapcar (lambda (param)
			  `(,param (second (assoc ,(symbol-name param) ,alist :test #'string-equal))))
			parameters))
	  ,@body)))))

(defmacro with-tainted-url-params ((&rest parameters) url-place &body body)
  "binds parameters to the tainted values of the url parameters.
See WITH-URL-PARAMETERS for the untainted variety.
Non-present values will be bound to nil. Note that that is NOT (taint nil), it's just nil.
NB: matching is case insensitive."
  (once-only (url-place)
    (with-gensyms (alist)
      `(let ((,alist (araneida:tainted-url-query-alist ,url-place)))
	(let (,@(mapcar (lambda (param)
			  `(,param (second (assoc ,(symbol-name param) ,alist :test #'string-equal))))
			parameters))
	  ,@body)))))
