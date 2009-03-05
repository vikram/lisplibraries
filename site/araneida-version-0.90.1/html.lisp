(in-package :araneida)

;;; XXX fix this, it's not correct
(defun html-reserved-p (c)
  (member c '(#\< #\"  #\> #\&)))

(defun html-escape (html-string)
  (apply #'concatenate 'string
         (loop for c across html-string
               if (html-reserved-p c)
               collect (format nil "&#~A;" (char-code c))
               else if (eql c #\Newline) collect "<br>"
               else collect (string c))))

(defun html-escape-tag (tag attrs content)
  (declare (ignore tag attrs))
  (s. (mapcar #'html-escape content)))

(setf (get 'escape 'html-converter) #'html-escape-tag)
(setf (get 'null 'html-converter) #'princ-to-string)

#+nil
(defun html-attr (attr)
  (declare (optimize (speed 3) (debug 0)))
  (labels ((flatten (a) (if (consp a)
                            (apply #'s. (mapcar #'flatten a))
                          a)))
    ;; surely somewhere there's a function that takes a list of
    ;; strings to concatenate?  or maybe this isn't slow really
    (let ((r (apply #'s.
                    (loop for a on attr by #'cddr
                          append (list " " (symbol-name (car a))
                                       "=\"" (s. (flatten (cadr a))) "\"")))))
      (the simple-string r))))

(macrolet ((html-attr-body ()
	     `(with-output-to-string (o)
	       (loop for (att val . rest) on attr by #'cddr do
		(cond
		  #+parenscript((and (symbolp att) (equal (symbol-name 'css) (symbol-name att)))
				(progn
				  (princ " " o)
				  (princ "style=\"" o)
				  (princ (val-printer (js::css-inline-func val)) o)
				  (princ "\"" o)))
		  ((symbolp att)
		   (progn
		     (princ " " o)
		     (princ (string-downcase (symbol-name att)) o)
		     (princ "=\"" o)
		     (princ (val-printer val) o)
		     (princ "\"" o)))
		  (t
		   (error "attribute ~S is not a symbol in attribute list ~S" att attr)))))))
  (defun html-attr (attr)
    (macrolet ((val-printer (val)
		 val))
      (html-attr-body)))
  (defun html-attr-escaped (attr)
    (macrolet ((val-printer (val)
		 `(html-escape ,val)))
      (html-attr-body))))

(defun empty-element-p (tag)
  (member (intern (symbol-name tag) #.*package*) '(img br hr link input meta)))

;; I wrote this to debug the new html functions. I might eventually work it into the
;; test suite.
;; -- Alan Shields [14 November 2005]
#+nil(defun testfunc ()
       (flet ((samplefun () "this!"))
	 (flet ((htmlf (htmlme) (html htmlme)))
	   (list
	    (htmlf '(comment))
	    (htmlf '((comment)))
	    (htmlf '((comment "detail")))
	    (htmlf '(comment "content"))
	    (htmlf '(comment (p "here")))

	    (htmlf '(p))
	    (htmlf '((p)))
	    (htmlf '((p :class "awful")))
	    (htmlf '(p "first " (span "second") ("third " "fourth" " fifth")))
     
	    (htmlf '(br))
	    (htmlf '((br)))
	    (htmlf '((br :silly "tag")))

	    (htmlf '((span :css (:color "black" :size "200%" :family "the&family")) "Print this"))
	    (htmlf '(html
		     (head (title "A simple title")
		      (css (h1 :color "red")
		           (h2 :color "blue")))
		     (body (h1 "A simple header"))))
	    
	    (htmlf `(html
		     (head (title "Another simple title")
		      (js-script
		       (defun foo (x)
			 (alert (+ "Be a lert: " x)))
		       (defun bar (x)
			 (alert (+ "Bar - " x)))))
		     (body
		      (p "All foos and bars come to an end")
		      (p ((a :onclick ,(js:js-inline (foo "e&nd"))) "Foo!"))
		      (p ((a :onclick ,(js:js-inline (bar "e&nd"))) "Bar!")))))

	    (htmlf #'samplefun)
       
	    (htmlf :br)
       
	    (htmlf 1)))))

(defmacro defhtmltag (tag (attributes-var content-var) &body body)
  "Define a custom HTML tag
Useful for custom phrases, or even special constructs.

Example:
(defhtmltag coffee (attr content)
  (declare (ignore attr content))
  \"c|_|\")

So, saying
(span \"Nice hot \" (coffee))

Would produce:
<span>Nice hot c|_|</span>

More in-depth use would be something such as:
(even-odd-list
 (li \"one\")
 (li \"two\")
 (li \"three\"))

Turning into:
(ul
 ((li :class \"odd\") \"one\")
 ((li :class \"even\") \"two\")
 ((li :class \"odd\") \"odd\"))

Your custom tag is expected to return either a string or
an html construct like you'd pass to HTML-STREAM."
  (araneida::with-gensyms (throw-away)
    `(setf (get ',tag :html-converter)
      (lambda (,throw-away ,attributes-var ,content-var)
	(declare (ignore ,throw-away))
	(funcall 
         (lambda (,attributes-var ,content-var)
	   ,@body)
	 ,attributes-var ,content-var)))))

(defun htmlp (html)
  "Returns t if html is a legal HTML list.
NB: HTML and friends print out a superset of legal html lists.

'(ul (li \"yes\") (li \"no\")) is legal
3 is not

But HTML will print both of them"
  (and (consp html)
       (not (stringp (car html)))))

(defmacro destructure-html ((tag-sym attrs-sym content-sym) html &body body)
  "Destructure an HTML construct.
(destructure-html (tag attrs content) '((span :class \"strange\") \"A strange span!\")
  (format t \"<~A ~{~A~}>~{~A~}</~A>\" tag attrs content tag))
If the construct is invalid, it will cause an error"
  (once-only (html)
    `(if (araneida:htmlp ,html)
      (let ((,tag-sym (if (consp (car ,html))
			  (caar ,html)
			  (car ,html)))
	    (,attrs-sym (if (consp (car ,html))
			    (cdar ,html)
			    nil))
	    (,content-sym (cdr ,html)))
	,@body))))

; I admit, this is a rather goofy way to write this. There's just so much code they have in common
; and this lets me modify them rather easily.
(macrolet ((html-body ()
	     `(ret-block
	       (cond ((htmlp things)
		      (destructure-html (tag attrs content) things
			(let ((special-effect (get tag :html-converter)))
			  (if special-effect
			      (if (not (functionp special-effect))
				  (error "Tag ~A has :html-converter set, but NOT as a function." tag)
				  (call-self stream (funcall special-effect tag attrs content) inline-elements))
			      (cond ((equal (symbol-name 'comment) (symbol-name tag))
				     (printing
				      (format-out "<!-- ~{~A ~}~%" attrs)
				      (iter-list (c content)
						 (call-self stream c inline-elements))
				      (format-out " -->~%")))
				    #+parenscript((equal (symbol-name 'css) (symbol-name tag))
						  (printing
						   (format-out "<style type=\"text/css\">~%")
						   (format-out "<!--~%")
						   (iter-list (c content)
							      (format-out (js::css-rule-to-string (js::make-css-rule (car c) (cdr c)))))
						   (format-out "~%-->")
						   (format-out "</style>")))
				    #+parenscript((equal (symbol-name 'js-script) (symbol-name tag))
						  (printing
						   (format-out "<script type=\"text/javascript\">~%")
						   (format-out "// <![CDATA[~%")
						   (format-out (js:js* (cons 'progn content)))
						   (format-out "~%// ]]>~%")))
				    ((not (empty-element-p tag))
				     (printing
				      (format-out "<~A~A>" (string-downcase (symbol-name tag)) (attr-printer attrs))
				      (iter-list (c content)
						 (call-self stream c inline-elements))
				      (format-out "</~A>~:[~;~%~]"
						  (string-downcase (symbol-name tag))
						  (not (member tag inline-elements)))))
				    (t
				     (format-out "<~A~A>" (string-downcase (symbol-name tag)) (attr-printer attrs))))))))
		     ((consp things)
		      (iter-list (thing things) (format-out "~A" thing)))
		     ((functionp things)
		      (call-function things stream))
		     ((keywordp things)
		      (format-out "<~A>" (string-downcase (symbol-name things))))
		     (t
		      (format-out "~A" (thing-printer things)))))))
  ; stream forms first
  (macrolet ((ret-block (output-block)
	       `(progn
		 ,output-block
		 t))
	     (iter-list ((item list) func)
	       `(dolist (,item ,list)
		 ,func))
	     (printing (&body list)
	       `(progn
		 ,@list))
	     (format-out (&rest args)
	       `(format stream ,@args))
	     (call-function (things stream)
	       `(funcall ,things ,stream)))
    (defun html-stream (stream things &optional inline-elements)
      "Format supplied argument as HTML.  Argument may be a string
(returned unchanged) or a list of (tag content) where tag may be
(tagname attrs).  ((a :href \"/ \") \"home\") is formatted as you'd
expect it to be.  INLINE-ELEMENTS is a list of elements not to print a
newline after.  Returns T unless broken, so can be the last form in a
handler
For special effects, set the HTML-CONVERTER property of a symbol for a tag to a function.  It will be called with arguments
(TAG ATTRS CONTENT) and should return a string to be interpolated at that point."
      (declare (optimize (speed 3))
	       (type stream stream))
      (macrolet ((attr-printer (attrs)
		   `(html-attr ,attrs))
		 (call-self (stream things &optional inline-elements)
		   `(html-stream ,stream ,things ,inline-elements))
		 (thing-printer (things)
		   `(princ-to-string things)))
	(html-body)))
    (defun html-escaped-stream (stream things &optional inline-elements)
    "Format supplied argument as HTML, escaping properly.
Just like html-stream except certain things are now html-escaped.
Content - in  '(p \"foo\") \"foo\" is the content - is escaped,
as well as the values of attributes. Please note that this CAN
result in double escaping if calling code also escapes.
For special effects, set the HTML-CONVERTER property of a symbol for a tag to a function.  It will be called with arguments
(TAG ATTRS CONTENT) and should return a string to be interpolated at that point. NB that the attrs and content will be
passed in *unescaped*"
      (declare (optimize (speed 3))
	       (type stream stream))
      (macrolet ((attr-printer (attrs)
		   `(html-attr-escaped ,attrs))
		 (call-self (stream things &optional inline-elements)
		   `(html-escaped-stream ,stream ,things ,inline-elements))
		 (thing-printer (things)
		   `(html-escape (princ-to-string ,things))))
	(html-body))))
  ; now for the string forms
  (macrolet ((ret-block (output-block)
	       output-block)
	     (iter-list ((item list) func)
	       `(apply #'concatenate 'string (mapcar (lambda (,item) ,func) ,list)))
	     (printing (&body list)
	       `(concatenate 'string
		 ,@list))
	     (format-out (&rest args)
	       `(format nil ,@args))
	     (call-function (things stream)
	       (declare (ignore stream))
	       `(funcall ,things)))
    (defun html (things &optional inline-elements)
      "Format supplied argument as HTML.  Argument may be a string (returned unchanged) or a list of (tag content)
where tag may be (tagname attrs).  ((a :href \"/\") \"home\") is formatted as you'd expect it to be.
For special effects, set the HTML-CONVERTER property of a symbol for a tag to a function.  It will be called with arguments
(TAG ATTRS CONTENT) and should return a string to be interpolated at that point."
      (declare (optimize (speed 3)))
      (macrolet ((attr-printer (attrs)
		   `(html-attr ,attrs))
		 (call-self (stream things &optional inline-elements)
		   `(html ,things ,inline-elements))
		 (thing-printer (things)
		   `(princ-to-string ,things)))
	(html-body)))))

#||
(search-html-tree '((string> ht :element) t (= p :element)) '((html) ((body) ((p) "foo") ((p) "bar") ((div :title "titl") "blah"))))
||#

(defun search-html-tree (search-terms tree)
  (labels ((node-matches (term tree)
	     (or (eql term t)
		 (destructuring-bind (op content name) term
		   (let ((r-op (if (eql op '=) 'equal op)))
		     (if (eql name :element)
			 (funcall r-op content (caar tree))
			 (funcall r-op content (getf (cdar tree) name))))))))
    (cond ((eql (cdr search-terms) nil)
	   (and (node-matches (car search-terms) tree) tree))
	  ((null tree) nil)
	  ((node-matches (car search-terms) tree)
	   (remove-if #'null
		      (mapcar (lambda (tr)
				(search-html-tree (cdr search-terms) tr))
			      (cdr tree)))))))


#+parenscript(defmacro css-file (request &rest rules)
	       "Sends CSS as a file in response to request, as specified by rules.
For example:
(defmethod handle-request-response ((handler my-handler) method request)
  (css-file request
	    (* :border \"1px solid black\")
	    (div.bl0rg :font-family \"serif\")
	    ((\"a:active\" \"a:hoover\") :color \"black\" :size \"200%\")))
See the documentation for Parenscript for more info on the CSS rules themselves."
	       `(progn
		 (request-send-headers ,request :content-type "text/css")
		 ,@(mapcar (lambda (rule)
			     `(princ (js::css-rule-to-string (js::css-rule ,@rule)) (request-stream ,request)))
			   rules)
		 t))

#+parenscript(defmacro js-file (request &rest body)
	       "Sends Javascript as a file in response to request, as specified by the javascript body.
For example:
(defmethod handle-request-response ((handler my-handler) method request)
  (js-file request
	   (defun hello ()
	     (alert \"Hello, World!\"))))
See the documentation for Parenscript for more info on how to do Javascript."
	       `(progn
		 (request-send-headers ,request :content-type "text/javascript")
		 (princ (js:js ,@body) (request-stream ,request))
		 t))