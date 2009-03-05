;; -*- lisp -*-
(in-package :it.bese.ucw)

;;;; This file contains MATCHER API, HANDLER API which both makes up
;;;; the ucw DISPATCHER API.

;;;; * MATCHER API - Matcher classes match the request to return
;;;; appropriate values to handlers so that dispatchers can work
;;;; accordingly.

(defclass abstract-matcher ()
  ()
  (:documentation "Abstract matcher class."))

(defgeneric matcher-match (matcher application context)
  (:documentation "Abstract method for subclasses to implement a
matcher.  This method would return multiple-values according to
matcher internal nature.

No methods defined on this function may rebind *context*, nor
change CONTEXT's application. Only if the method matches the
request, it is allowed to modify CONTEXT or APPLICATION, even in
that case methods defined on this function must not modify
CONTEXT's application nor rebind *context*."))

(defclass url-matcher (abstract-matcher)
  ((url-string :initarg :url-string
	       :accessor url-string
	       :documentation "String for matching exactly for
	    url-matcher and regexp pattern for
	    regexp-url-matcher."))
  (:documentation "Matcher used to match url-string
  exactly (string=)."))

(defmethod matcher-match ((matcher url-matcher)
			  (application standard-application)
			  (context standard-request-context))
  "Returns matched url-string to handler."
  (when (and
	 (slot-boundp matcher 'url-string)
	 (string= (url-string matcher) (query-path-sans-prefix context)))
    (ucw.rerl.dispatcher.info "URL-MATCHER matched against url-string: ~S." (url-string matcher))
    (values t
	    (url-string matcher))))

;; Print objects
(defmethod print-object ((matcher url-matcher) stream)
  (print-unreadable-object (matcher stream :type t :identity t)
    (format stream "~S" (url-string matcher))))

(defclass regexp-url-matcher (url-matcher)
  ((scanner :initarg :scanner
	    :accessor scanner
	    :documentation "CL-PPCRE scanner used for pattern
	    matching. Created automagically when url-string is
	    set via accessors or initform."))
  (:documentation "Regexp matcher class to match url-string
  via cl-ppcre scanner."))

(defmethod shared-initialize :after ((matcher regexp-url-matcher)
                                     slot-names &key &allow-other-keys)
  (declare (ignore slot-names))
  (when (slot-boundp matcher 'url-string)
    (setf (scanner matcher) (cl-ppcre:create-scanner (url-string matcher)))))

(defmethod (setf url-string) (value (matcher regexp-url-matcher))
  (setf (scanner matcher) (cl-ppcre:create-scanner value))
  (setf (slot-value matcher 'url-string) value))

(defmethod matcher-match ((matcher regexp-url-matcher)
			  (application standard-application)
			  (context standard-request-context))
  "Returns two values to handler on success: the whole match as a
string plus an array of substrings (or NILs) corresponding to the
matched registers.  "
  (when (slot-boundp matcher 'scanner)
    (let ((result
	   (multiple-value-list
	    (cl-ppcre:scan-to-strings (scanner matcher)
				      (query-path-sans-prefix context)
				      :sharedp t))))
      (when (car result)
	(ucw.rerl.dispatcher.info "REGEXP-URL-MATCHER matched against url-string: ~S." (url-string matcher))
	(values-list
	 (cons t result))))))

(defclass tal-matcher (abstract-matcher)
  ((extension :accessor extension :initform ".ucw" :initarg :extension
              :documentation "Only urls whcih end in EXTENSION
              will be checked for a corresponding .tal file."))
  (:documentation "Matcher used to match tal templates. Searches
  for a valid tal template."))

(defmethod matcher-match ((matcher tal-matcher)
			  (application standard-application)
			  (context standard-request-context))
  "Returns tal template-truename to handler."
  (when-bind template-truename
      (template-truename (application.tal-generator application)
			 (make-pathname :type "tal"
					:defaults (query-path-sans-prefix context)))
    (ucw.rerl.dispatcher.info "TAL-MATCHER matched against tal file: ~S." template-truename)
    (values t
	    template-truename)))

(defclass action-matcher (abstract-matcher)
  ()
  (:documentation "Matcher used to find action from the URL like
  AAAA.ucw?_s=XXX&_f=YYY&_a=ZZZ"))

(defmethod matcher-match ((matcher action-matcher)
			  (application standard-application)
			  (context standard-request-context))
  "Returns action that is found in request URL"
  (when-bind session (find-session application context)
    (when-bind frame-id (find-frame-id context)
      (when-bind frame (find-frame-by-id session frame-id)
        (when-bind action (find-action frame (find-action-id context))
	  (ucw.rerl.dispatcher.info "ACTION-MATCHER found action: ~S." action)
	  (setf (session.current-frame session) frame)
	  (setf (context.session context) session)
	  (values t
		  action))))))

;;;; * HANDLER API - Handler classes are used to handle requests that
;;;; * are matched by matchers. Matcher results are passed as last
;;;; * parameter for handlers use.

(defclass abstract-handler ()
  ()
  (:documentation "Abstract handler class."))

(defgeneric handler-handle (handler application context matcher-result)
  (:documentation "Abstract function for handler classes to
implement in order to handle a request matched by relevant
matcher.

These methods may modify context as they wish since they'r
matched, request will be closed after this method is run."))

(defclass function-handler (abstract-handler)
  ((handler :accessor handler
	    :initarg :handler
	    :documentation "Called when the dispatchers finds a
            maching request. This function must be a zero arg'ed
            (lambda ()
               ...)"))
  (:documentation "Function handler class funcalls the handler
  lambda while providing application and context. "))

(defmethod handler-handle ((function-handler function-handler)
			   (application standard-application)
			   (context standard-request-context)
			   matcher-result)
  (when (slot-boundp function-handler 'handler)
    (ucw.rerl.dispatcher.info "FUNCTION-HANDLER, running handler: ~S." (handler function-handler))
    (funcall (handler function-handler))))

(defclass action-handler (abstract-handler)
  ()
  (:documentation "Executes the action provided by the matcher
while ensuring the session and making a new frame."))

(defmethod handler-handle ((handler action-handler)
			   (application standard-application)
			   (context standard-request-context)
			   matcher-result)
  
  (let ((action (car matcher-result)))
    (ucw.rerl.dispatcher.info "ACTION-HANDLER, handling action: ~S." action)
    (ensure-session application context)
    (make-new-frame (context.session context))
    (with-action-error-handler ()
      (funcall action))
    ;; nb: don't use the same frame as returned by make-new-frame
    ;; to get the current component. 99% of the time they will be
    ;; the same as below, but 1% they will be differenet and that
    ;; 1% of the time is when all hell has broken lose and we're
    ;; dancing with cats...
    (render-loop (frame.window-component (context.current-frame context)))
    (save-backtracked (context.current-frame context))
    (flush-request-response context)))

(defclass entry-point-handler (action-handler)
  ((handler :accessor handler
	    :initarg :handler
	    :documentation "Function to run when this
	   entry-point-handler runs. This handler is a
	   zero-arged."))
  (:documentation "This handler is used to handle
  entry-points."))

(defmethod handler-handle ((handler entry-point-handler)
			   (application standard-application)
			   (context standard-request-context)
			   matcher-result)
  (ucw.rerl.dispatcher.info "ENTRY-POINT-HANDLER directing control to ACTION-HANDLER.")
  (call-next-method handler application context (cons (handler handler) matcher-result)))

(defclass parenscript-handler (abstract-handler)
  ((cache :accessor cache :initarg :cache :initform t)
   (compile-args :accessor compile-args :initarg :compile-args :initform '())
   (parenscript-file :accessor parenscript-file :initarg :parenscript-file)
   (cached-javascript :accessor cached-javascript)
   (last-compiled :accessor last-compiled))
  (:documentation "This handler is used to serve a
compiled javascript string from the given PARENSCRIPT-FILE.

The file is compiled lazily on the first request and in-package
forms are working as expected. Each form is evaluated, so using
backquote is possible and in fact a must. The following dynamic
variables are bound then compiling: *context* - the current
request context (hint: (context.application *context*))"))

(defmethod handler-handle ((handler parenscript-handler)
			   (application standard-application)
			   (context standard-request-context)
			   matcher-result)
  
  (with-accessors ((cache cache) (parenscript-file parenscript-file)
		   (cached-javascript cached-javascript) (last-compiled last-compiled)
		   (compile-args compile-args)) handler
    (ucw.rerl.dispatcher.info "PARENSCRIPT-HANDLER serving file ~S." parenscript-file)
    (when (or (not cache)
	      (not (slot-boundp handler 'cached-javascript))
	      (> (file-write-date parenscript-file)
		 last-compiled))
      (setf cached-javascript
	    (apply #'js:compile-parenscript-file-to-string parenscript-file compile-args))
      (setf last-compiled (get-universal-time)))
  ;; TODO make sure some cache info is sent so that it will be cached by the browser
    (serve-sequence cached-javascript
		    :last-modified last-compiled
		    :content-type "text/javascript")))

(defclass tal-handler (abstract-handler)
  ()
  (:documentation "This handler simply serves up TAL pages.

Tal pages can refer to the session object (and it will be
maintained across requests) but must avoid using call/answer or
actions."))

(defmethod handler-handle ((handler tal-handler)
			   (application standard-application)
			   (context standard-request-context)
			   matcher-result)
 (when (ends-with (query-path-sans-prefix context) (extension handler))
  (let ((template-truename (car matcher-result)))
    (ucw.rerl.dispatcher.info "TAL-HANDLER publishing tal ~S as ~S." template-truename (query-path-sans-prefix context))
    (ensure-session application context)
    (make-new-frame (context.session context))
    (block render
      (handler-bind ((error (lambda (condition)
			      (catch 'abort-action
				(handle-action-error condition (collect-backtrace condition)))
			      (render-loop (frame.window-component (context.current-frame context)))
			      (return-from render nil))))
	(%render-template context
			  (application.tal-generator application)
			  template-truename
			  'nil)))
    (flush-request-response context))))

;;;; * UCW Request Dispatcher

;;;; Whenever a requset comes into a ucw app we look at its list of
;;;; dispatcher and try to find one which sholud handle the request.

;;;; NB: Dispatcher instances are a shared resource, the dispatch
;;;; function may be called on the same dispatcher object by multiple
;;;; threads. Take this into consideration when writing custom
;;;; dispatcher classes. IOW: don't put any per-request state in the
;;;; dispatcher instance.

(defclass abstract-dispatcher ()
  ((priority :accessor priority
	     :initarg :priority
             :initform 0
             :documentation "Dispatchers will be checked from
             highest-priority to lowest priority. The default
             values for priority on the various classes assume
             this is a positive integer less than
             most-positive-fixnum.")))

(defgeneric dispatch (dispatcher application context)
  (:documentation "Entry point into a dispatcher. Must return T
  if the context has been handled or NIL if it hasn't.

No methods defined on this function may rebind *context*, nor
change CONTEXT's application. Only if the method returns T is it
allowed to modify CONTEXT or APPLICATION, even in that case
methods defined on this function must not modify CONTEXT's
application nor rebind *context*."))

(defmethod dispatch ((dispatcher abstract-dispatcher)
		     (application standard-application)
		     (context standard-request-context))
  (let ((result 
	 (multiple-value-list (matcher-match dispatcher application context))))
    (when (and (listp result)
	       (car result))
      (handler-handle dispatcher application context (rest result))
      t)))

(defclass minimal-dispatcher (abstract-dispatcher regexp-url-matcher function-handler)
  ()
  (:default-initargs :priority 1)
  (:documentation "A dispatcher which does as little work as
  possible. The handler function must do basically
  everything (including shutdowning down request and response)."))

(defmacro minimal-dispatcher (url-regexp &body body)
  `(make-instance 'minimal-dispatcher
                  :url-string ,url-regexp
                  :handler (lambda ()
                             ,@body)))

(defclass simple-dispatcher (abstract-dispatcher regexp-url-matcher function-handler)
  ()
  (:default-initargs :priority 1)
  (:documentation "This class of dispatchers avoids all of UCW's
  standard call/cc (and therefore frame/backtracking/component)
  mechanism.

Unlike all other UCW dispatchers a simple-dispatcher must not use
CALL, and must perform the rendering directly within the handler."))

(defmethod handler-handle ((dispatcher simple-dispatcher)
			   (application standard-application)
			   (context standard-request-context)
			   matcher-result)
  (ensure-session application context)
  (call-next-method)
  (flush-request-response context))

(defmacro simple-dispatcher (url-regexp &body body)
  `(make-instance 'simple-dispatcher
                  :url-string ,url-regexp
                  :handler (lambda ()
                             ,@body)))

(defclass url-dispatcher (abstract-dispatcher url-matcher entry-point-handler)
  ()
  (:default-initargs :priority 1))

(defmacro url-dispatcher (url-string &body body)
  "Returns a dispatcher which matches when URL-STRING matches and
executes, as if in a defaction, BODY."
  `(make-instance 'url-dispatcher
                  :url-string ,url-string
                  :handler (lambda ()
                             (with-call/cc
                               (let ((self nil))
                                 ,@body)))))

(defclass regexp-dispatcher (abstract-dispatcher regexp-url-matcher entry-point-handler)
  ()
  (:default-initargs :priority 1))

(defmacro regexp-dispatcher (url-regexp &body body)
  "Returns a dispatcher which matches when URL-REGEXP (a regular
expression which is passed untouched to cl-ppcre:scan-to-strings)
matches and executes, as if in a defaction, BODY.

Any registers in URL-REGEXP are available through the array bound
to *dispatcher-registers*."
  `(make-instance 'regexp-dispatcher
                  :url-string ,url-regexp
                  :handler (lambda ()
                             (with-call/cc
                               (let ((self nil))
                                 ,@body)))))

;;;; ** ACTION-DISPATCHER

(defclass action-dispatcher (abstract-dispatcher action-matcher action-handler)
  ()
  (:default-initargs :priority most-positive-fixnum)
  (:documentation "This is the core dispatcher for ucw. Due to
  how ucw's compute-url method works it is important that the
  action-dispatcher be checked before any url-dispatchers, so it
  should be the first element in an application's dispatcher
  list."))

(defmethod handler-handle ((dispatcher action-dispatcher)
			   (application standard-application)
			   (context standard-request-context)
			   matcher-result)
  (let ((frame (session.current-frame (context.session context))))
    (reinstate-backtracked frame)
    (call-callbacks frame (context.request context))
    (call-next-method))) 

(defun action-dispatcher (&optional (priority nil priorityp))
  (apply #'make-instance 'action-dispatcher
	 (when priorityp
	   (list :priority priority))))

;;;; *** Tal Directory dispatcher

(defclass tal-dispatcher (abstract-dispatcher tal-matcher tal-handler)
  ()
  (:default-initargs :priority 0)
  (:documentation "This dispatcher simply serves up TAL pages.
Tal pages can refer to the session object (and it will be
maintained across requests) but must avoid using call/answer or
actions."))

(defclass parenscript-dispatcher (abstract-dispatcher regexp-url-matcher parenscript-handler)
  ()
  (:default-initargs :priority 1))

(defun parenscript-dispatcher (url-regex parenscript-file &rest args &key (cache t) &allow-other-keys)
  "Return a parenscript-dispatcher"
  (remf-keywords args :cache)
  (make-instance 'parenscript-dispatcher
		 :url-string url-regex
		 :parenscript-file parenscript-file
                 :compile-args args
		 :cache cache))

(defun standard-dispatchers ()
  (list (make-instance 'action-dispatcher)
	(parenscript-dispatcher
	 "^ucw/js/per-application.js$"
	 (merge-pathnames
	  (make-pathname :directory '(:relative "src")
			 :name "per-application-parenscript"
			 :type "lisp")
	  (asdf:component-pathname
	   (asdf:find-system :ucw))))))


;; Copyright (c) 2003-2006 Edward Marco Baringer
;; All rights reserved. 
;; 
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;; 
;;  - Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 
;;  - Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 
;;  - Neither the name of Edward Marco Baringer, nor BESE, nor the names
;;    of its contributors may be used to endorse or promote products
;;    derived from this software without specific prior written permission.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
