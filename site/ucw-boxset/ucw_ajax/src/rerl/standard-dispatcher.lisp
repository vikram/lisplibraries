;; -*- lisp -*-
(in-package :it.bese.ucw)

;;;; This file contains MATCHER API, HANDLER API which both makes up
;;;; the ucw DISPATCHER API.

;;;; * MATCHER API - Matcher classes match the request to return
;;;; appropriate values to handlers so that dispatchers can work
;;;; accordingly.

(defparameter +polling-dispatcher-default-priority+      (- most-positive-fixnum 1000))
(defparameter +ajax-action-dispatcher-default-priority+  (- most-positive-fixnum 1001))
(defparameter +action-dispatcher-default-priority+       (- most-positive-fixnum 1002))
(defparameter +client-state-dispatcher-default-priority+ 1002)
(defparameter +parenscript-dispatcher-default-priority+  1001)
(defparameter +reload-dispatcher-default-priority+       1000)
(defparameter +minimal-dispatcher-default-priority+      1)
(defparameter +simple-dispatcher-default-priority+       1)
(defparameter +url-dispatcher-default-priority+          1)
(defparameter +regex-dispatcher-default-priority+        1)
(defparameter +tal-dispatcher-default-priority+          0)

(defclass abstract-matcher ()
  ()
  (:documentation "Abstract matcher class."))

(defgeneric matcher-match (matcher application context)
  (:documentation "Abstract method for subclasses to implement a
matcher.  This method would return multiple-values according to
matcher internal nature, but the first return value is always
used to indicate if this matcher matched.

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

(defprint-object (self url-matcher)
  (princ (url-string self)))

(defmethod matcher-match ((matcher url-matcher)
			  (application standard-application)
			  (context standard-request-context))
  "Returns matched url-string to handler."
  (ucw.rerl.dispatcher.dribble "~S trying to match as url-matcher, ~S = ~S"
                               matcher (query-path-sans-prefix context) (url-string matcher))
  (when (string= (url-string matcher) (query-path-sans-prefix context))
    (ucw.rerl.dispatcher.dribble "~S matched" matcher)
    (values t (url-string matcher))))

(defclass starts-with-url-matcher (url-matcher)
  ())

(defmethod matcher-match ((matcher starts-with-url-matcher)
			  (application standard-application)
			  (context standard-request-context))
  "Returns matched url-string to handler."
  (ucw.rerl.dispatcher.dribble "~S trying to match as starts-with-url-matcher, ~S starts-with? ~S"
                               matcher (query-path-sans-prefix context) (url-string matcher))
  (when (starts-with (query-path-sans-prefix context) (url-string matcher))
    (ucw.rerl.dispatcher.dribble "~S matched" matcher)
    (values t (url-string matcher))))

(defclass session-frame-and-url-matcher (session-frame-matcher url-matcher)
  ()
  (:documentation "Matches when the url matches and there's
an identifiable session and frame in the request."))

(defmethod matcher-match ((matcher session-frame-and-url-matcher)
			  (application standard-application)
			  (context standard-request-context))
  (ucw.rerl.dispatcher.dribble "~S trying to match as session-frame-and-url-matcher" matcher)
  (multiple-value-bind (matchesp session frame)
      (call-next-method)
    (when (and matchesp
               (string= (url-string matcher)
                        (query-path-sans-prefix context)))
      (ucw.rerl.dispatcher.dribble "~S matched" matcher)
      (values t session frame (url-string matcher)))))

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
matched registers."
  (ucw.rerl.dispatcher.dribble "~S trying to match ~S" matcher (query-path-sans-prefix context))
  (let ((result
         (multiple-value-list
             (cl-ppcre:scan-to-strings (scanner matcher)
                                       (query-path-sans-prefix context)
                                       :sharedp t))))
    (when (car result)
      (ucw.rerl.dispatcher.dribble "~S matched, regexp result is ~S" matcher result)
      (values-list
       (cons t result)))))

(defclass tal-matcher (abstract-matcher)
  ((extension :accessor extension :initform ".ucw" :initarg :extension
              :documentation "Only urls whcih end in EXTENSION
              will be checked for a corresponding .tal file."))
  (:documentation "Matcher used to match tal templates. Searches
  for a valid tal template."))

(defprint-object (self tal-matcher)
  (princ (extension self)))

(defmethod matcher-match ((matcher tal-matcher)
			  (application standard-application)
			  (context standard-request-context))
  "Returns tal template-truename to handler."
  (when-bind template-truename
      (template-truename (application.tal-generator application)
			 (make-pathname :type "tal"
					:defaults (query-path-sans-prefix context)))
    (ucw.rerl.dispatcher.debug "~S matched against tal file ~S" matcher template-truename)
    (values t template-truename)))

(defclass session-frame-matcher (abstract-matcher)
  ((frame-is-optional-p :accessor frame-is-optional-p :initarg :frame-is-optional-p))
  (:default-initargs :frame-is-optional-p nil)
  (:documentation "Matches when a valid session and a valid frame
could be identified from the request."))

(defmethod matcher-match ((matcher session-frame-matcher)
			  (application standard-application)
			  (context standard-request-context))
  (ucw.rerl.dispatcher.dribble "~S trying to match as session-frame-matcher, session-id is ~S, frame-id is ~S"
                               matcher (find-session-id context) (find-frame-id context))
  (when-bind session (find-session application context)
    (ucw.rerl.dispatcher.dribble "~S matched session ~S" matcher session)
    (let ((frame-id (find-frame-id context)))
      (if-bind frame (find-frame-by-id session frame-id)
        (progn
          (ucw.rerl.dispatcher.dribble "~S matched frame ~S" matcher frame)
          (values t session frame))
        (progn
          (ucw.rerl.dispatcher.dribble "~S NOT matched any frame" matcher)
          (values (frame-is-optional-p matcher) session nil))))))

;;;; * HANDLER API - Handler classes are used to handle requests that
;;;; * are matched by matchers. Matcher results are passed as last
;;;; * parameter for handlers use.

(defclass abstract-handler ()
  ()
  (:documentation "Abstract handler class."))

(defgeneric handler-handle (handler application context matcher-result)
  (:documentation "Abstract function for handler classes to
implement the handling of the matched request.

These methods may modify context as they wish since they are
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
    (ucw.rerl.dispatcher.debug "~S running handler ~S" function-handler (handler function-handler))
    (funcall (handler function-handler))))

(defvar +callbacks-only-action+
  (make-action-body (:with-call/cc nil :make-new-frame nil
                     :isolated nil :invocation-isolated nil
                     :call-render-loop nil :ajax t)
    (handle-ajax-request ()
      ;; nop, just send the success tag
      ))
  "This action is called after some callbacks were called but no action was found in the request")

(defclass entry-point-handler ()
  ((handler :accessor handler
	    :initarg :handler
	    :documentation "Function to run when this
	   entry-point-handler runs. This handler is a
	   zero-arged."))
  (:documentation "This handler is used to handle
  entry-points."))

(defmethod initialize-instance :after ((handler entry-point-handler) &key &allow-other-keys)
  (when (functionp (handler handler))
    (setf (handler handler) (make-action (handler handler) :invocation-isolated nil
                                         :isolated nil))))

(defmethod handler-handle ((handler entry-point-handler)
			   (application standard-application)
			   (context standard-request-context)
			   matcher-result)
  (ucw.rerl.dispatcher.debug "~S is handling the request" handler)
  (ensure-session application context)
  (handle-action application (context.session context) nil (handler handler)))

(defclass parenscript-handler (abstract-handler)
  ((cachep :accessor cachep :initarg :cache :initform t)
   (cached-javascript :accessor cached-javascript :initform nil)
   (last-compiled :initform 0 :accessor last-compiled))
  (:documentation "This handler is used to serve a
compiled javascript string.

The source is compiled lazily on the first request.
The following dynamic variables are bound when compiling:
  *context* - the current request context
              (hint: (context.application *context*))"))

(defgeneric parenscript-handler-compile (handler)
  (:documentation "Called when (re)compilation is needed. Should return the compiled js string."))

(defgeneric parenscript-handler-timestamp (handler)
  (:documentation "Should return the timestamp of the source (used for dirtyness check)."))

(defgeneric cached-javascript (handler)
  (:documentation "Should retrive/store the cached js string."))

(defmethod handler-handle ((handler parenscript-handler)
			   (application standard-application)
			   (context standard-request-context)
			   matcher-result)
  
  (with-accessors ((cachep cachep) (cached-javascript cached-javascript)
                   (last-compiled last-compiled)) handler
                   (ucw.rerl.dispatcher.debug "~S handling the request" handler)
                   (when (or (not cachep)
                             (not cached-javascript)
                             (> (parenscript-handler-timestamp handler)
                                last-compiled))
                     (ucw.rerl.dispatcher.debug "~S is compiling the file" handler)
                     ;; enable a #"" syntax for i18n lookups in parenscript
                     (let ((*readtable* (copy-readtable)))
                       (enable-js-sharpquote-reader)
                       (setf cached-javascript (parenscript-handler-compile handler)))
                     (setf last-compiled (get-universal-time)))
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
;;;; dispatcher and try to find one which should handle the request.

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
  (ucw.rerl.dispatcher.dribble "In dispatch, now calling matcher-match for (~S ~S ~S)" dispatcher application context)
  (let ((result (multiple-value-list (matcher-match dispatcher application context))))
    (ucw.rerl.dispatcher.dribble "Result is ~S" result)
    (when (and (consp result)
	       (car result))
      (ucw.rerl.dispatcher.debug "~S matched, calling handler-handle with (~S ~S ~S)" dispatcher application context (rest result))
      (handler-handle dispatcher application context (rest result))
      t)))

(defclass minimal-dispatcher (abstract-dispatcher regexp-url-matcher function-handler)
  ()
  (:default-initargs :priority +minimal-dispatcher-default-priority+)
  (:documentation "A dispatcher which does as little work as
  possible. The handler function must do basically
  everything (including shutdowning down request and response)."))

(defmacro make-minimal-dispatcher (url-regexp &body body)
  `(make-instance 'minimal-dispatcher
                  :url-string ,url-regexp
                  :handler (lambda ()
                             ,@body)))

(defclass simple-dispatcher (abstract-dispatcher regexp-url-matcher function-handler)
  ()
  (:default-initargs :priority +simple-dispatcher-default-priority+)
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

(defmacro make-simple-dispatcher (url-regexp &body body)
  `(make-instance 'simple-dispatcher
                  :url-string ,url-regexp
                  :handler (lambda ()
                             ,@body)))

(defclass url-dispatcher (abstract-dispatcher url-matcher entry-point-handler)
  ()
  (:default-initargs :priority +url-dispatcher-default-priority+))

(defmacro make-url-dispatcher (url-string &body body)
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
  (:default-initargs :priority +regex-dispatcher-default-priority+))

(defprint-object (dispatcher url-dispatcher)
  (format *standard-output* "~S" (url-string dispatcher)))

(defmacro make-regexp-dispatcher (url-regexp &body body)
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

;;;; ** CLIENT-STATE-DISPATCHER

(defclass client-state-dispatcher (abstract-dispatcher session-frame-and-url-matcher)
  ()
  (:default-initargs :url-string +client-state-dispatcher-url+
                     :priority +client-state-dispatcher-default-priority+)
  (:documentation "This dispatcher is a sink for requests pushing
client side state to the server."))

(defmethod handler-handle ((dispatcher client-state-dispatcher)
			   (application standard-application)
			   (context standard-request-context)
			   matcher-result)
  (destructuring-bind (session frame &rest args) matcher-result
    (declare (ignore args))
    (disallow-response-caching (context.response context))
    (ensure-session application context session)
    (call-callbacks frame (context.request context))))

;;;; ** ACTION-DISPATCHER

(defclass action-dispatcher (abstract-dispatcher session-frame-matcher)
  ()
  (:default-initargs :priority +action-dispatcher-default-priority+)
  (:documentation "This is the core dispatcher for ucw. Due to
  how ucw's compute-url method works it is important that the
  action-dispatcher be checked before any url-dispatchers, so it
  should be the first element in an application's dispatcher
  list."))

;; TODO there's a race condition: between matching the action and handling it (probably the
;; application should be kept locked up til the point when action is ready to be called. then lock
;; the session and release the app.)
(defmethod matcher-match ((matcher action-dispatcher)
			  (application standard-application)
			  (context standard-request-context))
  (ucw.rerl.dispatcher.dribble "~S trying to match as action-dispatcher action-id is ~S"
                               matcher (find-action-id context))
  (multiple-value-bind (matchesp session frame) (call-next-method)
    (when matchesp
      (when-bind action (find-action frame (find-action-id context))
        (let* ((action-invocation-id (find-action-invocation-id *context*))
               (action-is-ok (and (action-valid-p action)
                                  (or (not (action-invocation-isolated-p action))
                                      (not (find action-invocation-id (action-invocation-ids action)
                                                 :test #'string=))))))
          (ucw.rerl.dispatcher.dribble "Matched action ~S. Will execute it? ~A. Invocation id in the request is ~S, past invocations are ~S"
                                       action
                                       (if action-is-ok "Yes" "No")
                                       action-invocation-id (action-invocation-ids action))
          ;; TODO (attila) : somehow report when this action is not valid anymore.
          ;; we should mark in the action-entry that the first invocation of an isolated action is still running
          ;; and answer a page telling this fact to the user. then keep reloading that page until the
          ;; first invocation has finished and recorded the response in the action-entry.
          ;; then all subsequent invocations should blindly return that response without calling the action again.
          (when action-is-ok
            (values t session frame action)))))))

(defmethod handler-handle ((dispatcher action-dispatcher)
			   (application standard-application)
			   (context standard-request-context)
			   matcher-result)
  (destructuring-bind (session frame action) matcher-result
    (ensure-session application context session)
    (handle-action application session frame action :call-callbacks-p t)))

(defun handle-action (application session frame action &key call-callbacks-p)
  (with-lock-held-on-session session
    (assert (and action
                 (action-valid-p action))) ;; and it's not a invocation-isolated action or it's coming with a new invocation id. but it's a costly check...
    (let ((callbacks-called nil)
          (render-loop-called nil)
          (action-lambda (action-lambda action))) ; TODO clean up when drewc committed the funcallable stuff
      (flet ((ensure-frame-was-reinstated ()
               (when (and frame
                          (not (eq frame (context.current-frame *context*))))
                 (setf (context.current-frame *context*) frame)
                 (ucw.rerl.dispatcher.debug "Reinstating backtracked places to frame ~S" frame)
                 (reinstate-backtracked frame))))
        (ucw.rerl.dispatcher.debug "Handling action ~S in session ~S frame ~S call-callbacks-p ~S" action session frame call-callbacks-p)

        (when (action-isolated-p action)
          (setf (action-valid-p action) nil))

        (ensure-frame-was-reinstated)

        (when call-callbacks-p
          (setf action-lambda (rebind (action-lambda)
                                (lambda ()
                                  (setf callbacks-called (call-callbacks frame (context.request *context*)))
                                  (ucw.rerl.dispatcher.debug "Called call-callbacks, it found and executed ~S callbacks" callbacks-called)
                                  (funcall action-lambda)))))

        (let ((action-invocation-id (find-action-invocation-id *context*)))
          (when (action-make-new-frame-p action)
            (make-new-frame (context.session *context*)))
          (when (action-invocation-isolated-p action)
            (push action-invocation-id (action-invocation-ids action)))
          (handle-action-in-session action-lambda application session frame))

        ;; nb: don't use the same frame as returned by make-new-frame
        ;; to get the current component. 99% of the time they will be
        ;; the same as below, but 1% they will be differenet and that
        ;; 1% of the time is when all hell has broken lose and we're
        ;; dancing with cats...
        (when (action-call-render-loop-p action)
          (render-loop (frame.window-component (context.current-frame *context*)))
          (setf render-loop-called t)))

      (ucw.rerl.dispatcher.debug "Saving backtracked places into frame ~S after calling the action and ~S callbacks"
                                 (context.current-frame *context*) callbacks-called)

      (save-backtracked (context.current-frame *context*))

      (when (action-manage-response-p action)
        (flush-request-response *context*))

      (values callbacks-called render-loop-called))))


;;;; ajax-action-dispatcher

(defclass ajax-action-dispatcher (action-dispatcher starts-with-url-matcher)
  ()
  (:default-initargs :priority +ajax-action-dispatcher-default-priority+
                     :url-string +ajax-action-dispatcher-url+)
  (:documentation "This is specialized action dispatcher to handle ajax requests."))

(defmethod matcher-match ((matcher ajax-action-dispatcher)
			  (application standard-application)
			  (context standard-request-context))
  (ucw.rerl.dispatcher.dribble "~S trying to match as ajax-action-dispatcher" matcher)
  (when (starts-with (query-path-sans-prefix context) (url-string matcher))
    (multiple-value-bind (matchesp session frame action) (call-next-method)
      (declare (ignore matchesp))
      (values t session frame action))))

(defmethod handler-handle ((dispatcher ajax-action-dispatcher)
			   (application standard-application)
			   (context standard-request-context)
			   matcher-result)
  (destructuring-bind (session frame action) matcher-result
    (disallow-response-caching (context.response context))
    (if (and session frame action)
        (progn
          (ensure-session application context session)
          (handle-action application session frame action :call-callbacks-p t))
        (handle-ajax-request ()
          (abort-ajax-request "Couldn't identify some of the session, frame or action.")))))

;;;; ** reload-dispatcher

(defclass reload-dispatcher (abstract-dispatcher session-frame-matcher)
  ()
  (:default-initargs :priority +reload-dispatcher-default-priority+
                     :frame-is-optional-p t)
  (:documentation "This dispatcher looks for a valid session and calls render-loop when one was found."))

(defmethod matcher-match ((matcher reload-dispatcher)
			  (application standard-application)
			  (context standard-request-context))
  (let ((session-id (find-session-id context))
        (frame-id (find-frame-id context)))
    (ucw.rerl.dispatcher.dribble "~S trying to match as reload-dispatcher, session-id is ~S, frame-id is ~S"
                                 matcher session-id frame-id)
    (multiple-value-bind (matchesp session frame) (call-next-method)
      (if (and matchesp
               (session.current-frame session))
          (values t session frame)
          (when (and session-id frame-id)
            ;; if someone followed a link to a non-existent session/frame in this app
            ;; then answer a redirect to the request url without the session/frame params (see below)
            (values t nil nil))))))

(defmethod handler-handle ((dispatcher reload-dispatcher)
			   (application standard-application)
			   (context standard-request-context)
			   matcher-result)
  (destructuring-bind (session frame) matcher-result
    (if session
        (progn
          (ensure-session application context session)
          (with-lock-held-on-session session
            (when (and frame
                       (not (eq frame (context.current-frame context))))
              (setf (context.current-frame context) frame)
              (ucw.rerl.dispatcher.debug "Before calling render-loop reinstating backtracked places to frame ~S" frame)
              (reinstate-backtracked frame))
            (render-loop (context.window-component context))
            (flush-request-response context)))
        (let* ((query-path (query-path (context.request context)))
               (redirect-path (aif (position #\? query-path :test #'char=)
                                   (subseq query-path 0 it)
                                   query-path)))
          (ucw.rerl.dispatcher.debug "~S got a request into a non-existent session and/or frame, sending a redirect to ~S"
                                     dispatcher redirect-path)
          ;; TODO we should only strip the session/frame/action parameters from the query
          (send-redirect redirect-path (context.response context))
          (flush-request-response context)))))

;;;; polling-dispatcher

(defclass polling-dispatcher (abstract-dispatcher starts-with-url-matcher)
  ()
  (:default-initargs :url-string +polling-dispatcher-url+
                     :priority +polling-dispatcher-default-priority+)
  (:documentation "This dispatcher is handling the polling requests."))

(defmethod matcher-match ((matcher polling-dispatcher)
			  (application standard-application)
			  (context standard-request-context))
  (multiple-value-bind (matchesp url-prefix) (call-next-method)
    (declare (ignore url-prefix))
    (when matchesp
      (ucw.rerl.dispatcher.dribble "~S matched the url, extracted session id is ~S" matcher (find-session-id context))
      (let ((session (find-session application context))
            (frame nil))
        (when session
          (ucw.rerl.dispatcher.dribble "~S matched the session" matcher)
          (ensure-session application context session)
          (when-bind frame-id (find-frame-id context)
            (setf frame (find-frame-by-id session frame-id))))
        (ucw.rerl.dispatcher.debug "~S matched session ~S and frame ~S" matcher session frame)
        (values t session frame)))))

(defmethod handler-handle ((dispatcher polling-dispatcher)
			   (application standard-application)
			   (context standard-request-context)
			   matcher-result)
  (destructuring-bind (session frame) matcher-result
    (ucw.rerl.dispatcher.dribble "~S handling the request in session ~S frame ~S" dispatcher session frame)
    (disallow-response-caching (context.response context))
    (if session
        (handle-polling-of-session application session frame)
        (send-polling-response-to-unknown-session application))))

;;;; *** Tal Directory dispatcher

(defclass tal-dispatcher (abstract-dispatcher tal-matcher tal-handler)
  ()
  (:default-initargs :priority +tal-dispatcher-default-priority+)
  (:documentation "This dispatcher simply serves up TAL pages.
Tal pages can refer to the session object (and it will be
maintained across requests) but must avoid using call/answer or
actions."))

(defclass parenscript-dispatcher (abstract-dispatcher url-matcher parenscript-handler)
  ((compile-args :accessor compile-args :initarg :compile-args :initform '())
   (parenscript-file :accessor parenscript-file :initarg :parenscript-file))
  (:default-initargs :priority +parenscript-dispatcher-default-priority+)
  (:documentation "This handler is used to serve a
compiled javascript string from the given PARENSCRIPT-FILE.

The file is compiled lazily on the first request and in-package
forms are working as expected. Each form is evaluated, so using
backquote is possible and in fact a must. The following dynamic
variables are bound when compiling:
  *context* - the current request context
              (hint: (context.application *context*))"))

(defprint-object (self parenscript-dispatcher)
  (when (cachep self)
    (princ "cached")
    (princ " "))
  (princ (parenscript-file self))
  (princ " ")
  (princ (last-compiled self)))

(defmethod parenscript-handler-compile ((self parenscript-dispatcher))
  (apply #'js:compile-parenscript-file-to-string (parenscript-file self) (compile-args self)))

(defmethod parenscript-handler-timestamp ((self parenscript-dispatcher))
  (file-write-date (parenscript-file self)))

(defun make-parenscript-dispatcher (url-regex parenscript-file &rest args &key
                                              (cache t)
                                              (priority +parenscript-dispatcher-default-priority+)
                                              &allow-other-keys)
  "Return a parenscript-dispatcher"
  (remf-keywords args :cache)
  (make-instance 'parenscript-dispatcher
		 :url-string url-regex
		 :parenscript-file parenscript-file
                 :compile-args args
                 :priority priority
		 :cache cache))

(defclass i18n-parenscript-dispatcher (abstract-dispatcher url-matcher parenscript-handler)
  ((locale-to-js-map :initform (make-hash-table :test 'equal) :accessor locale-to-js-map-of)
   (last-purged-timestamp :initform (get-universal-time) :accessor last-purged-timestamp-of)
   (purge-interval :initform #.(* 60 60 24) :initarg :purge-interval :accessor purge-interval-of))
  (:default-initargs :priority +parenscript-dispatcher-default-priority+)
  (:documentation "This handler is used to serve a
compiled javascript string from *js-resource-registry*."))

(defmethod parenscript-handler-timestamp ((self i18n-parenscript-dispatcher))
  *js-resource-registry-last-modified*)

(defmethod parenscript-handler-compile ((self i18n-parenscript-dispatcher))
  (let* ((forms `(ucw.i18n.define
                  ,@(iter (for (name) in-hashtable *js-resource-registry*)
                          (collect (string-downcase (string name)))
                          (collect (localize name)))))
         (result (js:js* forms)))
    (ucw.rerl.dispatcher.info "~S is compiling for key ~S" self (i18n-parenscript-dispatcher-cache-key))
    (ucw.rerl.dispatcher.debug "~S at key ~S compiled ~S" self (i18n-parenscript-dispatcher-cache-key) result)
    result))

(defun i18n-parenscript-dispatcher-cache-key ()
  (mapcar #'locale-name (ensure-list *locale*)))

(defmethod cached-javascript ((self i18n-parenscript-dispatcher))
  (let* ((key (i18n-parenscript-dispatcher-cache-key))
         (result (gethash key (locale-to-js-map-of self))))
    (ucw.rerl.dispatcher.debug "~S ~A cached js with key ~S"
                               self (if result "found" "missed") key)
    result))

(defmethod (setf cached-javascript) (value (self i18n-parenscript-dispatcher))
  (when (> (get-universal-time)
           (+ (last-purged-timestamp-of self)
              (purge-interval-of self)))
    (ucw.rerl.dispatcher.info "~S is purging its cache" self))
  (let ((key (i18n-parenscript-dispatcher-cache-key)))
    (ucw.rerl.dispatcher.debug "~S is storing cached js with key ~S" self key)
    (setf (gethash key (locale-to-js-map-of self)) value)))

(defun make-standard-ucw-dispatchers ()
  (list (make-instance 'ajax-action-dispatcher)
        (make-instance 'action-dispatcher)
        (make-instance 'client-state-dispatcher)
        (make-instance 'polling-dispatcher)

        ;; the reload-dispatcher is only usable in a mod_lisp setup, where published directories get
        ;; processed first by apache, because the reload-dispatcher aggressively mathes any request
        ;; from which it can identify a valid session and call render-loop. apache should be set up
        ;; so that requests targetting static content doesn't even reach ucw (see the included
        ;; example apach config in ucw/etc).
        (make-instance 'reload-dispatcher)
        
        (make-instance 'i18n-parenscript-dispatcher
                       :url-string +i18n-parenscript-dispatcher-url+)
        (make-parenscript-dispatcher (map-to-dynamic-ucw-url "js/per-application.js")
          (merge-pathnames #P"src/per-application-parenscript.lisp"
           (asdf:component-pathname (asdf:find-system :ucw)))
          :eval-forms-p t)
        (make-parenscript-dispatcher (map-to-dynamic-ucw-url "js/functional.js")
          (merge-pathnames #P"src/lib/functional.lisp"
            (asdf:component-pathname (asdf:find-system :parenscript))))))

;; TODO drop these eventually
(defun parenscript-dispatcher (&rest args)
  (ucw.rerl.dispatcher.warn "parenscript-dispatcher has been replaced by make-parenscript-dispatcher. it will be removed eventually...")
  (apply #'make-parenscript-dispatcher args))
(defmacro regexp-dispatcher (&rest args)
  (ucw.rerl.dispatcher.warn "regexp-dispatcher has been replaced by make-regexp-dispatcher, it will be removed eventually...")
  `(make-regexp-dispatcher ,@args))
(defmacro url-dispatcher (&rest args)
  (ucw.rerl.dispatcher.warn "url-dispatcher has been replaced by make-url-dispatcher, it will be removed eventually...")
  `(make-url-dispatcher ,@args))
(defmacro simple-dispatcher (&rest args)
  (ucw.rerl.dispatcher.warn "simple-dispatcher has been replaced by make-simple-dispatcher, it will be removed eventually...")
  `(make-simple-dispatcher ,@args))
(defmacro minimal-dispatcher (&rest args)
  (ucw.rerl.dispatcher.warn "minimal-dispatcher has been replaced by make-minimal-dispatcher, it will be removed eventually...")
  `(make-minimal-dispatcher ,@args))

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
