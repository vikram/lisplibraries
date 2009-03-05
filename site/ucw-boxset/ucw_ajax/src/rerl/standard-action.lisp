;; -*- lisp -*-

(in-package :it.bese.ucw)

(enable-bracket-reader)

;;;; ** Call, Answer, Defaction and Defentry-Point

;;;; Call/Answer vodoo

(defmacro call (component-type &rest component-init-args)
  "Stop the execution of the current action and pass control to
a freshly created component of type COMPONENT-TYPE.

COMPONENT-INIT-ARGS are passed directly to the underlying
make-instance call. This form will return if and when the call'd
component calls answer, the value returned by this form is
whatever the call'd component passed to answer.

Notes:

This macro assumes that the lexcial variable UCW:SELF is bound to
the calling component."
  (rebinding (component-type)
    `(etypecase ,component-type
       ((or symbol standard-component-class)
        (call-component self (make-instance ,component-type ,@component-init-args)))
       (component
        (call-component self ,component-type)))))

(defmacro jump (component-type &rest component-init-args)
  `(jump-to-component (make-instance ,component-type ,@component-init-args)))

(defmacro answer (value)
  "Return control to the calling component passing it VALUE.

Calls to the answer macro should be in a tail position."
  `(answer-component self ,value))

(defmacro call-as-window (component-type &rest component-init-args)
  "Just like CALL but the new component is used for the entire
window. In other words the new component will be used to render
the entire browser window, independant of the current component's
position. The new component must therefore remember to render as
an entire html page.

This is useful for dealing with modal component like alerts and
dialogs which must be dealt with before the user can continue."
  `(call-component (context.window-component *context*)
                   (make-instance ,component-type ,@component-init-args)))

(defmethod handle-action-in-session ((action function) (application standard-application) session frame)
  (with-action-error-handler ()
    (ucw.rerl.dispatcher.dribble "About to call the action")
    (funcall action)))

;;;; Binding request params to variables

(defmacro with-request-params (request-lambda-list request &body body)
  "Bind, according the REQUEST-LAMBDA-LIST the parameters in
  REQUEST and execute BODY.

REQUEST-LAMBDA-LIST is a list of the form:

 ( [ ( symbol string ) | symbol ]
   [ default-value [ supplied-symbol-name ]? ]? )

If the request contains a param (no distinction between GET and
POST params is made) named STRING (which defaults to the symbol
name of SYMBOL) the variable SYMBOL is bound to the associated
value (which is always a string) . If no parameter with that name
was passed SYMBOL will be bound to DEFAULT-VALUE and the variable
named SUPPLIED-SYMBOL-NAME will be bound to NIL.

NB: Parameter names are matched case insensitively."
  (gen-request-param-binder request-lambda-list request body))

(defstruct arg-spec
  symbol
  name-string
  default-value
  supplied-symbol-name)

(defun gen-request-param-binder (args request body)
  (let ((args (mapcar (lambda (arg-spec)
                        (destructuring-bind ((name-symbol &optional name-string) &optional default-value supplied-p)
                            arg-spec
                          (make-arg-spec :symbol name-symbol
                                         :name-string (if name-string
                                                          name-string
                                                          (string-downcase (string name-symbol)))
                                         :default-value default-value
                                         :supplied-symbol-name supplied-p)))
                      (mapcar (lambda (arg-spec)
                                "normalize the arg specs"
                                (cons (ensure-list (car (ensure-list arg-spec)))
                                      (cdr (ensure-list arg-spec))))
                              args))))
    (let ((req (gensym)))
      `(let ((,req ,request))
         (declare (ignorable ,req))
         (let
             ;; bind the vars to their default values
             ,(iterate
                 (for arg-spec in args)
                 (collect (list (arg-spec-symbol arg-spec)
                                (arg-spec-default-value arg-spec)))
                 (awhen (arg-spec-supplied-symbol-name arg-spec)
                   (collect (list it nil))))
           ,@(mapcar (lambda (arg-spec)
                       `(awhen (get-parameter (context.request *context*) ,(arg-spec-name-string arg-spec))
                          (setf ,(arg-spec-symbol arg-spec) it)
                          ,(awhen (arg-spec-supplied-symbol-name arg-spec)
                             `(setf ,it t))))
                   args)
         ,@body)))))

;;;; Defining actions and entry points

(defmacro %defaction (name &rest args)
  "Defines an action method named NAME."
  (let ((qualifier '()))
    (when (symbolp (first args))
      (setf qualifier (list (pop args))))
    (destructuring-bind (((self self-class) &rest other-args) &body body)
        args
      `(defmethod/cc ,name ,@qualifier ((,self ,self-class) ,@other-args)
                     ;; extract declares and docstirngs from BODY and put them here
                     ,@(iterate (for form in body)
                                (while (or (stringp form) 
                                           (and (listp form) (eql 'cl:declare (car form)))))
                                (collect form)
                                (pop body))
                     (let ((self ,self))
                       (ucw.component.action.info
                        (format nil "Serving action (~{/~S~}/~A::~A ~S~{ ~S~})"
                                (call-request-path self)
                                ,(swank::shortest-package-nickname (symbol-package name))
                                ,(string name)
                                self   
                                (list ,@(arnesi:extract-argument-names other-args :allow-specializers t))))       
                       (block ,name
                         ,@body))))))

(defmacro defaction (name &rest rest)
  (if (equal (car rest) :isolate)
      (with-unique-names (memo-id memo memo-present-p) 
	`(defaction ,name ,(second rest)
	  (let ((,memo-id
		 (strcat (mapcar (rcurry #'funcall *context*)
				 (list #'find-session-id  
				       #'find-frame-id
				       #'find-action-id)))))
	  (multiple-value-bind (,memo ,memo-present-p)
	      (gethash ,memo-id (component.isolate-hash ,(caar (second rest))))
	    (if  ,memo-present-p
		 ,memo
		 (setf (gethash ,memo-id (component.isolate-hash ,(caar (second rest))))
		       (progn ,@ (cddr rest))))))))
      
      `(%defaction ,name ,@rest)))

(defmacro defentry-point (url
                          (&key application
                                (class 'url-dispatcher)
                                (priority nil priority-p))
                          request-lambda-list &body body)
  "Define an entry point bound to the url URL of type CLASS.

URL must be a string which, when appended to APPLICATION's
url-prefix, specifies the complete url for this entry
point. CLASS is a symbol naming the dispatcher class.

APPLICATION (evaluated) specifies the application object this
entry point will be attached to. If NIL *default-application*
will be used, otherwise it must be the name of an existing
application.

REQUEST-LAMBDA-LIST is a request lambda list as per
WITH-REQUEST-PARAMS.

The body of the entry-point is executed whenever the server
recieves a request for the specified url. The body can contain
calls to components but must not contain answer forms.

If the backend supports it (araneida and aserve) then the url is
automatically registered with the backend, otherwise (mod_lisp)
developers must manually configure the backend."
  (with-unique-names (app)
    (assert (stringp url)
            (url)
            "Entry point urls must be strings, ~S is not allowed." url)
    `(let ((,app (or ,application *default-application*)))
       (if ,app
           (setf
            ;; first delete any entry points of type CLASS with
            ;; url-string URL
            (application.dispatchers ,app)
            (delete-if (lambda (ep)
                         (and (eql (class-name (class-of ep)) ',class)
                              (string= ,url (slot-value ep 'url-string))))
                       (application.dispatchers ,app))
            ;; now add the entry point
            (application.dispatchers ,app)
            (append (application.dispatchers ,app)
                         (list
                          (make-instance ',class
                                         ,@(when priority-p `((:priority ,priority)))
                                         :url-string ,url
                                         :handler (lambda ()
                                                    (with-call/cc
                                                      (let ((self nil))
                                                        (with-request-params ,request-lambda-list
                                                            (context.request *context*)
                                                          ,@body))))))))
           ,(if application
                `(error "No application named ~S found." ',application)
                `(error "No application specified and *DEFAULT-APPLICATION* is NIL.")))
       ,url)))

(defun action-href (action
                    &key (component (context.window-component *context*))
                         (frame (context.current-frame *context*)))
  "Given an action returns an URL (as a string) which will call
the action lambda.

The COMPONENT parameter is passed directly to COMPUTE-URL, FRAME
is passed to MAKE-NEW-ACTION. ACTION may be a string, a lambda
or an action-entry made with make-action."
  (declare (type (or action-entry function) action))
  (let ((uri (if (and (action-entry-p action)
                      (action-ajax-p action))
                 (let ((uri (compute-url (context.application *context*)
                                         :action-id (action-id action))))
                   (setf (uri.path uri) (strcat (uri.path uri) +ajax-action-dispatcher-url+))
                   uri)
                 (compute-url component
                              :action-id (when action
                                           (etypecase action
                                             (function (action-id
                                                        (register-action (:frame frame :with-call/cc nil)
                                                          (funcall action))))
                                             (action-entry (action-id action))))))))
    (values (print-uri-to-string uri) uri)))

(defmacro action-href-body ((&rest args &key (component '(context.window-component *context*))
                                             (frame '(context.current-frame *context*))
                                             &allow-other-keys)
                            &body body)
  (remf-keywords args :frame :component)
  `(action-href (register-action ,args ,@body) :component ,component :frame ,frame))

(defun abort-raw-request ()
  (throw 'abort-raw-request nil))

(defmacro handle-raw-request ((&key (content-type "application/octet-stream")
                                    content-disposition
                                    expires-in-ut
                                    max-age-in-secs
                                    (cache (or expires-in-ut max-age-in-secs))
                                    (with-network-stream nil)
                                    (with-yaclml-stream nil))
                              &body body)
  "This macro handles a raw request. Will set the http cache control headers
according to the supplied arguments, send the headers, execute the BODY and
close the request. WITH-NETWORK-STREAM will be bound to the network stream
unless it's nil. When WITH-YACLML-STREAM is not nil the body will run inside a
with-yaclml-stream block and after it finished it will be properly encoded
and written into the network stream. Additionally if it is a symbol then
it will be bound with that name."
  (with-unique-names (request response)
    `(let* ((,request (context.request *context*))
            (,response (context.response *context*))
            ,@(when with-network-stream
                    `((,with-network-stream (network-stream ,request))))
            ,@(when with-yaclml-stream
                    (when (eq with-yaclml-stream t)
                      (setf with-yaclml-stream (gensym "YACLML-STREAM")))
                    `((,with-yaclml-stream (make-string-output-stream)))))
      (setf (get-header ,response "Status") "200 OK"
            (get-header ,response "Content-Type") ,content-type)
      ,(when content-disposition
         `(setf (get-header ,response "Content-Disposition") ,content-disposition))
      ,(if cache
           `(progn
             ,@(when max-age-in-secs
                    `((setf
                       (get-header ,response "Cache-Control")
                       (strcat "max-age=" (princ-to-string ,max-age-in-secs)))))
             ,@(when expires-in-ut
                    `((setf
                       (get-header ,response "Expires")
                       (date:universal-time-to-http-date ,expires-in-ut)))))
           `(setf
             (get-header ,response "Cache-Control") "no-cache, no-store"
             (get-header ,response "Expires") #.(date:universal-time-to-http-date
                                                 3000000000))) ; somewhere in 1995
      (catch 'abort-raw-request
        (handler-bind ((stream-error (lambda (c)
                                       (when (eq (stream-error-stream c)
                                                 (network-stream ,request))
                                         (abort-raw-request)))))
          ,(if with-yaclml-stream
                `(progn
                  (yaclml::with-yaclml-stream ,with-yaclml-stream
                    ,@body)
                  (send-headers ,response)      
                  (write-sequence (string-to-octets (get-output-stream-string ,with-yaclml-stream)
                                   (encoding ,response))
                   (network-stream ,request)))
                `(progn
                  (send-headers ,response)
                  ,@body))))
      (close-request ,request))))

(defun abort-ajax-request (&optional message)
  (throw 'abort-ajax-request message))

;; TODO, attila: the encoding in the default xml header could be taken from (encoding (context.response *context*))
;; is there a function that converts to the appropiate format?
(defmacro handle-ajax-request ((&key (xml-header "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>")
                                     (content-type "text/xml")
                                     (with-yaclml-stream t))
                               &body body)
  "This macro handles an AJAX request: disable caching, send the headers,
write XML-HEADER if CONTENT-TYPE is \"text/xml\", run the BODY in
with-yaclml-stream and close the request. Take care of the xml
encoding in the default header, change as needed."
  (unless with-yaclml-stream
    (error "WITH-YACLML-STREAM must be either t or a symbol to be used to name the stream binding"))
  `(handle-raw-request (:content-type ,content-type
                        :with-yaclml-stream ,with-yaclml-stream :cache nil)
    ,@(when (and (stringp content-type)
                 (string= content-type "text/xml"))
            `((<:as-is ,xml-header)))
    {with-xml-syntax
      <answer
        (let ((successp nil)
              (message nil)
              (yaclml-body))
          (unwind-protect
               (setf message (catch 'abort-ajax-request
                               (setf yaclml-body
                                     (with-yaclml-output-to-string
                                       ,@body
                                       (when-bind session (and (not (eq *context* :unbound))
                                                               (context.session *context*))
                                         (when (has-events-for-the-client session)
                                           (send-events-to-the-client session)))))
                               (setf successp t)))
            (if successp
                (progn
                  (ucw.rerl.ajax.debug "Successfully rendered ajax answer, body length is ~S" (length yaclml-body))
                  (<:as-is yaclml-body)
                  <result "success">)
                (progn
                  (ucw.rerl.ajax.debug "Failed to rendered ajax answer, error message is ~S" message)
                  <result "failure">
                  (when message
                    <error-message (<:as-html message)>)))))>}))

(defun call-callbacks-action-href ()
  ;; this is suboptimal here, it could be a constant, but...
  (action-href (register-ajax-action (:invocation-isolated nil)
                 ;; nop: we just have the callbacks called
                 )
               :component (context.window-component *context*)))


;; Copyright (c) 2003-2005 Edward Marco Baringer
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
