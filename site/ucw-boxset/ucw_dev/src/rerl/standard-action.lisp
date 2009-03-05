;; -*- lisp -*-

(in-package :it.bese.ucw)

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
position. The new component musttherefore remember to render as
an entirea html page.

This is usefull for dealing with modal componnet like alerts and
dialogs which must be dealty with before the user can continue."
  `(call-component (context.window-component *context*)
                   (make-instance ,component-type ,@component-init-args)))

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

(defun action-href (action-lambda
                    &key (component (context.window-component *context*))
                         (frame (context.current-frame *context*)))
  "Given a lambda returns an URL (as a string) which will call
the lambda.

The COMPONENT parameter is passed directly to COMPUTE-URL, FRAME
is passed to MAKE-NEW-ACTION."
  (print-uri-to-string
   (compute-url component
                :action-id (make-new-action frame action-lambda))))

(defmacro action-href-body ((&key (component '(context.window-component *context*))
                                  (frame '(context.current-frame *context*)))
                            &body body)
  `(action-href
    (lambda ()
      (arnesi:with-call/cc
        ,@body))
    :component ,component :frame ,frame))

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
