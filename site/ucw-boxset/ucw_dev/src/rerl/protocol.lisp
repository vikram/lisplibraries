;;; -*- lisp -*-

(in-package :it.bese.ucw)

;;;; * The RERL Protocol

;;;; A UCW server sits in a tight loop waiting for incoming HTTP
;;;; requests, elaborating them and then sending out the HTTP
;;;; responses. This loop is called the "RERL" (Request-Eval-Response
;;;; Loop) and the objects, methods and variables involved in the RERL
;;;; are described here.

;;;; ** Components

(defclass component ()
  ()
  (:documentation "The generic super class of all components."))

(defgeneric (setf component.calling-component) (caller component))

(defgeneric component.continuation (component)
  (:documentation "Accessor for COMPONENT's continuation.

The continuation is implemented as a 1 arg lambda which is called
with the value COMPONENT answers. and continues whatever was
stopped when control was transfered to the component."))

(defgeneric (setf component.continuation) (k component))

(defgeneric/cc call-component (caller callee)
  (:documentation "Transfer control from the component CALLER, to
  the component CALLEE.

CALLEE should replace CALLER in the user interface (by setting
itself in CALLER's place). CALLEE can return control to CALLER by
calling answer-component.

call-component can be called by user code even though it will
generally be called through the CALL macro."))

(defgeneric was-called-p (component)
  (:documentation "Returns whether COMPONENT has been called (so we can answer)"))

(defgeneric call-request-path (component)
  (:documentation "Return current request path as a list for reporting."))

(defgeneric answer-component  (callee value)
  (:documentation "Return control to CALLEE's CALLER, continuing
  with the value VALUE.

answer-component can be called by user code though it will
generally be called through the ANSWER macro."))

(defvar *current-component* :unbound
  "When rendering this is bound to the current component (the
  second argument passed to render).")

(defgeneric parent (component)
  (:documentation "Returns the parent of COMPONENT.

Only window components and detached components may return NIL
from this function."))

(defgeneric inspectable-component (component)
  (:documentation "Returns T when inspector links should be
  created for this component.

If methods on this function need a default value (iow the user
hasn't explicitly specified a preference) the return value of the
generic function INSPECT-COMPONENTS (passed the current
application) should be used."))

;;;; ** The rendering protocol

(defgeneric render (component)
  (:documentation "The generic entry point for rendering
  components to the user.")
  (:method-combination wrapping-standard))

(defgeneric compute-url (component &key action-id)
  (:documentation "Return a URL with the proper session, frame
  and action-id parameters."))

(defgeneric update-url (component url)
  (:documentation "Prepare URL for rendering as action urls.

Each component can define a method on UPDATE-URL to add
component-specific parameters. This is generally coupled with a
url-dispatcher to create bookmark-able urls."))

;;;; ** Servers

(defvar *default-server* nil
  "The server object to use when none is explicitly specified.")

(defclass server ()
  ()
  (:documentation "A single UCW server.

Within the RERL server objects provide the following
functionality:

* Take request and response objects as provided by the backend
  via the handle-request method and run one iteration of the
  RERL.

* Call the generic function associated-application to determine
  the application object which should handle the request.

* Create a request-context object (via a call to
  make-request-context on the application) and initialize it.

* Call the application's service method passing it the
  request-context.

* close the request and response when the application's
  service method returns.

The server object should also attempt to deal with all conditions
signalled during the service'ing of the request.

Servers are responsible for managing multiple applications within
the same UCW instance and dealing with, possibly, multiple
backends."))

(defgeneric server.applications (server)
  (:documentation "Return a list of all the application objects
  SERVER manages."))

(defgeneric startup-server (server)
  (:documentation "Make SERVER start responding to requests."))

(defgeneric shutdown-server (server)
  (:documentation "Make SERVER stop responding to requests and
  close/release and resources."))

(defgeneric associated-application (server request)
  (:documentation "Return the application object which will
  handle REQUEST."))

(defgeneric handle-request (server request response)
  (:documentation "Perform one iteration of the RERL on REQUEST and RESPONSE.

Methods defined on this generic function must be built from the
methods associated-application and service and should, as much as
possible, handle all conditions signalled by calls to service."))

;;;; ** Applications

(defvar *default-application* nil
  "The application object to use when none is explicitly specified.")

(defmacro in-application (application)
  `(setf *default-application* ,application))

(defclass application ()
  ()
  (:documentation "A UCW application.

Application objects are responsible for:

- Managing a collection of sessions and finding the right session for a given request.

- Managing session life time (session creation and expiration).

- Creating request contexts.

- Managing dispatchers and associating them to request objects.

- Creating http query paths which subsequently (when requested)
  call action lambdas.

Since application objects are used to create sessions (via
make-new-session) they are also important when customizing the
functionality of the RERL."))

(defgeneric application.url-prefix (application)
  (:documentation "Returns the url prefix (a string) for APPLICATION.

The URL prefix is that string which, when used as the prefix of
an incoming http query path, identifies an application."))

(defgeneric make-request-context (application request response)
  (:documentation "Create a new request-context form REQUEST and RESPONSE.

The returned context will have its application, request and
response slots set to APPLICATION, REQUEST and RESPONSE.

This method need not neccessarily return a new object each time
but may reuse previouly created objects in which case a call to
clear-context must be made before returning the object."))

(defgeneric find-session (application context)
  (:documentation "Return the session object in CONTEXT or NIL if
  there's no session."))

(defgeneric make-new-session (application)
  (:documentation "Return a new session object."))

(defgeneric remove-expired-sessions (application)
  (:documentation "Remove all the expired sessions in APPLICATION.

Implementations of this method must use the generic function
EXPIREDP to determine if a session is expired or not, and must
call DELETE-SESSION on the session objects."))

(defgeneric delete-session (application session &optional expire)
  (:documentation "Remove the session SESSION from APPLICATION.

If EXPIRE is T then EXPIRE-SESSION MUST be called on the
session."))

(defgeneric startup-application (application)
  (:documentation "Start the application's request-eval-response loop."))

(defgeneric shutdown-application (application)
  (:documentation "Terminate the application's life cycle.

Release any and all resources held by APPLICATION. The value
returned by the generic function is unspecified."))

(defgeneric debug-on-error (application)
  (:documentation "Returns T if APPLICATION should attempt to
  debug errors (instead of just returning an error page)."))

;;;; ** Request Contexts

(defclass request-context ()
  ((cookies))
  (:documentation "The generic super class of all request contexts.

A request-context object contains all the information associated
with one particular request/response loop. The request context is
usually accessed via the special variable *context* and contains
the currently relevant application, session, session-frame,
request and response object."))

(defgeneric context.request (context)
  (:documentation "Accessor for the request object in CONTEXT."))
(defgeneric (setf context.request) (request context))

(defgeneric context.response (context)
  (:documentation "Accessor for the response object in CONTEXT."))
(defgeneric (setf context.response) (response context))

(defgeneric context.application (context)
  (:documentation "Accessor for the application object in CONTEXT."))
(defgeneric (setf context.application) (application context))

(defgeneric context.session (context)
  (:documentation "Accessor for the session object in CONTEXT."))
(defgeneric (setf context.session) (session context))

(defgeneric context.current-frame (context)
  (:documentation "Return the \"current\" (most recent)
  session-frame in CONTEXT's session.")
  (:method ((context request-context))
    "Simply call session.current-frame on context's session object."
    (session.current-frame (context.session context))))

(defgeneric context.window-component (context)
  (:documentation "Return the \"current\" (most recenct) window
  component in CONTEXT's session."))

(defgeneric (setf context.window-component) (component context))

(defgeneric context.cookies (context)
  (:documentation "Return the parsed rfc2109:cookie structs"))

(defgeneric context.cookie-value (context cookie-name)
  (:documentation "Return the value of the named cookie"))

(defgeneric find-session-id (context)
  (:documentation "Returns the client supplied session-id in CONTEXT.

Methods should inspect the context's request object and return a
string specifying the session-id. No guarantee on the
validity (well-formedness or existence) of the returned
session-id is made.

If no session-id is supplied NIL must be returned."))

(defgeneric find-frame-id (context)
  (:documentation "Same as find-session-id but looks for the frame id."))

(defgeneric find-action-id (context)
  (:documentation "Same as find-session-id but looks for the action id."))

(defgeneric clear-context (context)
  (:documentation "Prepare REQUEST-CONTEXT for re-use.

This method must destructivly modify CONTEXT so that it becomes
indistinguishable from a freshly created object as returned by
make-request-context."))

;;;; ** The generic function SERVICE 

;;;; Service method and various constants and variables used while
;;;; serving a request.

(defgeneric service (relevant-object context)
  (:documentation "The core request handling generic function.

The service protocol consists of 4 passes: application -> session
-> session-frame -> action. At each phase the appropriate object
is passed to service with the current request object (which is
also bound to *context*). The appropiate object must perform
whatever it needs and then explicitly call service again passing
it the next object in the chain."))

(defvar *context* :unbound
  "The current request-context object.")

;;;; ** Sessions

(defclass session ()
  ())

(defgeneric session.id (session)
  (:documentation "Returns the id of SESSION.

An ID is a unique (within the same application) object
identifying SESSION."))

(defgeneric expiredp (session)
  (:documentation "Returns true if SESSION has expired."))

(defgeneric expire-session (session)
  (:documentation "Causes this session to cease to exist."))

(defgeneric make-new-frame (session)
  (:documentation "Adds a new session-frame object to SESSION."))

(defgeneric call-callbacks (frame request)
  (:documentation "Call all the callbacks in REQUEST for FRAME.

Methods defined on this generic function must be built up from
calls to map-parameters (part of the backend protocol) and
call-callback."))

(defgeneric session.current-frame (session)
  (:documentation "Return the current active frame in the session."))

(defgeneric session.value (key session &optional default)
  (:documentation "Fetch the value with key KEY in the session SESSION.

DEFAULT is returned should KEY not be found in SESSION's
session-table.

Keys are compared using EQL."))

(defgeneric (setf session.value) (value key session &optional default)
  (:documentation "Sets the value of KEY in SESSION to VALUE.

As with session.value keys are compared with EQL."))

(defun get-session-value (key
			  &optional
			  (default nil)
			  (session (context.session *context*)))
  "Convenience wrapper around session.value.

SESSION defaults to the current session."
  (session.value key session default))

(defun (setf get-session-value) (value key
				 &optional
				 (session (context.session *context*)))
  "Convience wrapper around (setf session.value).

SESSION defaults to the current session."
  (setf (session.value key session) value))

;;;; ** Session Frames

(defclass session-frame ()
  ()
  (:documentation "Super class of all session frame.

A session frame represents a single request/response interaction
with a client. Session frames know what actions they've generated
and what the component tree was when the response was sent."))

(defgeneric frame.id (session-frame))

(defgeneric frame.window-component (session-frame)
  (:documentation "Accessor for SESSION-FRAME's window component."))
(defgeneric (setf frame.window-component) (component session-frame))

(defgeneric call-callback (session-frame param-name value)
  (:documentation "Call the parameter callback in SESSION-FRAME
with name PARAM-NAME passing it VALUE. PARAM-NAME is always a
string, VALUE is either a string or a list of strings depending
on how many values where under the name PARAM-NAME."))

(defgeneric find-action (session-frame action-id)
  (:documentation "Return the action (a function) associated with ACTION-ID."))

(defgeneric make-next-frame (frame new-frame-id)
  (:documentation "Create the \"next\" frame object with id NEW-FRAME-ID.

The new frame object must build its history (component,
backtracks, etc.) assuming that FRAME is the previous (in
chronological order) frame."))

;;;; ** The Backend

;;;; UCW extracts the particular web server used through this
;;;; protocol.

(defclass backend ()
  ()
  (:documentation "Super class for UCW backends.

A backend object is used to represent one active running instance
of an http server which UCW can use to read requests and send
response.

A backend object's life cycle is managed by the three methods
initialize-backend, startup-backend and shutdown-backend."))

(defgeneric initialize-backend (backend &rest init-args)
  (:documentation "Prepare BACKEND but do not actually start serving request."))

(defgeneric startup-backend (backend &rest startup-args)
  (:documentation "Start the RERL. This method may block on
  single threaded lisps."))

(defgeneric shutdown-backend (backend &rest shutdown-args)
  (:documentation "Stop the RERL and release any resources."))

(defgeneric publish-directory (backend directory-pathname url-base)
  (:documentation "Publish the directory at PATHNAME at the URLs starting from URL-BASE."))

(defclass message () ()
  (:documentation "Generic superclass for all HTTP messages.

All HTTP messages provide the ability to inspect (and maybe
modify) the associated headers and mesage body."))

(defgeneric get-header (message header-name)
  (:documentation "Return the value of the header named HEADER-NAME.

HEADER-NAME must be a string and will be compared case sensitvly
agaist the headers in the message.

Callers are not allowed to modify the value returned by
GET-HEADER."))

(defgeneric (setf get-header) (value message header-name)
  (:documentation "Change the value of the header named
  HEADER-NAME. Like in get-header HEADER-NAME must be a
  string."))

(defgeneric add-header (message header-name value)
  (:documentation "Add another header line with
  HEADER-NAME=VALUE. Returns VALUE.

Like get-header and (setf get-header) HEADER-NAME must be a
string."))

(defclass request (message) ()
  (:documentation "Generic superclass for all HTTP requests."))

(defgeneric query-path (request)
  (:documentation "Return the QUERY-PATH of REQUEST as a string.

The returned string may share structure with objects in the
request and sholud be considered read-only. The returned
query-path must be already escaped."))

(defgeneric get-parameter (request parameter-name)
  (:documentation "Returns the query value, a string or a list of
  strings, associated with the name PARAMETER-NAME. As with
  GET-HEADER callers are not allowed to modify the value retruned
  by this function."))

(defgeneric map-parameters (request lambda)
  (:documentation "Apply LAMBDA to all the parameters in REQUEST.

LAMBDA will be passed two arguments: the name of the parameter, a
string, and the value of the parameter, a string or a list of
strings. The values are passed in that order and LAMBDA must not
modify any of the values passed to it."))

(defgeneric close-request (request)
  (:documentation "This method is called when the server is done
  with REQUEST. If any shared resources were grabbed during the
  request handling they should be freed."))

(defclass response (message) ()
  (:documentation "Generic superlcass for all HTTP responses.

An HTTP response has, other than the headers, two streams:

1) the html-stream - This is a character based stream which can
   be used to send ISO-8859-1 characters to the client.

2) the network-stream - This is the binary (unsigned-byte 8)
   stream attached directly to the client' browser."))

(defgeneric html-stream (response)
  (:documentation "Return the stream we can write html output to.

This stream may be cached in order to provide an accurate
Content-Length header."))

(defgeneric network-stream (response)
  (:documentation "Return the stream attached to the client's browser.

Any bytes written to this stream are sent immediatly to the
client (though buffering issues may arise)."))

(defgeneric encoding (response)
  (:documentation "Return a symbol representing the desired encoding
when writing strings into the network stream."))

(defgeneric clear-response (response)
  (:documentation "Restore RESPONSE to its clean state.

This generic function is generally used when dealing with an
error which occurs during the rendering of a window."))

(defgeneric mime-part-headers (mime-part)
  (:documentation "Returns an alist of the headers of MIME-PART.

The alist must be of the form (NAME . VALUE) where both NAME and
VALUE are strings."))

(defgeneric mime-part-body (mime-part)
  (:documentation "Returns the body of MIME-PART."))

;;;; *** Sending the RESPONSE

;;;; There are two ways to send the response to the client. You can
;;;; choose to let the backend deal with it; in this case you should
;;;; send the data, as a sequence of characters, to the response's
;;;; html-stream and then call send-response. If you want to do it
;;;; yourself you should first call send-headers and then write the
;;;; data, as a sequence of bytes, to the response's
;;;; network-stream. Calling send-headers is, technically, optional,
;;;; you could just as well write the HTTP headers yourself. There are
;;;; two major differences between these options: 1) if you let the
;;;; backend do it the conversion from characters to bytes is done for
;;;; you (based on the response's Content-Type header) as is the
;;;; calculation of the Content-Length header; 2) if you bypass the
;;;; backend the data is sent directly to the client as it's produced
;;;; (i believe this is called streaming).

(defgeneric send-response (response)
  (:documentation "Assuming RESPONSE is complete send it to the
  client. When calling this function we assume all headers have
  already been set and the body's content has been collected in
  the response object."))

(defgeneric send-headers (response)
  (:documentation "Sends all the headers in RESPONSE to the
  client. Unlike the send-response method we don't add a
  Content-Length header, though if one is present in RESPONSE it
  will be sent.

After calling this method the response data, a sequence of bytes,
should be sent to RESPONSE's network-stream."))

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
