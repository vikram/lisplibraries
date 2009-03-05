;; -*- lisp -*-

(in-package :it.bese.ucw)

;;;; ** STANDARD-SESSION-FRAME

;; TODO make a single isolation slot and store one of :per-invocation :once-only or nil
;; TODO isolated actions could be dropped for gc purposes. probably has only limited effects...
(defstruct (action-entry (:conc-name action-) (:constructor %make-action-entry))
  (lambda nil :type function)
  (valid-p t :type boolean)            ; invalid actions are never called
  (isolated-p nil :type boolean)       ; isolated actions get invalid after the first call
  (invocation-isolated-p nil :type boolean) ; keep track of a list of invocation id's that the client
                                            ; sent with the invocations and only run the action once for
                                            ; each invocation id (i.e. reloading the page does not trigger
                                            ; the action again, but clicking it does (as opposed to
                                            ; isloated-p which prevents repeated execution in any case))
  (invocation-ids '() :type list)           ; the list of timestamps we've seen up til now
  (call-render-loop-p t :type boolean) ; should render-loop be called when the action returns?
  (manage-response-p t :type boolean)  ; should flush-request-response be called when the action returns?
  (make-new-frame-p t :type boolean)   ; should we create a new frame when this action runs?
  (ajax-p nil :type boolean)           ; should the response of this action be treated as an ajax xml answer?
  (id nil :type string))

(defun make-action (lambda &key
                           (ajax nil)
                           (isolated nil)
                           (invocation-isolated (and (not isolated) (not ajax)))
                           (raw ajax)
                           (call-render-loop (not raw))
                           (make-new-frame (not raw))
                           (manage-response (not raw)))
  "Makes a new unregistered action-entry. Currently AJAX and RAW mean the same
which is turn off CALL-RENDER-LOOP, MAKE-NEW-FRAME and MANAGE-RESPONSE unless
they are explicitly set. AJAX t also means that the client side JS for this
action should treat the response as an UCW AJAX xml (for details what it means
see handle-ajax-request and ajax.lisp) when this action is used with <ucw: tags."
  (assert (or (xor isolated invocation-isolated)
              (not (or isolated invocation-isolated)))
          (isolated invocation-isolated)
          "An action may either be isolated or invocation-isolated. but not both at the same time.")
  (%make-action-entry
    :id "unregistered"
    :lambda lambda
    :isolated-p isolated
    :invocation-isolated-p invocation-isolated
    :ajax-p ajax
    :call-render-loop-p call-render-loop
    :make-new-frame-p make-new-frame
    :manage-response-p manage-response))

(defmacro make-action-body ((&rest args &key with-call/cc (make-new-frame with-call/cc) &allow-other-keys) &body body)
  (remf-keywords args :with-call/cc :make-new-frame)
  `(make-action (lambda ()
                  ,@(if with-call/cc
                        `((with-call/cc
                            ,@body))
                        body))
    :make-new-frame ,make-new-frame ; propagate t as the default make-new-frame value when with-call/cc is t
    ,@args))

(defmacro register-action ((&rest args &key (through-redirect nil)
                                  (frame '(context.current-frame *context*)) &allow-other-keys)
                           &body body)
  "Makes a new action and registers it in FRAME. For more details see make-action."
  (remf-keywords args :frame :through-redirect)
  (with-unique-names (action target-url)
    (if through-redirect
        `(let* ((,target-url (action-href (register-action (,@args) ,@body)))
                (,action (make-action-body (:raw t)
                           (handle-raw-request (:content-type "text/html" :with-yaclml-stream t)
                             (let ((response (context.response *context*)))
                               (setf (get-header response "Status") "302"
                                     (get-header response "Location") ,target-url)
                               (<:html
                                (<:head
                                 (<:title "302 - Redirect"))
                                (<:body
                                 (<:p "Page has moved "
                                      (<:a :href ,target-url "here")))))))))
          (register-action-in-frame ,frame ,action)
          ,action)
        `(let ((,action (make-action-body (,@args) ,@body)))
          (register-action-in-frame ,frame ,action)
          ,action))))

(defmacro register-ajax-action ((&rest args &key (ajax t) &allow-other-keys)
                                &body body)
  (remf-keywords args :ajax)
  `(register-action (:ajax ,ajax ,@args) ; propagate :ajax t as the default for register-action
    (handle-ajax-request () ; TODO handle-ajax-request macro could be moved out of the call/cc body
      ,@body)))

(defmethod register-action-in-frame ((frame standard-session-frame) action-entry)
  (setf (action-id action-entry) (insert-with-new-key (frame.actions frame) +action-id-length+ action-entry)))

(defmethod find-action ((frame standard-session-frame) (action-id string))
  (gethash action-id (frame.actions frame)))

(defmethod find-action ((f standard-session-frame) (action-id list))
  (find-action f (car action-id)))

(defmethod find-action ((f standard-session-frame) (action-id null))
  nil)

(defmethod (setf find-action) (lambda (frame standard-session-frame) (action-id string))
  (setf (gethash action-id (frame.actions frame))
	lambda))

(defstruct (callback-entry (:conc-name callback-) (:constructor %make-callback-entry))
  (lambda nil :type function)
  (dependencies '() :type list) ; a list of callbacks that should be run before this one
  (executed nil :type boolean)  ; while callbacks are processed this flag is used to mark execution
  (priority 0 :type fixnum)     ; callbacks will be called in the order determined by this priority
  (id nil :type string))

(defun make-callback (lambda &key (priority 0))
  "Creates a new (unregistered) callback.

When registered and the request arrives LAMBDA will be passed the
value (a string) associated with the input. If NAME is not
provided, or NIL, a random name will be generated.

Returns the freshly created callback-entry struct."
  (%make-callback-entry :lambda lambda :id "unregistered" :priority priority))

(defun register-callback (lambda &rest args &key
                                 (frame (context.current-frame *context*))
                                 id &allow-other-keys)
  "Makes a new callback and registers it in FRAME. For more details see make-callback."
  (remf-keywords args :frame :id)
  (let ((entry (apply #'make-callback lambda args)))
    (register-callback-in-frame frame entry :id id)
    (callback-id entry)))

(defmethod register-callback-in-frame ((frame standard-session-frame) callback &key id)
  "Registers a callback-entry in this frame. When passed in an
action generated by FRAME then CALLBACK will be called passing it the value of
the corresponding request param."
  (unless id
    (setf id (new-callback-id frame)))
  (setf (gethash id (frame.callbacks frame)) callback)
  (setf (callback-id callback) id)
  callback)

(defun new-callback-id (&optional (frame (context.current-frame *context*)))
  ;; we insert a nil here to mark that this callback id is used even if no callback
  ;; is gets registered under this key
  (insert-with-new-key (frame.callbacks frame) +action-id-length+ nil))

(defmethod call-callbacks ((frame standard-session-frame) (request request))
  "Execute all the callback lambda in CONTEXT's request.

Simply goes through the request's params and, for every param
which isn't +action-parameter-name+, +frame-parameter-name+ or
+session-parameter-name+, looks up and call the associated lambda
in the current frame. Makes sure dependent callbacks are executed after
their dependencies."
  (let ((callbacks (make-hash-table))
        (found-callbacks 0))
    (map-parameters request
                    (lambda (param value)
                      (unless (member param (list +action-parameter-name+
                                                  +action-invocation-parameter-name+
                                                  +frame-parameter-name+
                                                  +session-parameter-name+)
                                      :test #'string=)
                        (if-bind callback (gethash param (frame.callbacks frame))
                          (progn
                            (setf (callback-executed callback) nil)
                            (setf (gethash callback callbacks) value))
                          (ucw.rerl.warn "No callback found with id ~S in frame ~S (value is ~S)."
                                         param frame value)))))
    (setf found-callbacks (let ((count (hash-table-count callbacks)))
                            (unless (zerop count)
                              count)))
    (flet ((ready-to-execute-p (callback)
             (iter (for dependency in (callback-dependencies callback))
                   (always (callback-executed dependency)))))
      (iter (with done-something = nil)
            (iter (for (callback value) in-hashtable callbacks)
                  (when (ready-to-execute-p callback)
                    (collect (cons callback value) into to-be-called :result-type vector))
                  (finally
                   (sort to-be-called #'> :key (lambda (el)
                                                 (callback-priority (car el))))
                   (iter (for (callback . value) in-vector to-be-called)
                         (ucw.rerl.session-frame.debug "Calling callback ~S" callback)
                         (remhash callback callbacks)
                         (setf (callback-executed callback) t)
                         (with-action-error-handler ()
                           (funcall (callback-lambda callback) value))
                         (setf done-something t))))
            (while done-something)
            (setf done-something nil))
      (unless (zerop (hash-table-count callbacks))
        (cerror "Ignore them and continue processing the action"
                "Circular dependency found in callbacks of frame ~S, involved callbacks are: ~S!"
                frame (hash-table-keys callbacks))))
    found-callbacks))

(defmethod make-next-frame ((f standard-session-frame) new-id)
  (make-instance 'standard-session-frame
                 :backtracks (clone-backtracks (frame.backtracks f))
                 :window-component (frame.window-component f)
                 :id new-id))

(defmethod make-next-frame ((f null) new-id)
  (make-instance 'standard-session-frame :id new-id))

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
