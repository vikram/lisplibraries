;; See the file LICENCE for licence information.
(in-package :ucw)

(enable-sharpquote<>-reader)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dirty-component-tracking

(defclass dirty-component-tracking-application-mixin ()
  ()
  (:documentation "Application mixin that enables the tracking of dirty components."))

(defclass dirty-component-tracking-session (standard-session)
  ((dirty-components :initform (make-hash-table :weakness :key :test #'eq)
                     :accessor dirty-components-of)))

(defmethod session-class list ((app dirty-component-tracking-application-mixin))
  'dirty-component-tracking-session)


;;;;;;;;;;;;;;;;
;; single-frame

(defclass single-frame-application-mixin (dirty-component-tracking-application-mixin)
  ()
  (:documentation "Application mixin that disables backtracking and lives in a single frame."))

(defmethod request-context-class list ((app single-frame-application-mixin))
  'single-frame-request-context-mixin)

(defclass single-frame-request-context-mixin ()
  ()
  (:documentation "Request context for single frame apps."))

(defclass single-frame-session (dirty-component-tracking-session standard-session)
  ())

(defmethod session-class list ((app single-frame-application-mixin))
  'single-frame-session)

(defmethod dirty-components-of ((session standard-session))
  nil)

(defclass single-frame-session-frame (standard-session-frame)
  ())

(defmethod make-new-frame ((session single-frame-session))
  (let* ((id "*")
         (new-frame (aif (session.current-frame session)
                         (make-instance 'single-frame-session-frame
                                        :window-component (frame.window-component it)
                                        :id id)
                         (make-instance 'single-frame-session-frame :id id))))
    (setf (session.current-frame session) new-frame)
    new-frame))

(defmethod find-frame-by-id ((session single-frame-session) frame-id)
  (declare (ignore frame-id))
  (session.current-frame session))

(defmethod backtrack ((frame single-frame-session-frame) place &optional value)
  (declare (ignore place value))
  ;; nop
  t)

(defmethod save-backtracked ((frame single-frame-session-frame))
  ;; nop
  )

(defmethod reinstate-backtracked ((frame single-frame-session-frame))
  ;; nop
  )

;;;
;;; Dirty stuff
;;;
(defvar %disable-dirtyness-tracking%)

(defun register-dirty-component (component)
  (unless (boundp '%disable-dirtyness-tracking%)
    (let* ((session (session-of component))
           (table (dirty-components-of session)))
      ;; TODO portable assert that we have the lock on the session
      #+sbcl (assert (eq (sb-thread::mutex-value (lock-of session)) (current-thread)) ()
                     "You must have a lock on the session while registering dirty components in it")
      (when table
        (ucw.rerl.ajax.debug "About to register dirty component ~S, the session has ~S dirty components currently"
                             component (hash-table-count table))
        (setf (gethash component table) t)
        (notify-session-event session)))))

(defun unregister-dirty-component (component)
  (let* ((session (session-of component))
         (table (dirty-components-of session)))
    ;; TODO portable assert that we have the lock on the session
    #+sbcl (assert (eq (sb-thread::mutex-value (lock-of session)) (current-thread)) ()
                   "You must have a lock on the session while unregistering dirty components from it")
    (when table
      (remhash component table)
      (ucw.rerl.ajax.debug "Component ~S is not dirty anymore, the session has ~S dirty components currently"
                           component (hash-table-count table)))))

(defun has-dirty-components ()
  (iterate-dirty-components (lambda (component)
                              (declare (ignore component))
                              (return-from has-dirty-components t)))
  nil)

(defmacro without-dirtyness-tracking (&body body)
  "Disable dirtyness tracking. IOW, register-dirty-component will have no effects
while in the dynamic scope of without-dirtyness-tracking."
  `(let ((%disable-dirtyness-tracking% t))
    ,@body))

(defun mark-dirty (component)
  "It's a (setf (dirtyp component) t) inside a with-lock-held-on-session for convenience."
  (with-lock-held-on-session (session-of component)
    (setf (dirtyp component) t)))

(defun iterate-dirty-components (visitor)
  (ucw.rerl.ajax.dribble "iterate-dirty-components entered with visitor ~S" visitor)
  (when-bind table (dirty-components-of (context.session *context*))
    (let ((components (hash-table-keys table)))
      (ucw.rerl.ajax.dribble "List of dirty components before collecting ~S" components)
      (setf components (iter (for component in components)
                             (ucw.rerl.ajax.dribble "Checking component ~S" component)
                             (unless (dirtyp component)
                               (ucw.rerl.ajax.dribble "Components ~S is not dirty anymore, unregistering" component)
                               (unregister-dirty-component component)
                               (next-iteration))
                             (for (values visiblep distance) = (visiblep component))
                             (if visiblep
                                 (collect (cons component distance))
                                 (ucw.rerl.ajax.dribble "Component ~S is not visible, dropping from the list" component))))
      (ucw.rerl.ajax.dribble "List of dirty components before sorting ~S" components)
      (setf components (sort components #'< :key #'cdr))
      (ucw.rerl.ajax.dribble "List of dirty components after sorting ~S" components)
      (iter (for (component . nil) in components)
            (ucw.rerl.ajax.debug "iterate-dirty-components visiting component ~S, dirtyp? ~S"
                                 component (dirtyp component))
            (when (dirtyp component)    ; need to check again, it might have been rendered meanwhile
              (ucw.rerl.ajax.debug "iterate-dirty-components calling visitor with component ~S" component)
              (funcall visitor component))))))

(defmethod expire-session :after ((session single-frame-session))
  ;; abort the client pollers
  (setf (latest-polling-thread-of session) nil)
  (notify-session-event session))

(defvar +default-polling-delay+ 3000
  "The default delay in ms to wait before the client connects the server again for new events.")
(defvar +default-polling-delay-on-error+ 3000
  "The default delay in ms to wait before the client connects the server again again for new events after an error.")
(defvar +max-number-of-kept-alive-polling-connections+ 30
  "While there are less then this many polling connections, they are blocked on the server and woke up when an event is available.")
(defparameter *current-number-of-kept-alive-polling-connections* 0)

(defun calculate-client-polling-delay ()
  (if (and *supports-threads-p*
           (< *current-number-of-kept-alive-polling-connections*
              +max-number-of-kept-alive-polling-connections+))
      0
      +default-polling-delay+))

(defun handle-polling-of-session (application session frame)
  (with-lock-held-on-session session
    (ucw.rerl.ajax.debug "handle-polling-of-session entered while there are ~S alive pollers"
                         *current-number-of-kept-alive-polling-connections*)
    (unwind-protect
         (progn
           (incf *current-number-of-kept-alive-polling-connections*)
           (let ((session (context.session *context*))
                 (current-thread (current-thread)))
             (setf (latest-polling-thread-of session) current-thread)
             (notify-session-event session) ; wake up any previous pollers to make them quit
             (when (and *supports-threads-p*
                        (not (has-events-for-the-client session)))
               (ucw.rerl.ajax.debug "client-polling-handler got nil from has-events-for-the-client, falling asleep")
               (loop named waiting do
                     (wait-for-session-event session) ; we release the session lock and wait for a notification
                     (unless (eq (latest-polling-thread-of session)
                                 current-thread)
                       (ucw.rerl.ajax.debug "client-poller aborting because there's a newer polling thread")
                       (notify-session-event session) ; wake up any other (possible poller) threads waiting
                       (return-from handle-polling-of-session))
                     (when (has-events-for-the-client session)
                       (return-from waiting))))
             (ucw.rerl.ajax.debug "client-poller woke up, sending back the ajax answer")
             ;; go through the public protocol, so transactions and stuff is alive while serving polling requests
             (handle-action-in-session (lambda ()
                                         (handle-ajax-request ()
                                           (<ucw:script :toplevelp t
                                                        `(ucw.io.polling.set-delay ,(calculate-client-polling-delay)))
                                           (send-events-to-the-client session)))
                                       application session frame)))
      (decf *current-number-of-kept-alive-polling-connections*))))

(defgeneric send-polling-response-to-unknown-session (application)
  (:documentation "Called when a polling request was received in an unknown session and it needs to be handled.

By default it returns a small javascript that instructs the client to reload the page to get a new session.")
  (:method  (application)
            (handle-ajax-request ()
              (<ucw:script :toplevelp t
                           `(ucw.load-relative-url ,(application.url-prefix application))))))

