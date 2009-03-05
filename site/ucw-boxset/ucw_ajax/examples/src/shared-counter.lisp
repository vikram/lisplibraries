;;;; -*- lisp -*-

(in-package :it.bese.ucw-user)

(defclass shared-counter-application (single-frame-application-mixin cookie-session-application)
  ())

(defclass shared-counter-session ()
  ()
  (:documentation "This is only here to demo how to customize the session object."))

(defmethod session-class list ((app shared-counter-application))
  'shared-counter-session)

(defvar *shared-counter-application*
  (make-instance 'shared-counter-application
                 :url-prefix "/shared-counter/"
                 :debug-on-error t))

;;;; define the window component

(defcomponent shared-counter-window (simple-window-component)
  ((body :initarg :body :accessor body-of :component (shared-counter-component)))
  (:render (self)
           (render (body-of self)))
  (:default-initargs
    :title "UCW shared counter example"
    :stylesheet (list "/shared-counter/ucw/ucw.css")))

(defentry-point "^(index.ucw|)$" (:application *shared-counter-application*
                                  :class regexp-dispatcher)
    ()
  (call 'shared-counter-window))

(defun delete-shared-counter-sessions ()
  (let ((app (find-if (rcurry #'typep 'shared-counter-application)
                      (ucw::server.applications *default-server*))))
    (clrhash (ucw::application.session-table app))))

;;;; shared counter example itself

(defparameter *shared-counter* 0)
(defparameter *shared-counter-last-updater* nil)
(defparameter *shared-counter-listeners* nil)

(labels ((iterate-shared-counter-listeners (visitor)
           (iter (with previous-cell = nil)
                 (for cell :first *shared-counter-listeners* :then (cdr cell))
                 (while cell)
                 (for component = (tg:weak-pointer-value (car cell)))
                 (for session = (when component
                                  (session-of component)))
                 (ucw::ucw.rerl.ajax.debug "In iterate-shared-counter-listeners visiting component ~S" component)
                 (if component
                     (progn
                       (with-recursive-lock-held ((lock-of session))
                         (funcall visitor component))
                       (setf previous-cell cell))
                     (if (eq cell *shared-counter-listeners*)
                         (setf *shared-counter-listeners* (cdr cell))
                         (progn
                           (setf (cdr previous-cell) (cdr cell))
                           (setf previous-cell cell))))))
         (mark-dirty-all-shared-counter-listeners ()
           (iterate-shared-counter-listeners
            (lambda (component)
              (ucw::ucw.rerl.ajax.debug "Notifying ~S in session ~S" component (ucw::session-of component))
              (mark-dirty component)))))

  (defun subscribe-to-shared-counter (component)
    (with-lock-held-on-current-application
      (pushnew (tg:make-weak-pointer component)
               *shared-counter-listeners*
               :key #'tg:weak-pointer-value)
      (mark-dirty-all-shared-counter-listeners)))

  (defun increment-shared-counter ()
    ;; we need to lock the entire app to avoid a race condition when another session tries to notify
    ;; the listeners in a paralel request. calling mark-dirty across sessions is a potential source
    ;; of deadlocks if not handled correctly (because the current session is locked while requests
    ;; are processed, but they are processed in paralel by multiple threads. so locking another session
    ;; direclty is dangerous, because if it is doing the same to us then we can easily deadlock)
    (with-lock-held-on-current-application
      (incf *shared-counter*)
      (setf *shared-counter-last-updater* (ucw::peer-address (context.request *context*)))
      (mark-dirty-all-shared-counter-listeners))))

(defcomponent shared-counter-component (widget-component)
  ()
  (:render (self)
    (macrolet ((render-info (desc &body body)
                 `(<:p ,desc
                      (<:span :style "font-size: x-large; font-weight: bold"
                              ,@body))))
      (<:p "This is a vm-global counter. Each shared-counter-component subscribe to the update events "
           "and incrementing the counter calls mark-dirty on all subscribed components. The client side "
           "is polling the server for any changes. If the lisp under the server supports threads and "
           "there are less then a given number of open connections, then the request processing thread "
           "falls asleep and the request is not answered until there's an event for the client.")
      (<:p "What this all means is that even if you have several sessions (with standalone browser processes "
           "e.g. two different machines/users/browser types) this counter is still shared among them. "
           "Incrementing it in one session should ajax-render the components in all the other sessions "
           "unless polling is disabled.")
      (render-info "The current counter value is: "
                   (<:as-html *shared-counter*))
      (render-info "The last updater's IP is: "
                   (<:as-html *shared-counter-last-updater*))
      (render-info "Number of subscribed event listeners: "
                   (<:as-html (with-lock-held-on-current-application
                                (list-length *shared-counter-listeners*))))
      (<:p (<ucw:a :action (register-ajax-action ()
                             (increment-shared-counter))
                   "increment it for eveyone's delight"))
      (<:p (<:a :href "#" :onclick (js:js-inline* `(if (ucw.io.polling.is-polling)
                                                    (ucw.io.polling.stop)
                                                    (ucw.io.polling.start)))
                (<:as-html "toggle polling on/off"))))))

(defmethod initialize-instance :after ((self shared-counter-component) &key &allow-other-keys)
  (subscribe-to-shared-counter self))

