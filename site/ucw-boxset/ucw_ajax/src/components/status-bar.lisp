;; -*- lisp -*-

(in-package :it.bese.ucw)

(defcomponent status-bar ()
  ((messages :accessor messages :initarg messages :initform '()
             :documentation "An ALIST of the messages to
show. Each element is a cons of the form (SEVERITY .
MESSAGE). SEVERITY is one of :ERROR, :WARN, :INFO and MESSAGE is
a string which will be html-escaped.")
   (js-class :accessor js-class :initarg :js-class :initform "ucw-status-bar"
             :documentation "A string specifying the CSS class
for the div which wraps that status-bar's output.")
   (js-class-for-error :accessor js-class-for-error :initarg :js-class-for-error
                       :initform "ucw-status-bar-error"
                       :documentation "The class used for the P
tags which wrap error messages.

When to explicity specified or set this defaults
to (strcat (js-class status-bar) \"-error\").")
   (js-class-for-warn :accessor js-class-for-warn :initarg :js-class-for-warn
                      :initform "ucw-status-bar-warn"
                      :documentation "The CSS class used for the
P tags which wrap warn messages.

When not explicity specified or set this defaults
to (strcat (js-class status-bar) \"-warn\").")
   (js-class-for-info :accessor js-class-for-info :initarg :js-class-for-info
                      :initform "ucw-status-bar-info"
                      :documentation "The CSS class used for the
P tags which wrap info messages.

When not explicity specified or set this defaults
to (strcat (js-class status-bar) \"-info\")."))
  (:documentation "Stateless status bar to display messages."))

(defmethod shared-initialize :after ((status-bar status-bar) slot-names
                                     &key
                                     (js-class-for-error nil js-class-for-error-p)
                                     (js-class-for-warn nil js-class-for-warn-p)
                                     (js-class-for-info nil js-class-for-info-p)
                                     &allow-other-keys)
  (declare (ignore slot-names))
  (macrolet ((set-default-unless-specified (accessor supplied-p suffix)
               `(setf (,accessor status-bar) (if ,supplied-p
                                                 ,accessor
                                                 (if (js-class status-bar)
                                                     (strcat (js-class status-bar) ,suffix)
                                                     nil)))))
    (set-default-unless-specified js-class-for-error
                                  js-class-for-error-p
                                  "-error")
    (set-default-unless-specified js-class-for-warn
                                  js-class-for-warn-p
                                  "-warn")
    (set-default-unless-specified js-class-for-info
                                  js-class-for-info-p
                                  "-info")))

(defmethod render ((sb status-bar))
  (with-slots (messages) sb
    (<:div :id "status-bar"
      (iter
       (for (severity . msg) in (reverse messages))
       (<:p :class (ecase severity
                     (:error (js-class-for-error sb))
                     (:warn (js-class-for-warn sb))
                     (:info (js-class-for-info sb)))
         (<:as-html msg))))
    (setf messages '())))

(defgeneric add-message (status-bar msg &key severity &allow-other-keys)
  (:method ((sb status-bar) msg &key (severity :info))
    (setf (messages sb) (acons severity msg (messages sb))))
  (:documentation "Add the message text MSG to STATUS-BAR with
severity SEVERITY."))

(defcomponent status-bar-mixin ()
  ((status-bar :accessor status-bar
               :initarg status-bar
               :component (status-bar))))

(defmethod show-status-bar ((win status-bar-mixin))
  (render (status-bar win)))

(defgeneric show-message (msg &key severity &allow-other-keys)
  (:method ((msg string) &key (severity :info) (context *context*))
    (let* ((win (context.window-component context))
           (win-class (class-of win)))
      (assert (subtypep win-class (find-class 'status-bar-mixin))
              (win)
               "Type ~S of ~S is not a subclass of STATUS-BAR-MIXIN"
               win-class win)
      (add-message (status-bar win) msg :severity severity))))
