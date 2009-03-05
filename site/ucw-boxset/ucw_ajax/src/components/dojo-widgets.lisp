;; -*- lisp -*-
;; See the file LICENCE for licence information.

(in-package :it.bese.ucw)

;; TODO better integrate ucw dirty checking and validation with dojo

(enable-sharpquote<>-reader)
(enable-bracket-reader)

(defmacro rendering-dojo-tooltip-for (id &body body)
  "This macro will bind the html rendered in its body to the dom node given by ID as a tooltip."
  `(<:span (@ "dojo:type" "tooltip" :connectid ,id) :style "display: none"
           ,@body))

;; TODO move to parenscript? name?
(defun to-js-boolean (value)
  (if value 'true 'false))

;;; This file contains some dojo widget wrappers. They are basically CLOS objects
;;; representing dojo widgets with their parameters.

(defmacro def-dojo-widget (name supers slots &rest args)
  (flet ((find-arg (name)
           (find name args :key #'car))
         (delete-arg (arg)
           (setf args (delete arg args :test #'eq))))
    (let ((default-initargs (find-arg :default-initargs))
          (dojo-type)
          (metaclass (find-arg :metaclass)))
      (setf args (copy-list args))
      (if (listp name)
          (setf dojo-type (second name)
                name (first name)))
      (if default-initargs
          (progn
            (delete-arg default-initargs)
            (setf default-initargs (copy-list default-initargs)))
          (setf default-initargs (list :default-initargs)))
      (if metaclass
          (delete-arg metaclass)
          (setf metaclass (list :metaclass 'standard-component-class)))
      (when dojo-type
        (setf (getf (rest default-initargs) :dojo-type) dojo-type))
      (unless (find 'dojo-widget supers)
        (setf supers (append supers (list 'dojo-widget))))
      `(defclass ,name ,supers ,slots
        ,@(when (> (length default-initargs) 1)
                (list default-initargs))
        ,metaclass
        ,@args))))

(defclass dojo-widget (html-element ajax-component-mixin)
  ((dojo-type :accessor dojo-type-of :initarg :dojo-type)
   (widget-id :accessor widget-id)
   (onload-scripts :initform '() :initarg :onload-scripts))
  (:documentation "An abstract dojo widget that does not render anything."))

(defmethod (setf dom-id) :after (value (self dojo-widget))
  (setf (widget-id self) (strcat value "-widget")))

(defmethod initialize-instance :after ((self dojo-widget) &key &allow-other-keys)
  ;; trigger our specialized method above
  (setf (dom-id self) (dom-id self))
  (setf (dojo-type-of self) (dojo-type-of self)))

(defgeneric onload-scripts-of (dojo-widget)
  (:method-combination list)
  (:method list ((self dojo-widget))
           (slot-value self 'onload-scripts)))

(defmethod add-onload-script ((self dojo-widget) script)
  (setf (slot-value self 'onload-scripts)
        (cons 'progn (cons script (slot-value self 'onload-scripts)))))

(defmethod render :after ((self dojo-widget))
  (awhen (onload-scripts-of self)
    (multiple-value-bind (parenscripts)
        (iter (for script in it)
              (when script
                (if (stringp script)
                    (error "String scripts are not yet supported") ; TODO need parenscript quote for this
                    (collect script :into parenscripts)))
              (finally (return (values parenscripts))))
      (when parenscripts
        (<ucw:script :toplevelp t `(progn
                                    (log.debug "Calling dojo.add-on-load to register some widget scripts "
                                     ,(subseq (princ-to-string parenscripts) 0 64))
                                    (dojo.add-on-load
                                     (lambda ()
                                       (let ((widget (dojo.widget.by-id ,(widget-id self))))
                                         (assert widget)
                                         (with-this widget ; bind 'this' to the widget
                                           ,@parenscripts))))))))))

(defclass simple-dojo-widget (dojo-widget)
  ()
  (:documentation "A dojo widget which should be wrapped in a <div> with a dojoType=\"...\" attribute."))

(defmacro with-dojo-widget-tag ((widget &rest args) &body body)
  ;; if we started ajax rendering from here then do not render the dojo div, because it's
  ;; against the dojo contract to replace those dom nodes. rather render the body div, so
  ;; the client side will only replace that dom node...
  `(if (ajax-rendering-starting-point-p ,widget)
       (progn
         ,@body)
       (<:div :id (widget-id ,widget)
              (@ "dojo:type" (dojo-type-of ,widget) ,@args)
        ,@body)))

(defmethod render-widget-wrapper :around ((self simple-dojo-widget) next-render-method)
  "Wrap the simple dojo widget in a single <div dojoType=\"...\"> tag."
  (<:div :class (css-class self) :id (widget-id self) :style (css-style self)
         (@ "dojo:type" (dojo-type-of self))
         (funcall next-render-method self)))

(defmethod render ((self simple-dojo-widget))
  (<:div :id (widget-id self)
         (@ "dojo:type" (dojo-type-of self))))

;;;
;;; dojo-content-pane - ContentPane
;;;
(def-dojo-widget (dojo-content-pane "ContentPane") ()
  ((body :initform nil :initarg :body :accessor body-of :component nil)))

(defmethod render ((self dojo-content-pane))
  (with-dojo-widget-tag (self)
    (awhen (body-of self)
      (etypecase it
        (function (funcall it))
        (component (render it))))))

;;;
;;; dojo-tab-container - TabContainer
;;;
(def-dojo-widget (dojo-tab-container "TabContainer") (switching-container)
  ((do-layout-p :initform nil :initarg :do-layout-p :accessor do-layout-p)
   (remember-selected-tab-p :initform nil :initarg :remember-selected-tab-p :accessor remember-selected-tab-p)))

(defmethod render-widget-wrapper :around ((self dojo-tab-container) next-render-method)
  (with-dojo-widget-tag (self "dojo:doLayout" (to-js-boolean (do-layout-p self))
                              "dojo:selectedChild" (awhen (container.current-component self)
                                                     (widget-id it))
                              "dojo:postInitialize" (when (remember-selected-tab-p self)
                                                      (js:js* `(ucw.widget.tab-container.setup-remember-selected-tab
                                                                ,(dom-id self) ,(widget-id self)))))
    (call-next-method)))

(defmethod render ((self dojo-tab-container))
  (iter (for (nil . tab) in (container.contents self))
        (render-ajax-stub tab)))

;;;
;;; dojo-tab
;;;
(def-dojo-widget dojo-tab (list-container dojo-content-pane)
  ((label :initform nil :initarg :label :accessor label-of)
   (closablep :initform t :initarg :closablep :accessor closablep))
  (:default-initargs :dom-id (js:gen-js-name-string :prefix "_dj-tab")))

(defmethod render-widget-wrapper :around ((self dojo-tab) next-render-method)
  (with-dojo-widget-tag (self "dojo:label" (label-of self)
                              "dojo:closable" (to-js-boolean (closablep self)))
    (call-next-method)))

(defmethod ajax-render-new-tab ((self dojo-tab) &key (select t))
  (within-xhtml-tag "tabs"
    (render-ajax-stub self))
  (<ucw:script `(let ((tab-adder (ucw.io.make-node-walking-ajax-answer-processor "tabs"
                                  (lambda (node original-node)
                                    (log.debug "Processing a tab: " node.tag-name " with id " node.id)
                                    (let ((tab (dojo.widget.create-widget node))
                                          (container (dojo.widget.by-id ,(widget-id (parent self)))))
                                      (assert (= tab (dojo.widget.by-id ,(widget-id self))))
                                      (log.debug "Adding tab " tab.widget-id " to container " container.widget-id)
                                      (.add-child container tab)
                                      (assert (= tab.parent container))
                                      ,(when select
                                             `(.select-child container tab)))
                                    ;; we disable postprocess-inserted-node by returning false
                                    ;; because we already instantiated the widgets with the
                                    ;; dojo.widget.create-widget above. but then we need to eval
                                    ;; the script tags ourselves.
                                    (ucw.io.eval-script-tags original-node)
                                    (return false)))))
                 (tab-adder nil current-ajax-answer nil))))

(defmethod onload-scripts-of list ((self dojo-tab))
  ;; add our handler that'll ajax-load the tab contents
  `(progn
    (assert (= (dojo.widget.by-id ,(widget-id self)) this))
    (assert (slot-value (dojo.widget.by-id ,(widget-id self)) 'parent))
    (log.debug "Setting up content loader of tab " this.widget-id)
    (this.set-handler
     (lambda (pane node)
       (log.debug "AJAX-getting tab pane " pane)
       (ucw.io.execute-ajax-action
        (create :url
                ,(action-href
                  (register-ajax-action (:with-call/cc nil :make-new-frame nil)
                    (within-dom-replacements-tag
                      (ajax-render self))))
                :forms-to-ask (array)
                :progress-label ,#"progress-label.loading-tab"))))
    (when (.is-showing this)
      (.load-contents this))
    ;; if closable then add a handler that tells the server to remove the tab
    ;; otherwise abort closing the tab
    (log.debug "Setting up on-close of tab " this.widget-id)
    (dojo.event.connect this "onClose"
     ,(when (closablep self)
          `(lambda ()
            (log.debug "Calling server to close the tab " ,(dom-id self))
            ,(js-server-callback (:progress-label #"progress-label.closing-tab"
                                                  :sync 'false)
                                 (awhen (parent self)
                                   (without-dirtyness-tracking
                                     (unless (remove-component it (funcall (container.key-generator it) self))
                                       (ucw.component.warn "Tab ~S was not found in the container when the close server callback was called" self)))))
            (return true))))))

;;;
;;; dojo-split-container - SplitContainer
;;;
(def-dojo-widget (dojo-split-container "SplitContainer") (list-container)
  ((sizer-width :initform 5 :initarg :sizer-width :accessor sizer-width-of)
   (active-sizing-p :initform nil :initarg :active-sizing-p :accessor active-sizing-p))
  (:default-initargs :dom-id (js:gen-js-name-string :prefix "_dj-split")
    :orientation :horizontal))

(defmethod render-widget-wrapper :around ((self dojo-split-container) next-render-method)
  (with-dojo-widget-tag (self "dojo:orientation" (if (eq (orientation self) :horizontal)
                                                     "horizontal"
                                                     "vertical")
                              "dojo:sizerwidth" (sizer-width-of self)
                              "dojo:activesizing" (to-js-boolean (active-sizing-p self)))
    (call-next-method)))

(defmethod render ((self dojo-split-container))
  (iter (for (nil . c) in (container.contents self))
        (render c)))

;;;
;;; Date and time
;;;
(def-dojo-widget dojo-dropdown (simple-dojo-widget)
  ((effect :initform "fade" :initarg :effect :accessor effect-of)))

(defclass local-time-based-dojo-widget (generic-html-input local-time-based-date-field dojo-widget)
  ())

(def-dojo-widget (dojo-date-picker "DatePicker") (local-time-based-dojo-widget)
  ((display-format :initarg :display-format :accessor display-format-of))
  (:default-initargs :dom-id (js:gen-js-name-string :prefix "_dj-date")))

(def-dojo-widget (dojo-time-picker "TimePicker") (local-time-based-dojo-widget)
  ()
  (:default-initargs :dom-id (js:gen-js-name-string :prefix "_dj-time")))

(defclass local-time-based-dojo-dropdown-widget (local-time-based-dojo-widget dojo-dropdown)
  ()
  (:metaclass standard-component-class))

(def-dojo-widget (dojo-dropdown-date-picker "DropdownDatePicker") (local-time-based-dojo-dropdown-widget dojo-date-picker)
  ()
  (:default-initargs :dom-id (js:gen-js-name-string :prefix "_dj-dd-date")))

(def-dojo-widget (dojo-dropdown-time-picker "DropdownTimePicker") (local-time-based-dojo-dropdown-widget dojo-time-picker)
  ()
  (:default-initargs :dom-id (js:gen-js-name-string :prefix "_dj-dd-time")))

(defmethod (setf value) ((local-time local-time) (self dojo-dropdown-date-picker))
  (setf (client-value self) (format-rfc3339-timestring local-time :omit-time-part-p t)))

(defmethod (setf value) ((local-time local-time) (self dojo-dropdown-time-picker))
  (setf (client-value self) (format-rfc3339-timestring local-time :omit-date-part-p t)))

(defmethod value ((self dojo-dropdown-date-picker))
  (let ((client-value (client-value self)))
    (when (and client-value
               (not (zerop (length client-value))))
      (parse-timestring client-value :allow-missing-time-part-p t))))

(defmethod value ((self dojo-dropdown-time-picker))
  (let ((client-value (client-value self)))
    (when (and client-value
               (not (zerop (length client-value))))
      ;; HINT: the server's *default-timezone* shoud be +utc-zone+ to minimize headaches
      (parse-timestring client-value :allow-missing-date-part-p t))))

(defmethod render ((self local-time-based-dojo-dropdown-widget))
  ;; TODO when using an <ucw:input then custom yaclml attributes are not propagated to the rendered <:input tag
  (<:input :id (widget-id self)
           :class (css-class self)
           :style (css-style self)
           :accesskey (accesskey self)
           :title (tooltip self)
           :tabindex (tabindex self)
           ;; NOTE: dojo drops this dom node with all its attributes. some of them are copied though...
           (@ "dojo:type" (dojo-type-of self)
              "dojo:inputId" (dom-id self)
              "dojo:name" (register-callback (lambda (value)
                                               (ucw.component.dojo.debug "Received date ~S form the dojo DatePicker" value)
                                               (setf (client-value self) value)))
              "dojo:displayFormat" (when (and (typep self 'dojo-dropdown-date-picker)
                                              (slot-boundp self 'display-format))
                                     (display-format-of self))
              "dojo:value" (client-value self)
              "dojo:containerToggle" (effect-of self))))

(defmethod javascript-init ((field dojo-date-picker) (validator time-range-validator))
  ;; set the dojo dddp's valid range
  (let ((min-value (min-value validator))
        (max-value (max-value validator)))
    `(dojo.add-on-load
      (lambda ()
        (let ((field (dojo.widget.by-id ,(widget-id field)))
              (date-picker))
          (cond
            ((instanceof field dojo.widget.*dropdown-date-picker) (setf date-picker field.date-picker))
            ((instanceof field dojo.widget.*date-picker) (setf date-picker field))
            (t (let ((message (+ "Unexpected date widget type '" field.widget-type "'")))
                 (log.error message field)
                 (throw message))))
          ;; TODO this is messing with dojo internals and will probably break in the future.
          ;; we should call a set-start-date method if one were available...
          ,(when min-value
             `(setf date-picker.start-date (dojo.date.from-rfc3339
                                            ,(format-rfc3339-timestring min-value :omit-timezone-part-p t))))
          ,(when max-value
             `(setf date-picker.end-date (dojo.date.from-rfc3339
                                          ,(format-rfc3339-timestring max-value :omit-timezone-part-p t))))
          ,(when (or min-value max-value)
             `(date-picker._init-u-i 42)))))))

(defclass dojo-timestamp-picker ()
  ((date-picker :accessor date-picker-of :component (dojo-dropdown-date-picker))
   (time-picker :accessor time-picker-of :component (dojo-dropdown-time-picker))
   (value :initform nil :initarg :value))
  (:default-initargs :value (now))
  (:metaclass standard-component-class))

(defmethod shared-initialize :after ((self dojo-timestamp-picker) slot-names &key &allow-other-keys)
  (declare (ignore slot-names))
  (setf (value self) (value self)))

(defmethod value ((self dojo-timestamp-picker))
  (unless (slot-value self 'value)
    (let ((date (value (date-picker-of self)))
          (time (value (time-picker-of self))))
      (multiple-value-bind (usec sec min hour x1 x2 x3 x4 x5 timezone)
          (decode-local-time time)
        (declare (ignore x1 x2 x3 x4 x5))
        (multiple-value-bind (x1 x2 x3 x4 day month year)
            (decode-local-time date)
          (declare (ignore x1 x2 x3 x4))
          (setf (slot-value self 'value)
                (encode-local-time usec sec min hour day month year timezone))))))
  (slot-value self 'value))

(defmethod (setf value) ((local-time local-time) (self dojo-timestamp-picker))
  (setf (value (date-picker-of self)) local-time)
  (setf (value (time-picker-of self)) local-time))

(defmethod render :before ((self dojo-timestamp-picker))
  (setf (slot-value self 'value) nil))

(defmethod render ((self dojo-timestamp-picker))
  (<:table
      (<:tr
       (<:td (render (date-picker-of self)))
       (<:td (render (time-picker-of self))))))


