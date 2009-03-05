(in-package :it.bese.ucw)

(enable-bracket-reader)

(defclass ajax-component-mixin ()
  ((rendered-a-form-p :initform nil :accessor rendered-a-form-p
                      :documentation "Indicates that this component rendered an <ucw:form tag in its render method.")
   (have-ever-been-rendered-p :initform nil :accessor have-ever-been-rendered-p)
   (forbid-ajax-rendering-p :initform nil :accessor forbid-ajax-rendering-p :initarg :forbid-ajax-rendering-p
                            :documentation "This predicate my forbid AJAX rendering from this component and instruct the renderer to look further on the parent chain. The primary use of this is that sometimes (mostly due to browser rendring bugs) it's better to render bigger chunks of the page.")
   (ajax-rendering-starting-point-p :initform nil :accessor ajax-rendering-starting-point-p
                                    :documentation "While rendering, it marks that AJAX rendering was restarted at this point."))
  (:default-initargs :dom-id (js:gen-js-name-string :prefix "ajax"))
  (:documentation "This is a marker class that marks a point in the component
hierarchy from where a partial (AJAX) render may be started. The component
must render exactly one top-level DOM node and it must have an id attribute.
The client side js will look up the DOM node identified by id and replace
with the freshly rendered one. See also ajax-widget-component."))

(defmethod allow-ajax-rendering-restart ((component ajax-component-mixin))
  (and (not (forbid-ajax-rendering-p component))
       (have-ever-been-rendered-p component)))

(defparameter %ajax-stub-rendering-in-progress% nil
  "Marks that we are going to render only a stub, so bail out in render :wrapping ajax-component-mixin.")

(defgeneric render-ajax-stub (ajax-component)
  (:method :around ((self ajax-component-mixin))
           (let ((%ajax-stub-rendering-in-progress% t))
             (call-next-method)))
  (:method ((self ajax-component-mixin))
           (render self))
  (:documentation "Start rendering and stop at ajax-component-mixin boundaries. Only render a stub at those points (usually a <:div with an id) that can be later lazily replaced with an AJAX request."))

(defmethod render :wrap-around ((self ajax-component-mixin))
  (setf (rendered-a-form-p self) nil)
  (prog1
      (call-next-method)
    (setf (have-ever-been-rendered-p self) t)))

(defmethod render :wrapping ((self ajax-component-mixin))
  (unless %ajax-stub-rendering-in-progress%
    (call-next-method)))

(defgeneric ajax-render (component)
  (:documentation "This method is called when we are rendering parts of the component hierarchy with AJAX.
By default it simply calls render after marking this fact on the ajax-component-mixin.")
  (:method :around ((self ajax-component-mixin))
           (setf (ajax-rendering-starting-point-p self) t)
           (unwind-protect
                (call-next-method)
             (setf (ajax-rendering-starting-point-p self) nil)))
  (:method ((self ajax-component-mixin))
           (render self)))

(defmacro within-xhtml-tag (tag-name &body body)
  "Execute BODY and wrap its yaclml output in a TAG-NAME xml node
with \"http://www.w3.org/1999/xhtml\" xml namespace."
  `{with-xml-syntax
     <(progn ,tag-name) :xmlns "http://www.w3.org/1999/xhtml"
                        (@ "xmlns:dojo" "http://www.dojotoolkit.org/2004/dojoml")
       ,@body>})

(defmacro within-dom-replacements-tag (&body body)
  "Execute BODY and wrap its yaclml output in a dom-replacements xml node
with \"http://www.w3.org/1999/xhtml\" xml namespace. Client side js
iterates the elements of this node and replaces their counterparts
in the DOM tree with them."
  `(within-xhtml-tag "dom-replacements"
     ,@body))

(defun render-nearest-ajax-component (component &key (at-form-boundary-p t)
                                                (wrap-in-dom-replacements t))
  (ucw.rerl.ajax.debug "render-nearest-ajax-component from ~S, at-form-boundary-p ~S" component at-form-boundary-p)
  (let ((ajax-component (iter (with winner)
                              (with seen-ajax-form)
                              (for current first component then (parent current))
                              (while current)
                              (while (or (not winner)
                                         at-form-boundary-p))
                              (ucw.rerl.ajax.dribble "Checking ~S, while winner is ~S" current winner)
                              (when (typep current 'ajax-component-mixin)
                                (when (and (not seen-ajax-form)
                                           (rendered-a-form-p current)
                                           at-form-boundary-p)
                                  ;; mark that we have found an ajax
                                  ;; component which rendered a form
                                  (setf seen-ajax-form t)
                                  (setf winner nil))

                                (when (and (not winner)
                                           (allow-ajax-rendering-restart current))
                                  ;; store the next candidate
                                  (setf winner current)))

                              (while (slot-boundp current 'parent))
                              (finally (return winner)))))
    (ucw.rerl.ajax.debug "render-nearest-ajax-component ended up at ~S" ajax-component)
    (unless ajax-component
      (error "No suitable ajax-component-mixin was found while walking the parent slots of ~A, unable to render AJAX answer" component))
    (if wrap-in-dom-replacements
        (within-dom-replacements-tag
          (ajax-render ajax-component))
        (ajax-render ajax-component))))

(defun ajax-render-dirty-components ()
  (within-dom-replacements-tag
    (iterate-dirty-components
     (lambda (component)
       (ucw.rerl.ajax.debug "ajax-render-dirty-components at component ~S" component)
       (render-nearest-ajax-component component :wrap-in-dom-replacements nil)))
    (debug-only
      (iterate-dirty-components
       (lambda (c)
         (when (visiblep c)
           (restart-case (error "A visible dirty component ~A remained in session ~A after calling ajax-render-dirty-components. This would lead to a constant ajax rerendering in the poller. Make sure you either render all connected components or detach them!"
                                c (session-of c))
                         (remove-dirtyness () :report (lambda (stream)
                                                        (format stream "Remove dirtyness"))
                                           (setf (dirtyp c) nil)
                                           (values nil t)))))))))


(defmacro js-server-callback ((&rest args &key
                                     (invocation-isolated nil invocation-isolated-provided-p)
                                     &allow-other-keys) &body body)
  "This macro can be used to define unnamed server callbacks in parenscript bodies."
  (let ((action-args nil))
    (when invocation-isolated-provided-p
      (push* action-args invocation-isolated :invocation-isolated))
    (remf-keywords args :invocation-isolated)
    ``(ucw.io.execute-ajax-action
       (create :url
        ,(action-href
          (register-ajax-action (,@action-args)
            ,@body))
        ,,@args))))

