(in-package :araneida)

(defmacro deftemplate (template-symbol (&rest parameters) &body body)
  "Defines an HTML template.
The template is just a plain lisp function. The template makes
a nice way to wrap it.

Example:
(deftemplate mytemplate (warning)
  `(p \"A regular paragraph\" ,@(when warning (blink \"NOW WITH A BADLY FORMATTED WARNING!\"))))"

  `(setf (get ',template-symbol :araneida-template) (lambda ,parameters ,@body)))


(defun call-template (template &rest parameters)
  "Calls an HTML template
Just like a normal function call

Example:
(call-template 'mytemplate t)"

  (declare (type symbol template))
  (let ((tracep (get template :araneida-template-trace)))
    (when tracep
      (format *trace-output* "template ~S called with parameters ~S" template parameters))
    (let ((result (apply (get template :araneida-template) parameters)))
      (when tracep
	(format *trace-output* "template ~S => ~S" template result))
      result)))

(defmacro trace-template (template)
  "Traces a template, just like tracing a function"
  `(progn
    (warn "tracing ~A" ',template)
    (setf (get ',template :araneida-template-trace) t)))
    
(defmacro untrace-template (template)
  "Untraces a template, just like untracing a function"
  `(progn
    (warn "no longer tracing ~A" ',template)
    (setf (get ',template :araneida-template-trace) nil)))
