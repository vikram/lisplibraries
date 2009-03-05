;;;; -*- lisp -*-
;; See the file LICENCE for licence information.

(in-package :it.bese.ucw)

(enable-js-sharpquote-reader)

;; forward log.* to dojo.logging.log.*
(macrolet ((forward (name level)
             `(js:defjsmacro ,(intern-concat (list 'log. name)) (&rest args)
               (when (>= ,level ,*ucw-compile-time-log-level*)
                 (cons ',(intern-concat (list 'dojo.log. name)) args)))))
  (forward debug +debug+)
  (forward info +info+)
  (forward warn +warn+)
  (forward error +error+)
  (forward critical +fatal+))

(js:defjsmacro with-this (this &body body)
  `(.call (lambda ()
            ,@body)
    ,this))

(js:import-macros-from-lisp 'when-bind 'awhen 'if-bind 'aif 'cond)

(js:defjsmacro defjsfun (name args &body body)
  `(setf ,name
    (lambda ,(iter (for arg in args)
                   (collect (if (listp arg)
                                (car arg)
                                arg)))
      ,@(iter (for arg in args)
              (when (listp arg)
                (unless (= (length arg) 2)
                  (error "Hm, what do you mean by ~S?" arg))
                (let ((name (first arg)))
                  (if (eq (second arg) :by-id)
                      (collect `(setf ,name ($ ,name)))
                      (collect `(when (= ,name undefined)
                                 (setf ,name ,(cadr arg))))))))
      ,@body)))

(js:defjsmacro into-namespace (name &body definitions)
  (js:with-unique-js-names (package)
    `(let ((,package (dojo.eval-obj-path ,(js::symbol-to-js name) true)))
      ,@(iter (for def in definitions)
              (for def-name = (if (and (consp def)
                                       (symbolp (first def)))
                                  (symbol-name (first def))
                                  ""))
              (cond
                ((or (string= (symbol-name '#:defjsfun) def-name)
                     (string= (symbol-name '#:defun) def-name))
                 (collect `(defjsfun (slot-value ,package ',(cadr def)) ,@(cddr def))))
                ((string= (symbol-name '#:setf) def-name)
                 (collect `(setf ,@(iter (for (name value) on (rest def) by #'cddr)
                                         (collect `(slot-value ,package ',name))
                                         (collect value)))))
                (t (collect def)))))))

(js:defjsmacro with-ucw-error-handler (&body body)
  `(try
    (progn ,@body)
    (:catch (e)
      (if (= e "graceful-abort")
          (log.debug "Gracefully aborting execution and returning to toplevel")
          (progn
            (log.warn "Exception reached toplevel")
            ,(if (debug-on-error (context.application *context*))
                 `debugger
                 `(let ((message #"unknown-error-while-processing-server-answer"))
                   (alert (+ "UCW: " message)))))))))

(js:defjsmacro with-ajax-answer ((data) &body body)
  (js:with-unique-js-names (result-node)
    `(progn
      (log.info "Processing AJAX answer")
      (setf ,data (ucw.get-first-child-by-tag-name ,data "answer"))
      (unless ,data
        (log.warn "AJAX answer node is nil, probably a malformed response, maybe a full page load due to an unregistered action id?")
        (throw (new (ucw.server-communication-error "AJAX answer is empty"))))
      (log.debug "Found answer DOM node? " (not (not ,data)))
      (let ((,result-node (ucw.get-first-child-by-tag-name ,data "result")))
        (if (or (not ,result-node)
                (not (= (dojo.dom.text-content ,result-node)
                        "success")))
            (let ((error-message #"unknown-server-error"))
              (when-bind error-node (ucw.get-first-child-by-tag-name ,data "error-message")
                (setf error-message (dojo.dom.text-content error-node)))
              (unless (dojo.string.is-blank error-message)
                (alert error-message))
              (throw "graceful-abort"))
            ,@body)))))

(js:defjsmacro assert (expression &rest args-to-throw)
  (unless args-to-throw
    (setf args-to-throw (list (strcat "Assertion failed: " (princ-to-string expression)))))
  `(unless ,expression
    ,@(when (debug-on-error (context.application *context*))
        '(debugger))
    (throw ,@args-to-throw)))
