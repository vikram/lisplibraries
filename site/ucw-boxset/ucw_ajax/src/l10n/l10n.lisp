;; See the file LICENCE for licence information.
(in-package :ucw)

(defun reload-ucw-resources ()
  "This method reloads the UCW resources needed by some UCW services. Invoke
   after your :lang package has been set up."
  (flet ((system-relative-pathname (system path)
           (merge-pathnames path (asdf:component-pathname
                                  (asdf:find-system system)))))
    (let ((resource-dir (system-relative-pathname
                         :ucw (make-pathname :directory (list :relative "src" "l10n" "resources")
                                             :name :wild
                                             :type "lisp"))))
      (ucw.rerl.l10n.debug "Looking for UCW resources in dir ~S" resource-dir)
      (dolist (resource-file (directory resource-dir))
        (ucw.rerl.l10n.info "Loading UCW resource file ~S" resource-file)
        (load resource-file)))))

(defmacro define-js-resources (locale &body resources)
  "Wraps defresources and registers the resources being defined
as js resources, so that they are sent down to the client side."
  `(progn
    (register-js-resources
     ,@(iter (for (name . body) in resources)
             (when (> (length body) 1)
               (error "How should we send functional resources to js? ~S has arguments..." name))
             (collect `',name)))
    (defresources ,locale ,@resources)))

(defclass l10n-tal-generator (file-system-generator)
  ()
  (:documentation "TAL File system generator which maps template
  names to files while considering the locale of the request
  context."))

(defmethod yaclml:template-truename ((generator l10n-tal-generator) (name pathname))
  ;; first see if there's a locale specific version
  (when-bind locales (context.locale *context*)
    (ucw.rerl.l10n.debug "Looking for tal directory with locales ~A" locales)
    (iter (for locale in locales)
          (for file = (make-pathname :defaults name
                                     :directory (list :relative (locale-name locale))))
          (ucw.rerl.l10n.debug "Trying tal file ~A" file)
          (awhen (call-next-method generator file)
            (ucw.rerl.l10n.debug "Found locale specific tal directory ~A" it)
            (return-from yaclml:template-truename it))))
  ;; no locale for this context, use the default
  (ucw.rerl.l10n.debug "No locale specific tal directory found for locale file ~A" name)
  (call-next-method))







