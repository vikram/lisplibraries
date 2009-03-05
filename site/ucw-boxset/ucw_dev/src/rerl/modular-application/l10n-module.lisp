;; See the file LICENCE for licence information.
(in-package :ucw)

(export
 '(l10n-application
   application.default-locale
   application.accepted-locales
   l10n-request-context
   context.locale
   l10n-tal-generator))

(eval-always
  (defucwlogger ucw.rerl.l10n (ucw.rerl))
  (defun import-cl-l10n-symbols (&optional (package *package*))
    "Import the public l10n symbols into PACKAGE"
    (iter (for sym in-package :cl-l10n :external-only t)
          (import sym package)))
  (import-cl-l10n-symbols (find-package :it.bese.ucw)))

(defclass l10n-application-module (modular-application-mixin)
  ((default-locale :initarg :default-locale :initform nil
                   :documentation "Something that the cl-l10n locale function understands")
   (resource-package :initarg :resource-package :accessor application.resource-package
                     :documentation "When bound cl-l10n's *resource-package* is bound to this value.")
   (accepted-locales :initarg :accepted-locales :initform '()
                     :documentation "When not nil the user-requested locales will be filtered according to this list."))
  (:documentation "Application class which can handle l10n requests."))

(defmethod initialize-instance :after ((app l10n-application-module) &key &allow-other-keys)
  ;; explicitly call our customized setf's to resolve the locale name
  (awhen (application.default-locale app)
    (setf (application.default-locale app) it))
  (awhen (application.accepted-locales app)
    (setf (application.accepted-locales app) it)))

(defmethod application.default-locale ((app l10n-application-module))
  (slot-value app 'default-locale))

(defmethod (setf application.default-locale) ((locale locale) (app l10n-application-module))
  (setf (slot-value app 'default-locale) locale))

(defmethod (setf application.default-locale) (locale (app l10n-application-module))
  (with-resource-package (application.resource-package app)
    (setf (application.default-locale app) (locale locale))))

(defmethod application.accepted-locales ((app l10n-application-module))
  (slot-value app 'accepted-locales))

(defmethod (setf application.accepted-locales) (locales (app l10n-application-module))
  (with-resource-package (application.resource-package app)
    (setf (slot-value app 'accepted-locales)
          (iter (for locale in locales)
                (collect (locale locale))))))

(defmethod effective-context-definition list ((app l10n-application-module))
  'l10n-request-context-mixin)

(defclass l10n-request-context-mixin ()
  ((locale :accessor context.locale :initarg :locale :initform nil))
  (:documentation "Request context for l10n apps. Contains one
  extra slot: the list of locales associated with this request.
  This is directly usable with the cl-l10n:with-locale macro."))

(defmethod make-request-context :around ((app l10n-application-module) (request request) (response response))
  (flet ((body ()
           (let ((context (call-next-method)))
             (setf (context.locale context) (normalize-locale-list
                                             (or (process-accept-language app request)
                                                 (list (application.default-locale app)))))
             (ucw.rerl.l10n.debug "Setting context.locale to ~S" (context.locale context))
             context)))
    (if (slot-boundp app 'resource-package)
        (with-resource-package (application.resource-package app)
          (body))
        (body))))

(defmethod service :around (app (context l10n-request-context-mixin))
  (declare (ignore app))
  (aif (context.locale context)
       (with-locale it
         (call-next-method))
       (call-next-method)))

(defmethod service :around ((app l10n-application-module) context)
  (if (slot-boundp app 'resource-package)
      (with-resource-package (application.resource-package app)
        (call-next-method))
      (call-next-method)))

(defgeneric  process-accept-language (application request)
  (:method ((app l10n-application-module) (request request))
           (awhen (get-header request "Accept-Language")
             (let ((langs (parse-accept-language-header it)))
               (ucw.rerl.l10n.debug "Parsed language header ~S, app default locale is ~S, app accepted locales are ~S"
                                    langs (application.default-locale app) (application.accepted-locales app))
               (iter (with accepted-locales = (application.accepted-locales app))
                     (for (lang weigth) in langs)
                     (for locale = (locale lang :errorp nil))
                     (ucw.rerl.l10n.debug "Looked up locale ~S from ~S" locale lang)
                     (unless locale
                       (next-iteration))
                     (when (and (or (not accepted-locales)
                                    (member locale accepted-locales))
                                (not (member locale result :key #'car)))
                       (collect (cons locale weigth) :into result))
                     (finally
                      (setf result (mapcar #'car (sort result #'> :key #'cdr)))
                      (awhen (locale (application.default-locale app))
                        (unless (member it result :test #'eq)
                          (setf result (nconc result (list it)))))
                      (ucw.rerl.l10n.debug "The final sorted locale list is ~S" result)
                      (return result)))))))

(defclass l10n-application (modular-application l10n-application-module)
  ()
  (:documentation "l10n application class for backward
  compatibilty, don't use this if you plan to extend application
  features, instead roll your own app class using application
  modules."))

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

(defpackage :it.bese.ucw.l10n.tags
  (:nicknames :<l10n))

(pushnew (cons "http://common-lisp.net/project/ucw/l10n/core"
               (find-package :it.bese.ucw.l10n.tags))
         yaclml:*uri-to-package*
         :test #'equal)

(deftag <l10n::lookup (&attribute params resource &body body)
  (emit-html `(lookup-resource ,resource ,params)))
