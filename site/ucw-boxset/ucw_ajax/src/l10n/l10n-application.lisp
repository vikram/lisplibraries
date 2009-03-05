;; See the file LICENCE for licence information.
(in-package :ucw)

(defclass l10n-application-mixin ()
  ((default-locale :initarg :default-locale :initform nil
                   :documentation "Something that the cl-l10n locale function understands")
   (resource-package :initarg :resource-package :accessor application.resource-package
                     :documentation "When bound cl-l10n's *resource-package* is bound to this value.")
   (accepted-locales :initarg :accepted-locales :initform '()
                     :documentation "When not nil the user-requested locales will be filtered according to this list."))
  (:documentation "Application class which can handle l10n requests."))

(defmethod initialize-instance :after ((app l10n-application-mixin) &key &allow-other-keys)
  ;; explicitly call our customized setf's to resolve the locale name
  (awhen (application.default-locale app)
    (setf (application.default-locale app) it))
  (awhen (application.accepted-locales app)
    (setf (application.accepted-locales app) it)))

(defmethod application.default-locale ((app l10n-application-mixin))
  (slot-value app 'default-locale))

(defmethod (setf application.default-locale) ((locale locale) (app l10n-application-mixin))
  (setf (slot-value app 'default-locale) locale))

(defmethod (setf application.default-locale) (locale (app l10n-application-mixin))
  (with-resource-package (application.resource-package app)
    (setf (application.default-locale app) (locale locale))))

(defmethod application.accepted-locales ((app l10n-application-mixin))
  (slot-value app 'accepted-locales))

(defmethod (setf application.accepted-locales) (locales (app l10n-application-mixin))
  (with-resource-package (application.resource-package app)
    (setf (slot-value app 'accepted-locales) (mapcar #'locale locales))))


#|
(defclass l10n-session-mixin ()
  ()
  (:documentation "Session mixin for l10n apps, to store an accept-language-cache slot."))

(defmethod session-class list ((app l10n-application-mixin))
  'l10n-session-mixin)
|#

(defclass l10n-request-context-mixin ()
  ((locale :initarg :locale))
  (:documentation "Request context for l10n apps. Contains one
  extra slot: the list of locales associated with this request.
  This is directly usable with the cl-l10n:with-locale macro."))

(defmethod request-context-class list ((app l10n-application-mixin))
  'l10n-request-context-mixin)

(define-shared-hashtable accept-language-cache :test #'equal :purge-interval-size +accept-language-cache-purge-size+)

(defmethod context.locale (context)
  (current-locale))

(defmethod context.locale ((context l10n-request-context-mixin))
  (if (slot-boundp context 'locale)
      (slot-value context 'locale)
      (progn
        (let* ((locale nil)
               (request (context.request context))
               (app (context.application context))
               (accept-language-value (get-header request "Accept-Language")))
          (setf locale (or (ensure-accept-language-cache-value accept-language-value
                             (ucw.rerl.l10n.info "Cache miss for Accept-Language header value '~S'"
                                                 accept-language-value)
                             (normalize-locale-list (process-accept-language app request)))
                           (normalize-locale-list (list (application.default-locale app)))))
          (ucw.rerl.l10n.debug "Setting context.locale to ~S" locale)
          (setf (context.locale context) locale)))))

(defmethod (setf context.locale) (value (context l10n-request-context-mixin))
  (setf (slot-value context 'locale) value))

(defmethod make-request-context :around ((app l10n-application-mixin) (request request) (response response))
  (if (slot-boundp app 'resource-package)
      (with-resource-package (application.resource-package app)
        (call-next-method))
      (call-next-method)))

(defmethod service :around ((app l10n-application-mixin) (context l10n-request-context-mixin))
  (flet ((body ()
           (aif (context.locale context)
                (with-locale it
                  (ucw.rerl.l10n.debug "Binding *locale* to ~S" it)
                  (call-next-method))
                (progn
                  (ucw.rerl.l10n.debug "Context has no locale, leaving alone *locale*")
                  (call-next-method)))))
    (if (slot-boundp app 'resource-package)
        (with-resource-package (application.resource-package app)
          (body))
        (body))))

(defgeneric process-accept-language (application request)
  (:method ((app l10n-application-mixin) (request request))
           (awhen (get-header request "Accept-Language")
             (when (> (length it) +maximum-accept-language-value-length+)
               (ucw.rerl.l10n.warn "Refusing to parse Accept-Language header value, its length is ~S" (length it))
               (return-from process-accept-language nil))
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

(defclass l10n-application (standard-application l10n-application-mixin)
  ()
  (:documentation "l10n application class for backward
  compatibilty, don't use this if you plan to extend application
  features, instead roll your own app class using application
  mixins."))





