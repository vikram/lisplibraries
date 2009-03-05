;; See the file LICENCE for licence information.
(in-package :ucw)

;;;; *** Cookie session

(defvar +ucw-session-cookie-name+ "ucw-session-id"
  "Name of the cookie used when storing the session id.")

(defclass cookie-session-application-mixin ()
  ()
  (:documentation "Class for applications which use cookies for sesion tracking.

Cookie session applications work exactly like
standard-applications except that when the session is not found
using the standard mechanisms the id is looked for in a cookie."))

(defmethod request-context-class list ((app cookie-session-application-mixin))
  'cookie-session-request-context-mixin)

(defclass cookie-session-request-context-mixin ()
  ())

(defmethod (setf context.session) :after
    ((session standard-session) (context cookie-session-request-context-mixin))
  (add-set-cookie-header (context.response context)
                         (rfc2109:make-cookie :name +ucw-session-cookie-name+
                                              :value (session.id session)
                                              :path (application.url-prefix (context.application context)))))

(defmethod find-session-id :around ((context cookie-session-request-context-mixin))
  (or (call-next-method)
      (context.cookie-value context +ucw-session-cookie-name+)))

(defclass cookie-session-application (standard-application cookie-session-application-mixin)
  ()
  (:documentation "Cookie session application class for backward
  compatibilty, don't use this if you plan to extend application
  features, instead roll your own app class using application
  mixins."))
