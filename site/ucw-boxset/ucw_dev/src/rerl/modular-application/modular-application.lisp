;; See the file LICENCE for licence information.
(in-package :ucw)

;;;; * UCW Moduler Application API - This enables us to mix several
;;;; features of different application together like i18n & cookie.
;;;; Also, an end-user can define his/her own context mixin class to
;;;; provide a modular context which is crucial for real applications.

(defclass modular-application-mixin ()
  ()
  (:documentation "Superclass for all application mixins."))

(defgeneric effective-context-definition (modular-application-mixin)
  (:documentation "Returns the name of the effective context mixin class name.")
  (:method-combination list))

(defclass modular-application (standard-application modular-application-mixin)
  ((request-context-class :accessor request-context-class-of)))

(defmethod effective-context-definition list ((app modular-application))
  'standard-request-context)

(defmethod shared-initialize :after ((app modular-application) slot-names &key &allow-other-keys)
  (setf (request-context-class-of app)
        (make-instance 'standard-class
                       :direct-superclasses (mapcar #'find-class (effective-context-definition app))
                       :name (intern-concat (list (class-name-of app) "-REQUEST-CONTEXT")))))

(defmethod make-request-context ((app modular-application)
				 (request request)
				 (response response))
  (make-instance (request-context-class-of app)
                 :request request
                 :response response
                 :application app))
