(in-package :ucw)

(defcomponent user-login (simple-window-component status-bar-mixin)
  ((username :accessor username
             :initarg :username
             :initform (make-instance 'string-field
                                      :input-size 10
                                      :validators (list (make-instance 'not-empty-validator)))
             :documentation "User login name.")
   (password :accessor password
             :initarg :password
             :initform (make-instance 'password-field
                                      :input-size 10
                                      :validators (list (make-instance 'not-empty-validator)))
             :documentation "User password."))
  (:documentation "User login component.")
  (:default-initargs
    :title "User Login"
    :stylesheet "/ucw/ucw.css"))

(defmethod render ((self user-login))
  (render (status-bar self))
  (<:table
   (<:tr
    (<:td "Username")
    (<:td (render (username self))))
   (<:tr
    (<:td "Password")
    (<:td (render (password self))))
   (<:tr
    (<:td :colspan 2
          (<ucw:input :type "submit" :action (submit self) :value "Ok")
          (<ucw:input :type "submit" :action (cancel self) :value "Cancel")))))

(defmethod render :before ((self user-login))
  (show-message "Please enter login and password."))

(defmethod report-error ((self user-login) slot-name condition)
  (show-message (format nil "~a - ~a~%"
                            (label (slot-value self slot-name)) condition)
                    :severity :error))

(defaction cancel ((self user-login))
  (answer nil))

(defaction submit ((self user-login))
  (when (validp self)
      (aif (check-credentials self)
           (answer it)
           (show-message "Bad login or password." :severity :error))))

(defmethod check-credentials ((self user-login))
  (let* ((username (value (username self)))
         (password (value (password self)))
         (user (find-application-user username)))
    (when (and user (check-user-password user password))
      user)))
