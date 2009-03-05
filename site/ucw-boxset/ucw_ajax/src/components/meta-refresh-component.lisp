(in-package :ucw)

(defcomponent meta-refresh-component (window-component)
  ()
  (:documentation "A component, that when call-as-window ed,
 will cause a meta-refresh back to the original components.
You probably want to just call the function #'meta-refresh directly." ))

(defmethod render ((mrc meta-refresh-component))
  (let ((refresh-url (action-href-body () (ok mrc))))
    (<:html
     (<:head
      (<:meta :http-equiv "refresh"
	      :content (format nil "0,~a" refresh-url))))))

(defun/cc meta-refresh ()
  "Cause a meta-refresh (a freshly got (GET) url) at this point.
This is useful in order to have a GET url after a form POST's
actions have completed running. The user can then refresh to his
heart's content."
  (call-as-window 'meta-refresh-component))

;;The form's action would then be
;(progn (frob-my-form form)
;       (meta-refresh)
;       (incf (refreshes-of form)))

;;when the form submits it executes the action, which does the frobbing,
;;then renders the meta-refresh-component, which causes the refresh.
;;The action associated with the refresh, 'ok, simply returns to where we came from,
;; the form's action. It then proceeds to evaluate (incf (refreshes-of form)),
;; and it will evaluate this s-expr every time the user refreshes the resulting page
;; (without the "This was a post" error message).