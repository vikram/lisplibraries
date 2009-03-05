(in-package #:metatilities)

;;; Interface determination

(defvar *default-interface* nil)

(defun default-interface ()
  "Return the current default interface (this is setfable)."
  *default-interface*)

(defun (setf default-interface) (value)
  (setf *default-interface* value))

(defgeneric is-interface-available-p (interface-name)
  (:documentation "Returns true is interface-name is available."))

(defmethod is-interface-available-p ((interface (eql nil)))
  (values nil))

(defun is-default-interface-available-p ()
  (is-interface-available-p *default-interface*))


;;; quitting

(defgeneric quit-lisp* (interface)
  (:documentation "Quits Lisp"))

(defmethod quit-lisp* (interface)
  (declare (ignore interface))
  (print "I would love to quit for you, but I'm not sure how?"))

(defun quit-lisp ()
  (quit-lisp* *default-interface*))


;;; memory management stuff

(defgeneric total-bytes-allocated* (interface)
  (:documentation "")
  (:method (interface)
           (declare (ignore interface))
           (values nil)))

(defun total-bytes-allocated ()
  "Returns the total number of bytes that this Lisp session has allocated."
  (total-bytes-allocated* *default-interface*))

(defgeneric gc-time* (interface)
  (:documentation "")
  (:method (interface)
           (declare (ignore interface))
           (values nil)))

(defun gc-time ()
  "Returns the total amount of time that this Lisp session has spent in garbage collection."
  (gc-time* *default-interface*))

(defgeneric collect-garbage* (interface)
  (:documentation ""))

(defun collect-garbage ()
  "Tell lisp that now is a good time to collect any accumulated garbage."
  (collect-garbage* *default-interface*))


;;; other

(defmacro make-load-form* (class-name)
  #+(or openmcl (not mcl) ansi-make-load-form)
  `(defmethod make-load-form ((self ,class-name) &optional environment)
    (declare (ignore environment))
    (make-load-form-saving-slots self))
  #+(and digitool (not ansi-make-load-form))
  `(defmethod make-load-form ((self ,class-name))
    (make-load-form-saving-slots self)))

