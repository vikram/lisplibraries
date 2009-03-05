;;; -*- lisp -*-

;;;; * Example configuration file for UCW


(in-package :it.bese.ucw)

;;;; These are the default values as in the default.lisp in UCW,
;;;; thus you can safely omit them if they are ok for you.

(setf *ucw-swank-port* 4005)

(setf *ucw-backend-type* ':httpd)
(setf *ucw-backend-host* "127.0.0.1")
(setf *ucw-backend-port* 8080)
(setf *ucw-server-class* 'standard-server)

(setf *ucw-applications-directory* "/etc/ucw/applications.d/")

(setf *ucw-systems* '(:ucw.admin :ucw.examples))
(setf *ucw-applications* '(ucw-user::*example-application*
                           ucw-user::*l10n-example-application*
                           ucw::*admin-application*))


(setf *inspect-components* t)
(setf *debug-on-error* t)
(setf *ucw-log-level* +info+)
