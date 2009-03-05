;;; -*- lisp -*-

;;;; * Example configuration file for UCW


(in-package :it.bese.ucw)

;;;; These are the default values as in the default.lisp in UCW,
;;;; thus you can safely omit them if they are ok for you.

(setf *ucw-swank-port* 4005)

(setf *ucw-backend-type* ':httpd)
(setf *ucw-backend-host* "127.0.0.1")
(setf *ucw-backend-port* 2001)
(setf *ucw-server-class* 'standard-server)

(setf *ucw-applications-directory* "/etc/ucw/applications.d/")

#- (or allegro openmcl) ;; cl-l10n is incompatible with at least allegro and openmcl
(progn 
  (setf *ucw-systems* '(:ucw.admin :ucw.examples :ucw.examples.l10n))
  (setf *ucw-applications* '(ucw-user::*example-application*
                             ucw-user::*l10n-example-application*
                             ucw::*admin-application*)))
#+ (or allegro openmcl)
(progn 
  (setf *ucw-systems* '(:ucw.admin :ucw.examples))
  (setf *ucw-applications* '(ucw-user::*example-application*
                             ucw::*admin-application*)))

(setf *inspect-components* t)
(setf *debug-on-error* t)
(setf *ucw-log-level* +info+)
