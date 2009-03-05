;;; -*- lisp -*-

;;;; * Startup file for ucw_ajax

(in-package :common-lisp-user)

(defparameter *ucw-directory* (merge-pathnames #P"ucw_ajax/" *load-truename*))

(load (merge-pathnames #P"start-common.lisp" *load-truename*))
