;;; -*- lisp -*-

;;;; * Startup file for ucw_dev

(in-package :common-lisp-user)

(defparameter *ucw-directory* (merge-pathnames #P"ucw_dev/" *load-truename*))

(load (merge-pathnames #P"start-common.lisp" *load-truename*))
