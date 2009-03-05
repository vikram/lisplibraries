;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: modlisp -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          signals.lisp
;;;; Purpose:       Signal processing functions
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Jan 2007
;;;;
;;;; $Id: processes.lisp 10985 2006-07-26 18:52:03Z kevin $
;;;; *************************************************************************

(in-package #:kmrcl)

(defun signal-key-to-number (sig)
  "These signals and numbers are only valid on POSIX systems, perhaps
some are Linux-specific."
  (case sig
    (:hup 1)
    (:int 2)
    (:quit 3)
    (:kill 9)
    (:usr1 10)
    (:usr2 12)
    (:pipe 13)
    (:alrm 14)
    (:term 15)
    (t
     (error "Signal ~A not known." sig))))


(defun set-signal-handler (sig handler)
  "Sets the handler for a signal to a function. Where possible, returns
the old handler for the function for later restoration with remove-signal-handler
below.

To be portable, signal handlers should use (&rest dummy) function signatures
and ignore the value. They should return T to tell some Lisp implementations (Allegro)
that the signal was successfully handled."
  (let ((signum (etypecase sig
                  (integer sig)
                  (keyword (signal-key-to-number sig)))))
    #+allegro (excl:add-signal-handler signum handler)
    #+cmu (system:enable-interrupt signum handler)
    #+(and lispworks unix)
    ;; non-documented method to get old handler, works in lispworks 5
    (let ((old-handler (when (and (boundp 'system::*signal-handler-functions*)
                                  (typep system::*signal-handler-functions* 'array))
                         (aref system::*signal-handler-functions* signum))))
      (system:set-signal-handler signum handler)
      old-handler)
    #+sbcl (sb-sys:enable-interrupt signum handler)
    #-(or allegro cmu (and lispworks unix) sbcl)
    (declare (ignore sig handler))
    #-(or allegro cmu (and lispworks unix) sbcl)
    (warn "Signal setting not supported on this platform.")))

(defun remove-signal-handler (sig &optional old-handler)
  "Removes a handler from signal. Tries, when possible, to restore old-handler."
  (let ((signum (etypecase sig
                  (integer sig)
                  (keyword (signal-key-to-number sig)))))
    ;; allegro automatically restores old handler, because set-signal-handler above
    ;; actually pushes the new handler onto a list of handlers
    #+allegro (declare (ignore old-handler))
    #+allegro (excl:remove-signal-handler signum)
    #+cmu (system:enable-interrupt signum (or old-handler :default))
    ;; lispworks removes handler if old-handler is nil
    #+(and lispworks unix) (system:set-signal-handler signum old-handler)
    #+sbcl (sb-sys:enable-interrupt signum (or old-handler :default))
    #-(or allegro cmu (and lispworks unix) sbcl)
    (declare (ignore sig handler))
    #-(or allegro cmu (and lispworks unix) sbcl)
    (warn "Signal setting not supported on this platform.")))
