;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2007 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-def)

;; we define "TEST" with a string name, so it will match (def test ...) no matter what is the
;; home package of 'test. this is done like that to be able to write
;; (def any-package::test some-lib::test ...) in any library to create the toplevel TEST
;; defun called some-lib::test without shadowing stefil:test everywhere.
(def (definer :available-flags "do") "TEST" ()
  (function-like-definer -definer- 'stefil:deftest -whole- -environment- -options-))

(def definer stefil::suite (name &rest args)
  `(stefil:defsuite ,name ,@args))

(integrated-export 'stefil::suite :cl-def)
