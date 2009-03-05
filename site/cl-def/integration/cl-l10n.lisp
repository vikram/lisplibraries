;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2007 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-def)

(def definer cl-l10n::resources (locale &body resources)
  `(cl-l10n:defresources ,locale ,@resources))

(integrated-export 'cl-l10n::resources :cl-def)
