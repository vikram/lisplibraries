;;;; Copyright (c) 2004-2005, Dirk H.P. Gerrits
;;;; All rights reserved.
;;;;
;;;; This software is licensed under the terms of the BSD license, stated in the
;;;; file named LICENSE.
;;;;
;;;;----------------------------------------------------------------------------
;;;;
;;;; This file implements distributed nodes.
;;;;
;;;;----------------------------------------------------------------------------

(in-package :erlisp)

(defclass node ()
  ()
  (:documentation "A node in a network."))

(defclass local-node (node)
  ()
  (:documentation "A local node in a network."))

(defclass remote-node (node)
  ()
  (:documentation "A non-local node in a network."))

(defvar *current-node* (make-instance 'local-node))

(defun current-node ()
  "Return the current/local node."
  *current-node*)
