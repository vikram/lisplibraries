;;;; Copyright (c) 2004-2005, Dirk H.P. Gerrits
;;;; All rights reserved.
;;;;
;;;; This software is licensed under the terms of the BSD license, stated in the
;;;; file named LICENSE.
;;;;
;;;;----------------------------------------------------------------------------
;;;;
;;;; This file contains the FiveAM test suite definition for Erlisp.  To run it,
;;;; make sure Erlisp and Marco Baringer's FiveAM are installed and accessible
;;;; through ASDF:*CENTRAL-REGISTRY* and then evaluate:
;;;;
;;;;   (asdf:operate 'asdf:test-op :erlisp)
;;;;
;;;; Once run, you can re-run them with the same command, or with a simple:
;;;;
;;;;   (5am:!)
;;;;
;;;;----------------------------------------------------------------------------

(in-package :erlisp)

(use-package :5am)

(5am:def-suite :erlisp)
