;;;; -*- lisp -*-

(defpackage :it.bese.yaclml.test
  (:use :common-lisp
        :arnesi
        :iterate
	:it.bese.yaclml
	:it.bese.FiveAM))

(unless (5am:get-test :it.bese)
  (5am:def-suite :it.bese))

(5am:def-suite :it.bese.yaclml :in :it.bese)
