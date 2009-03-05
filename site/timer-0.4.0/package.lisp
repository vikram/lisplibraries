;;;; 
;;;; package.lisp
;;;; 
;;;; Created: 2003-11-07 by Zach Beane <xach@xach.com>
;;;; 
;;;; See the file COPYING for license information.
;;;; package.lisp,v 1.2 2003/11/09 17:06:23 xach Exp

(defpackage #:timer
  (:use #:cl)
  (:export
   #:make-timer
   #:schedule-timer #:schedule-timer-relative
   #:timer-expired-p #:timer-name
   #:unschedule-timer
   #:enable-timers #:timers-enabled-p))

  



