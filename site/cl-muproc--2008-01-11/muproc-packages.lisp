;;; Copyright (c) 2005-2006, Mu Aps. All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; WRITTEN BY: KLAUS HARBO klaus@harbo.net / klaus@mu.dk

(defpackage :cl-muproc.compat
  (:documentation "For cl-muproc to be supported by an implementation,
a implementation of all symbols exported from CL-MUPROC.COMPAT must be
provided.  See PORTING file in cl-muproc distribution for more
information.")
  (:use :common-lisp #+(or sbcl cmu) :bordeaux-threads)
  (:export 
   %all-processes%
   %current-process%
   %dequeue%
   %enqueue%
   %make-lock%
   %with-lock%
   %make-queue%
   %make-timer%
   %process-alive-p%
   %process-interrupt%
   %process-p%
   %process-plist%
   %process-priority%
   %process-run-function%
   %queue-empty-p%
   %schedule-timer%
   %schedule-timer-relative%
   %unschedule-timer%
   %with-debugging-stack%
   %with-exclusive-access%
   %with-timeout%
   ))

(defpackage :cl-muproc
  (:use :common-lisp :cl-muproc.compat)
  (:nicknames :muproc)
  (:export
   #:*muproc-errorstream*
   #:*muproc-inport*
   #:*muproc-mumsg*
   #:*muproc-packet*
   #:in-muproc-p
   #:mumsg
   #:mumsg-p
   #:mumsg-receive
   #:mumsg-send
   #:mupacket-p
   #:muproc-address-p
   #:muproc-all-processes
   #:muproc-current-process
   #:muproc-discard-all-pending-input
   #:muproc-exit
   #:muproc-exit-condition
   #:muproc-find
   #:muproc-get-field
   #:muproc-get-registered-port
   #:muproc-kill
   #:muproc-link
   #:muproc-log-errorstream
   #:muproc-make-interrupt-timer
   #:muproc-monitor
   #:muproc-msgtag=
   #:muproc-name
   #:muproc-name-p
   #:muproc-p
   #:muproc-packet-age
   #:muproc-port
   #:muproc-port-name-p
   #:muproc-receive
   #:muproc-register-port-name
   #:muproc-schedule
   #:muproc-schedule-relative
   #:muproc-schedule-timer
   #:muproc-schedule-timer-relative
   #:muproc-send
   #:muproc-send-to-terminated-muproc-condition
   #:muproc-set-trap-exits
   #:muproc-spawn
   #:muproc-trap-exits-p
   #:muproc-unmatched-input-count
   #:muproc-unmatched-input-p
   #:muproc-unregister-port-name
   #:muproc-unschedule-timer
   #:muproc-with-timeout
   #:muproc-with-message-tag
   #:muproc-with-registered-port
   #:muprocn
   ))

(defpackage :cl-muproc.generic-server
  (:use :cl :cl-muproc :cl-muproc.compat)
  (:nicknames :muproc.generic-server :cl-muproc.gensrv :muproc.gensrv)
  (:export
   #:*muproc-generic-server-package*
   #:*muproc-generic-server-port-name*
   #:muproc-default-server-name
   #:muproc-define-call-handler
   #:muproc-define-cast-handler
   #:muproc-exit-after-handler
   #:muproc-generic-call
   #:muproc-generic-cast
   #:muproc-generic-start
   #:muproc-generic-stop
   ))

(defpackage :cl-muproc.supervisor
  (:use :cl :cl-muproc :cl-muproc.compat)
  (:nicknames :muproc.supervisor)
  (:export
   #:supervised-muproc
   #:supervisor
   #:supervisor-start
   ))

;;eof