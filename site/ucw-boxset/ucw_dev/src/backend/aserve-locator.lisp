;; -*- lisp

(in-package :net.aserve)

;;;; ** The aserve locator

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass locator-pattern (locator)
    ())

  (arnesi:defclass-struct (pattern-handler (:conc-name pattern-handler-)) ()
    (pattern       nil :documentation "cl-ppcre compatible regular expression pattern")
    (scanner       nil :documentation "scanner for the regexp")
    (host-handlers nil :documentation "list of host-handlers")))

(defmethod standard-locator ((req http-request) (locator locator-pattern))
  "standard function for finding an entity in an exact locator
   return the entity if one is found, else return nil"
  (if (uri-scheme (request-raw-uri req))
      (return-from standard-locator nil))
  (let ((url (request-decoded-uri-path req))
        (vhost (request-vhost req)))
    (dolist (handler (locator-info locator))
      (multiple-value-bind (result registers)
          (ppcre:scan-to-strings (pattern-handler-scanner handler) url)
        (declare (ignore registers))
        (when result
          (let ((hh (or (assoc vhost (pattern-handler-host-handlers handler)
                               :test #'eq)
                        (assoc :wild (pattern-handler-host-handlers handler)
                               :test #'eq))))
            (when hh
              (return (host-handler-entity hh)))))))))

(defclass pattern-entity (computed-entity)
  ((pattern :initarg :pattern
            :initform nil
            :reader pattern)))

(defun publish-pattern-entity (ent pattern locator host host-p remove)
  (dolist (entpair (locator-info locator))
    (when (equal (pattern-handler-pattern entpair) pattern)
      (when (and remove (not host-p))
        (setf (locator-info locator)
              (remove entpair (locator-info locator)))
        (return-from publish-pattern-entity nil))
      (let ((handlers (pattern-handler-host-handlers entpair)))
        (dolist (host host)
          (dolist (hostpair handlers
                   (when (not remove)
                     (push (make-host-handler :host host :entity ent)
                           handlers)))
            (when (eq host (host-handler-host hostpair))
              (if remove
                  (setq handlers (remove hostpair handlers :test #'eq))
                  (setf (host-handler-entity hostpair) ent))
              (return))))
        (setf (pattern-handler-host-handlers entpair) handlers)
        (return-from publish-pattern-entity ent))))
  (when (not remove)
    (let ((new-ent (make-instance 'pattern-handler
                                  :pattern pattern
                                  :scanner (ppcre:create-scanner pattern)
                                  :host-handlers (mapcar #'(lambda (host)
                                                             (make-host-handler :host host
                                                                                :entity ent))
                                                         host))))
      (push new-ent (locator-info locator)))))

(defun publish-pattern
    (&key (host nil host-p) port pattern
     function class format
     content-type
     (server *wserver*)
     locator
     remove
     authorizer
     timeout
     plist
     hook
     headers)
  (let (hval)
    (when (null locator) 
      (setq locator (find-locator :pattern server)))
    (setq hval (convert-to-vhosts
                (if (and host (atom host))
                    (list host)
                    host)
                server))
    (if remove ; eliminate the entity if it exists
        (progn
          (publish-pattern-entity nil pattern locator hval host-p t)
          nil)
        (let ((ent (make-instance (or class 'pattern-entity)
                                  :host hval 
                                  :port port
                                  :pattern pattern
                                  :function function
                                  :format format
                                  :content-type content-type
                                  :authorizer authorizer
                                  :plist plist
                                  :hook hook
                                  :timeout timeout
                                  :headers headers)))
          (publish-pattern-entity ent pattern locator hval
                                  host-p nil)
          ent))))

;; Copyright (c) 2004, Anthony W. Juckel
;; All rights reserved. 
;; 
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;; 
;;  - Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 
;;  - Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 
;;  - Neither the name of Edward Marco Baringer, nor BESE, nor the names
;;    of its contributors may be used to endorse or promote products
;;    derived from this software without specific prior written permission.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
