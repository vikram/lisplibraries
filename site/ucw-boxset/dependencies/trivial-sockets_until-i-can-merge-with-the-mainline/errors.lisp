(in-package :trivial-sockets)

;; you're using a part of the interface that the implementation doesn't do
(define-condition unsupported (error) 
  ((feature :initarg :feature :reader unsupported-feature))
  (:report (lambda (c s)
             (format s "~S does not support trivial-socket feature ~S."
                     (lisp-implementation-type) (unsupported-feature c)))))


;; all-purpose error: host not found, host not responding,
;; no service on that port, etc
(define-condition socket-error (error)
  ((nested-error :initarg :nested-error :reader socket-nested-error)))

