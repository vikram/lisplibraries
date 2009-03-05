(in-package :cl-user)
(defpackage trivial-sockets
  (:use :CL)
  (:export #:open-stream #:socket-error #:socket-nested-error
	   #:unsupported #:unsupported-feature
	   #:open-server #:close-server #:accept-connection
	   #:with-server))

