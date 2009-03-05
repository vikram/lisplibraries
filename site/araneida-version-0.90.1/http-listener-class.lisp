(in-package :araneida)

(defclass http-listener ()
  ((handler :initform (make-instance 'dispatching-handler) :initarg :handler
	    :accessor http-listener-handler)
   (address :initform #(0 0 0 0) :initarg :address
	    :accessor http-listener-address)
   (default-hostname :initarg :default-hostname
		     :accessor http-listener-default-hostname
		     :documentation "Hostname to merge request URLs w/o Host header against")
   (port :initform 80 :initarg :port :accessor http-listener-port)
   (socket :initform nil :accessor http-listener-socket)))

(defstruct http-thread  pid last-hit (quitting nil))

(defclass serve-event-http-listener (http-listener)
  ((serve-event :accessor http-listener-serve-event)))

(defclass threaded-http-listener (http-listener)
  ((master-thread :accessor http-listener-thread)
   (timeout :initarg :timeout :initform 60 ; should be 300, probably
	    :accessor http-listener-timeout)
   (max-spare :initform 15 :initarg :max-spare
	      :accessor http-listener-max-spare)
   (min-spare :initform 5 :initarg :min-spare
	      :accessor http-listener-min-spare)
   (threads :initform nil :accessor http-listener-threads)))
