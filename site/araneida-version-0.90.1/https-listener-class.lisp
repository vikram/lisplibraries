(in-package :araneida)

;;; we don't _really_ do ssl processing: that's expected to take place in 
;;; the reverse proxy upstream.  However, it's nice to knwo where the
;;; certs and keys and things are; means we can generate apache config
;;; file sections

(defclass https-listener-mixin ()
  ((ssl-certificate :initarg :ssl-certificate :initform nil
                    :documentation "Pathname for SSL certificate file"
                    :accessor https-listener-ssl-certificate )
   (ssl-private-key :initarg :ssl-private-key :initform nil
                    :documentation "Pathname for SSL private key, or NIL if ssl-certificate includes it"
                    :accessor https-listener-ssl-private-key)))

(defclass threaded-https-listener
    (threaded-http-listener https-listener-mixin reverse-proxy-listener-mixin)
  ())

(defclass serve-event-https-listener
    (serve-event-http-listener
     https-listener-mixin reverse-proxy-listener-mixin)
  ())

