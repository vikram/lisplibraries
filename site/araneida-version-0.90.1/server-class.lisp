(in-package :araneida)

;;; This is legacy code, kept around for old clients.  new
;;; applications should be using the HTTP-LISTENER api instead

(defclass server ()
  ((base-url :initarg :base-url :accessor server-base-url
             :documentation "The base URL of the web site as the user sees it")
   (port :initarg :port :accessor server-port
         :documentation "The port to actually bind to.  Typically this is 8000 or so when SERVER-BASE-URL refers to port 80")
   (name :initarg :name :accessor server-name
         :documentation "The local address to actually listen to.  Should usually be set to the hostname of the server's base-url")
   (ssl-enabled-p :initarg :ssl-enabled-p  :accessor server-ssl-enabled-p
                  :initform nil
                  :documentation "Use SSL on this server?  Note: we do not handle SSL ourselves; this is only useful for OUTPUT-APACHE-CONF")
   (ssl-certificate :initarg :ssl-certificate :initform nil
                    :documentation "Pathname for SSL certificate file"
                    :accessor server-ssl-certificate )
   (ssl-private-key :initarg :ssl-private-key :initform nil
                    :documentation "Pathname for SSL private key.  May be NIL if ssl-certificate includes it"
                    :accessor server-ssl-private-key)))
   

