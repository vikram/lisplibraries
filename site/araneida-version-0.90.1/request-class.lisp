(in-package :araneida)

(defclass request ()
  ((url :initarg :url :accessor request-url
        :documentation "The URL requested.")
   (http-version :initarg :http-version :reader request-http-version)
   (urlstring :initarg :urlstring :accessor request-urlstring)
   (handled-by :initform nil :accessor request-handled-by)
   (user :initarg :user :accessor request-user
         :documentation "The user associated with the request, filled in by the authentication handler")
   (method :initarg :method :reader request-method
           :documentation "The request method, as a keyword")
   #+nil				;unused
   (socket :initarg :socket :reader request-socket
           :documentation "This socket connects to the client browser")
   (stream :initarg :stream :accessor request-stream
           :documentation "This stream is connected to the peer")
   (headers :initarg :headers :reader request-headers
            :documentation "Internal use only.")
   (body :initarg :body :reader request-body
         :documentation "Request body")
   (unparsed-body :initform "" :initarg :unparsed-body :reader request-unparsed-body
		  :documentation "Unparsed request body")
   (base-url :initarg :base-url :accessor request-base-url
             :documentation "The URL prefix that requests of this class know how to handle.")
   (condition :initarg :condition :accessor request-condition
              :documentation "If a previous handler raised a condition, it is stored here for the :error hander"))
  
  (:documentation "This is instantiated when a client makes a request to the http server"))

