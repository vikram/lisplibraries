(defpackage "MY-ARANEIDA-EXAMPLE"
   (:use "CL" "ARANEIDA"))
(in-package :my-araneida-example)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; This is for using Araneida behind a reverse-proxying 
  ;; Apache (or other HTTP proxy) server.  You may need to alter the 
  ;; configuration for your local setup
  #+nil (pushnew :araneida-uses-proxy *features*)
  ;; if you have a threaded SBCL, and want to, you can use the (new,
  ;; whizzy) thread-based server instead of the (older, better tested)
  ;; SERVE-EVENT thing
  #+araneida-threads (pushnew :araneida-uses-threads *features*))

#+sbcl
(defun my-fqdn ()
  (sb-bsd-sockets:host-ent-name 
   (sb-bsd-sockets:get-host-by-name (machine-instance))))

(defvar *demo-url*
  ;; This is the public URL of the site.  We decide fairly arbitrarily
  ;; that if we have a reverse proxy, it can bind to port 80.
  ;; Otherwise, you probably don't want to run araneida as root, so
  ;; leave this > 1024 unless you have some spiffy way of letting
  ;; non-root apps get hold of privileged ports.
  (make-url :scheme "http" :host "localhost" ;(my-fqdn)
	    :port 
	    #+araneida-uses-proxy 80
	    #-araneida-uses-proxy 8000))

#-araneida-uses-proxy
(defvar *listener*
  (make-instance #+araneida-uses-threads 'threaded-http-listener
		 #-araneida-uses-threads 'serve-event-http-listener
		 :port (url-port *demo-url*)))

#+araneida-uses-proxy
(defvar *listener*
  (let ((fwd-url (copy-url *demo-url*)))
    (setf (url-port fwd-url) (+ 1024 (url-port *demo-url*)))
    (make-instance #+araneida-uses-threads 'threaded-reverse-proxy-listener
		   #-araneida-uses-threads 'serve-event-reverse-proxy-listener
		   :translations
		   `((,(urlstring *demo-url*) ,(urlstring fwd-url)))
		   :address #(0 0 0 0)
		   :port (url-port fwd-url))))

(defclass hello-handler (handler)
  ((hits :initform 0 :accessor hello-hits)))
(defclass reset-handler (handler)
  ((hello :initarg :hello :initform (error "missing required argument")
	  :reader reset-hello)))


(defmethod handle-request-response ((handler hello-handler) method request)
  (let ((hits (incf (hello-hits handler))))
    (when (zerop (mod hits 7))
      (signal 'http-payment-required
              :message "Game Over.  Insert coin"))
    (request-send-headers request)
    (html-stream 
     (request-stream request)
     `(html (head (title "Hello world"))
	    (body (h1 "Hello")
		  ,@(when (zerop (mod hits 4)) (list (/ hits 0)))
		  (p "There have been " ,hits 
		     " accesses to this page since the last "
		     ((a :href "reset") "reset")))))))

(defmethod handle-request-response ((handler reset-handler) method request)
  (let ((hello (reset-hello handler)))
    (setf (hello-hits hello) 0)
    (request-redirect request "hello")
    t))

(let ((hello (make-instance 'hello-handler)))
  (install-handler (http-listener-handler *listener*)
		   hello 
		   (urlstring (merge-url *demo-url* "/hello"))  t)
  (install-handler (http-listener-handler *listener*)
		   (make-instance 'reset-handler :hello hello)
		   (urlstring (merge-url *demo-url* "/reset")) t))
		 
;;; if we're going to serve static files, we should configure the known
;;; MIME types to something useful
;; (setf *content-types* (read-mime-types "/etc/mime.types"))

;;; point the root url at a static view of the Araneida sources
(let* ((pn (asdf:component-pathname (asdf::find-system 'araneida))))
  (install-handler (http-listener-handler *listener*)
		   (make-instance 'static-file-handler :pathname pn)
		   (urlstring *demo-url*) nil))

#+araneida-uses-proxy
(with-open-file (o "/tmp/araneida-apache.cf" :direction :output)
  (apache-conf-segment *listener* o))

;;; ready to rock? 
#|
(start-listening *listener*)

#+clisp (host-serve-events) ; starts listening under clisp
|#

