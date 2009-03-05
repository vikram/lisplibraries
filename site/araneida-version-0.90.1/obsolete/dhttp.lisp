;;; Cruft compatibility layer for dhttp-using code to take advantage
;;; of new features in http-server instead.

;;; Continues to exist solely for my benefit as  nobody else ever uses dhttp

(defpackage "DHTTP"
  (:use "LISP")
  (:shadowing-import-from HTTPSRV
                          REQUEST  similar, not quite the same
                          ; identical
                          universal-time-to-rfc-date 
                          request-path-info request-stream
                          request-socket request-body
                          request-method request-header
                          request-session html
                          session-request
                          body-param
                          ))

(in-package :dhttp)


(defvar *all-servers* (make-hash-table :test #'equal))

(defun export-url (host urlstring &rest args)
  (let* ((server (or (gethash host *all-servers*)
                     (setf (gethash host *all-servers*) (make-server host))))
         (url (httpsrv:merge-url (httpsrv:server-base-url server) urlstring)))
    (apply #'httpsrv:export-handler url args)))

(defun export-host (hostname)
  (let ((server (or (gethash host *all-servers*)
                    (setf (gethash host *all-servers*) (make-server
                                                        host)))))
    (httpsrv:export-server server)))
  
(defun start-server (port-number &optional idle)
  (httpsrv:server-start port-number :idle idle))

(defun escape-url (&rest args) (apply #'httpsrv:urlstring-escape args))
(defun redirect (&rest args) (apply #'httpsrv:request-redirect args))
(defun file-request (&rest args) (apply #'httpsrv:file-request-handler args))
(defun send-headers (&rest args) (apply #'httpsrv:request-send-headers args))

(defun request-query-string (request)
  (httpsrv:url-query (request-url request)))

;;; httpsrv has url-query-param, but you can't get here from there
(defun query-param (name query-string)
  "Assuming QUERY-STRING is made of name=value pairs separated by #\; or #\& , 
find the value of the NAME parameter.  Returns nil if not present"
  (let ((pairs (mapcar (lambda (x) (split x 2 '(#\=) ))
                       (split query-string nil '(#\& #\;)))))
    (cadr (assoc name pairs :test #'string=))))

(defun url-no-query ((url htplike-url))
  (let ((u (httpsrv:copy-url url)))    
    (setf (httpsrv:url-query u) nil
          (httpsrv:url-fragment u) nil)
    u))

(defun request-original-minus-query-string (request)
  (urlstring (url-no-query (request-original-url request))))
(defun request-minus-query-string (request)
  (urlstring (url-no-query (request-url request))))

(defun update-query-param (name value query-string)
  "Return a new query string based on QUERY-STRING but with the additional or updated parameter NAME=VALUE"
  (let ((pairs (mapcar (lambda (x) (split x 2 '(#\=) ))
                       (split query-string nil '(#\& #\;)))))
    (aif (assoc name pairs :test #'string=)
         (rplacd it (list value))
         (setf pairs (acons name (list value) pairs)))
    (join "&" (mapcar (lambda (x) (s. (car x)  "=" (cadr x))) pairs))))


(defun request-url (request) (urlstring (httpsrv:request-url request)))
(defun request-original-url (request)
  (httpsrv:urlstring (httpsrv:request-original-url request)))
(defun request-base-url (request)
  (urlstring (httpsrv:request-base-url request)))


#|

 these were never used externally, as far as I know 

parse-body
ip-authenticate
basic-authenticate
make-auth-realm
realm-allows-credentials-p
realm-name
send-file

 these can easily be unused

session-request
output-apache-conf
start-debugging-server
|#
