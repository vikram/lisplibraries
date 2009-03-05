(in-package :araneida)

;;; this file is no longer loaded

(defun output-apache-conf-segment (server &optional (stream t))
  "Output on STREAM an httpd.conf VirtualHost section that describes SERVER"
  (let* ((u (server-base-url server))
         (real-u (make-instance 'http-url :host (url-host u)
                                :port (server-port server))))
    (format stream 
            "<VirtualHost ~A:~A>
ServerName ~A
ProxyPass / ~A
ProxyPassReverse / ~A~%"
            (url-host u) (url-port u) (url-host u)
            (urlstring real-u) (urlstring real-u))
    (when (server-ssl-enabled-p server)
      (format stream "SSLEngine on~%SSLCertificateFile ~A~%NoCache *~%" 
              (namestring (server-ssl-certificate server)))
      (aif (server-ssl-private-key server)
           (format stream "SSLCertificateKeyFile ~A~%"
                   (namestring it))))
    (format stream "SetEnvIf User-Agent \".*MSIE.*\" nokeepalive ssl-unclean-shutdown~%</VirtualHost>~%")))

#|
(output-apache-conf-segment
 (make-instance 'server
                :base-url (parse-urlstring "https://www.stargreen.com/")
                :port "8000"
                :ssl-enabled-p nil))
|#

(defmethod server-equal-p ((server1 t) (server2 t)) nil)
(defmethod server-equal-p ((server1 server) (server2 server))
  (and (eql (class-of server1) (class-of server2))
       (let ((class (class-of server1)))
         (loop for slot in '(base-url port name ssl-enabled-p ssl-certificate
				      ssl-private-key)
               for name = slot
               always
               (equal (slot-value server1 name) (slot-value server2 name))))))



(defmethod print-object ((s server) stream)
  (print-unreadable-object (s stream :type t :identity t)
                           (format stream "~A port ~A"
                                   (urlstring (server-base-url s))
                                   (server-port s) )))

