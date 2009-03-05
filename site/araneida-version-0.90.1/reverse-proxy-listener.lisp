(in-package :araneida)

(defclass reverse-proxy-listener-mixin ()
  ((translations :initarg :translations
		 :accessor reverse-proxy-translations)))

(defun string-prefix-p (shorter longer)
  (let ((n (mismatch shorter longer))
	(l (length shorter)))
    (if (or (not n) (= n l)) l nil)))

(defun query-start-position (string)
  (let* ((first-slash (position #\/ string))
         (second-slash (if first-slash (position #\/ string :start (1+ first-slash))))
         (third-slash (if second-slash (position #\/ string :start (1+ second-slash)))))
    (or third-slash (length string))))
		   
(defmethod handle-request-using-listener
    :around ((l reverse-proxy-listener-mixin) handler request)
  (let* ((r (copy-request request))
	 (u (request-urlstring r))
	 (new
	  (loop for (from to) in (reverse-proxy-translations l)
                for pos =
               (if (and (consp to)
                        (eq (car to) :wild-host))
                   (string-prefix-p
                    (concatenate 'string (subseq u 0 (query-start-position u))
                                 (second to))
                    u)
                   (string-prefix-p to u))
		when pos
		return (concatenate 'string from (subseq u pos)))))
    (when new
      (setf (request-urlstring r) new
	    (request-url r) (parse-urlstring new)))
    (call-next-method l handler r)))

(defclass threaded-reverse-proxy-listener
    (threaded-http-listener reverse-proxy-listener-mixin)
  ())

(defclass serve-event-reverse-proxy-listener
    (serve-event-http-listener reverse-proxy-listener-mixin)
  ())

(defgeneric apache-conf-segment (reverse-proxy-listener output-stream)
  (:documentation "Write out a workable proxy configuration for Apache to output-stream. This could be used as an example to start from, or as the real deal. Will append SSL information if using an HTTPS listener"))
(defmethod apache-conf-segment ((l reverse-proxy-listener-mixin) stream)
  (dolist (trans (reverse-proxy-translations l))
    (destructuring-bind (from to) trans
      (let ((fu (parse-urlstring from)))
	(format stream 
		"<VirtualHost ~A:~A>
ServerName ~A:~A
ProxyPass / ~A
ProxyPassReverse / ~A~%"
		(url-host fu) (url-port fu) 
		(url-host fu) (url-port fu) 
		to to)
	(when (typep l 'https-listener-mixin)
	  (format stream "SSLEngine on~%SSLCertificateFile ~A~%NoCache *~%" 
		  (namestring (https-listener-ssl-certificate l)))
	  (let ((priv (https-listener-ssl-private-key l)))
	    (when priv
	      (format stream "SSLCertificateKeyFile ~A~%"
		      (namestring priv)))))
	(format stream "SetEnvIf User-Agent \".*MSIE.*\" nokeepalive ssl-unclean-shutdown~%</VirtualHost>~%")))))

