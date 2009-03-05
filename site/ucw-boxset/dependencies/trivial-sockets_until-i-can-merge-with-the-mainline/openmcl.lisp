(in-package :trivial-sockets)

(defun resolve-hostname (name)
  (cond
   ((eql name :any) "0.0.0.0")
   ((typep name '(vector * 4)) (format nil "~{~A~^.~}" (coerce name 'list)))
   (t name)))

(defun open-stream (peer-host peer-port 
			      &key (local-host :any) (local-port 0)
			      (external-format :default)
			      (element-type 'character)
			      (protocol :tcp))
  (declare (ignore element-type))
  (unless (eql protocol :tcp)
    (error 'unsupported :feature `(:protocol ,protocol)))
  (unless (eql external-format :default)
    (error 'unsupported :feature :external-format))
  (handler-bind ((ccl::socket-creation-error
                  (lambda (c) (error 'socket-error :nested-error c))))
    (ccl:make-socket :address-family :internet
		     :connect :active
		     :type :stream
		     :remote-host (resolve-hostname peer-host)
		     :remote-port peer-port
		     :local-host (resolve-hostname local-host)
		     :local-port local-port)))
			      
(defun open-server (&key (host :any) (port 0)
			 (reuse-address t)
			 (backlog 1)
			 (protocol :tcp))
  "Returns a SERVER object and the port that was bound, as multiple values"
  (unless (eql protocol :tcp)
    (error 'unsupported :feature `(:protocol ,protocol)))
  (handler-bind ((ccl::socket-creation-error
                  (lambda (c) (error 'socket-error :nested-error c))))
    (let* ((host (if (eql host :any) nil host))
           (socket (ccl:make-socket :address-family :internet
                                    :type :stream
                                    :connect :passive
                                    :local-host host
                                    :local-port port
                                    :reuse-address reuse-address
                                    :backlog backlog)))
      (values socket (ccl:local-port socket)))))

(defun close-server (server)
  (close server))

(defun accept-connection (socket
			  &key
			  (external-format :default)
			  (element-type 'character))
  (declare (ignore element-type))      ; openmcl streams are bivalent.
  (unless (eql external-format :default)
    (error 'unsupported :feature :external-format))
  (handler-bind ((ccl:socket-error
                  (lambda (c) (error 'socket-error :nested-error c))))
    (ccl:accept-connection socket :wait t)))