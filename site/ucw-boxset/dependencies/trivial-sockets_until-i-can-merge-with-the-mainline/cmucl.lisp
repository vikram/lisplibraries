(in-package :trivial-sockets)

(defun resolve-hostname (name)
  (cond
   ((eql name :any) "0.0.0.0")
   ((typep name '(vector * 4)) (format nil "~{~A~^.~}" (coerce name 'list)))
   (t name)))

(defun pretty-stream-name (host port)
  (format nil "~A:~A" (resolve-hostname host) port))

(defun open-stream (peer-host peer-port 
			      &key (local-host :any) (local-port 0)
			      (external-format :default)
			      (element-type 'character)
			      (protocol :tcp))
  (unless (eql external-format :default)
    (error 'unsupported :feature :external-format))
  (unless (and (eql local-host :any) (eql local-port 0))
    (error 'unsupported :feature :bind))
  (unless (eql protocol :tcp)
    (error 'unsupported :feature `(:protocol ,protocol)))
  ;; connect-to-inet-socket signals simple-erors.  not great
  (handler-bind ((error (lambda (c) (error 'socket-error :nested-error c))))
    (let ((s (ext:connect-to-inet-socket
              (resolve-hostname peer-host) peer-port)))
      (sys:make-fd-stream s :input t :output t :element-type element-type
			  :buffering :full
                          :name (pretty-stream-name peer-host peer-port)))))

(defun open-server (&key (host :any) (port 0)
                    (reuse-address t)
                    (backlog 1)
                    (protocol :tcp))
  "Returns a SERVER object and the port that was bound, as multiple values"
  (unless (eql protocol :tcp)
    (error 'unsupported :feature `(:protocol ,protocol)))
  (handler-bind ((error (lambda (c) (error 'socket-error :nested-error c))))
    (let ((socket (if (equal (resolve-hostname host) "0.0.0.0")
                      ;; create-inet-listener barfs on `:host nil'
                      (ext:create-inet-listener port :stream
                                                :reuse-address reuse-address
                                                :backlog backlog)
                      (ext:create-inet-listener port :stream
                                                :reuse-address reuse-address
                                                :backlog backlog
                                                :host host))))
      (multiple-value-bind (host port)
          (ext:get-socket-host-and-port socket)
        (declare (ignore host))
        (values socket port)))))
			      
(defun close-server (server)
  (unix:unix-close server))

(defun accept-connection (socket
			  &key
			  (external-format :default)
			  (element-type 'character))
  (unless (eql external-format :default)
    (error 'unsupported :feature :external-format))
  (handler-bind ((error (lambda (c) (error 'socket-error :nested-error c))))
    (let ((fd (ext:accept-tcp-connection socket)))
    (multiple-value-bind (peer-host peer-port)
        (ext:get-peer-host-and-port fd)
      (sys:make-fd-stream fd
                          :input t :output t
                          :element-type element-type
                          :auto-close t
                          :buffering :full
                          :name (pretty-stream-name peer-host peer-port))))))

