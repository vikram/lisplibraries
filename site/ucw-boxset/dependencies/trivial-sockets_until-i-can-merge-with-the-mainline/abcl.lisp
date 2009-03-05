
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
  (unless (and (eql local-host :any) (eql local-port 0))
    (error 'unsupported :feature :bind))
  (unless (eql protocol :tcp)
    (error 'unsupported :feature `(:protocol ,protocol)))
  (unless (eql external-format :default)
    (error 'unsupported :feature :external-format))
  (handler-bind ((error (lambda (c) (error 'socket-error :nested-error c))))
                (ext:get-socket-stream
                 (ext:make-socket (resolve-hostname peer-host) peer-port)
                 :element-type element-type)))


(defun open-server (&key (host :any) (port 0)
                         (reuse-address t)
                         (backlog 50)
                         (protocol :tcp))
  "Returns a SERVER object and the port that was bound, as multiple values"
  (unless (eql protocol :tcp)
    (error 'unsupported :feature `(:protocol ,protocol)))
  (unless (equal (resolve-hostname host) "0.0.0.0")
    (error 'unsupported :feature :bind))
  (unless (= backlog 50)
    ;; the default, as of jdk 1.4.2
    (error 'unsupported :feature :backlog))
  (handler-bind ((error (lambda (c) (error 'socket-error :nested-error c))))
    (let ((sock (ext:make-server-socket port)))
      (java:jcall (java:jmethod "java.net.ServerSocket" "setReuseAddress" "boolean")
		  sock
		  (java:make-immediate-object reuse-address :boolean))
      (values sock
	      (java:jcall (java:jmethod "java.net.ServerSocket" "getLocalPort")
			  sock)))))

(defun close-server (server)
  (ext:server-socket-close server))

(defun accept-connection (socket
                          &key
                          (external-format :default)
                          (element-type 'character))
  (unless (eql external-format :default)
    (error 'unsupported :feature :external-format))
  (handler-bind ((error (lambda (c) (error 'socket-error :nested-error c))))
    (ext:get-socket-stream (ext:socket-accept socket)
			   :element-type element-type)))

