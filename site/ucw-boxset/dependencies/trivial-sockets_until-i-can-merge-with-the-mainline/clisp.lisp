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
  ;; FIXME I wish there were a smarter way to detect only the errors 
  ;; we're interested in, but CLISP impnotes don't say what to look for
  (handler-bind ((error (lambda (c) (error 'socket-error :nested-error c))))
    (socket:socket-connect peer-port (resolve-hostname peer-host)
			   :element-type element-type
			   :external-format external-format
			   :buffered nil
			   )))


(defun open-server (&key (host :any) (port 0)
			 (reuse-address t)
			 (backlog 1)
			 (protocol :tcp))
  (unless (eql protocol :tcp)
    (error 'unsupported :feature `(:protocol ,protocol)))
  (unless (equal (resolve-hostname host) "0.0.0.0")
    (warn "Ignoring HOST value ~S, using :any instead." host)
    (setf host :any))
  (unless (= backlog 1)
    ;; we established that the default backlog is 1 by stracing clisp
    ;; 2.33.2 (2004-06-02) (built 3304881526)
    (error 'unsupported :feature :backlog))
  (unless reuse-address
    (error 'unsupported :feature :nil-reuse-address))
  (handler-bind ((error (lambda (c) (error 'socket-error :nested-error c))))
    (let ((s (socket:socket-server port)))
      (values s (socket:socket-server-port s)))))
    
(defun close-server (server)
  (socket:socket-server-close server))

(defun accept-connection (socket
			  &key
			  (external-format :default)
			  (element-type 'character))
  (handler-bind ((error (lambda (c) (error 'socket-error :nested-error c))))
    (socket:socket-accept socket :external-format external-format
			  :element-type element-type
			  :buffered nil)))
