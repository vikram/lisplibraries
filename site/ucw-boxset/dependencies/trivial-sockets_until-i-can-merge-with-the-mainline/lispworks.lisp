(in-package :trivial-sockets)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "comm"))

(defun resolve-hostname (name)
  (cond
   ((eql name :any) "0.0.0.0")
   ((typep name '(vector * 4)) (format nil "~{~A~^.~}" (coerce name 'list)))
   (t name)))

(defun open-stream (peer-host peer-port 
			      &key (local-host :any) (local-port 0)
			      (external-format :default)
			      (element-type 'base-char)
			      (protocol :tcp))
  (unless (eql protocol :tcp)
    (error 'unsupported :feature `(:protocol ,protocol)))
  (unless (eql external-format :default)
    (error 'unsupported :feature `(:external-format ,external-format)))
  (unless (eql local-host :any)
    (error 'unsupported :feature `(:local-host ,local-host)))
  (unless (eql local-port 0)
    (error 'unsupported :feature `(:local-port ,local-port)))
  (handler-bind ((error (lambda (c) (error 'socket-error :nested-error c))))
    (comm:open-tcp-stream (resolve-hostname peer-host)
                          peer-port
                          :element-type element-type
                          :errorp t)))
		
;; there is no (published) way to make a server socket in lispworks
;; this server implementation is a hack around the otherwise elegant
;; lispworks #'comm:start-up-server functionality

(defun make-queue ()
  (cons nil nil))

(defun queue-empty-p (queue)
  (null (car queue)))

(defun enqueue (x queue)
  (if (null (car queue))
      (setf (cdr queue) (setf (car queue) (list x)))
    (setf (cdr (cdr queue)) (list x)
          (cdr queue) (cdr (cdr queue))))
  (car queue))

(defun dequeue (queue)
  (pop (car queue)))

(defclass server ()
  ((process :reader get-process)
   (lock :initform (mp:make-lock))
   (clients :initform (make-queue))))

(defun open-server (&key (host :any) (port 0)
                         (reuse-address t)
                         (backlog 5)
                         (protocol :tcp))
  "Returns a SERVER object and the port that was bound, as multiple values"
  (unless (eql host :any) 
    ;; not in the manual, appears in arglist, maybe not on all platforms
    (warn "Ignoring HOST ~S, using :any instead." host)
    (setf host :any))
  (unless (eql protocol :tcp)
    (error 'unsupported :feature `(:protocol ,protocol)))
  (unless (eql backlog 5)
    ;; not in the manual, appears in arglist, maybe not on all platforms
    (error 'unsupported :feature `(:backlog ,backlog)))
  (let ((server (make-instance 'server)))
    (with-slots (process lock clients) 
        server
      (multiple-value-bind (new-process condition)
          ;; we enqueue all incoming connections until #'accept-connection retrieves them
	  (let ((comm::*use_so_reuseaddr* reuse-address))
	    (comm:start-up-server :function #'(lambda (socket)
						(mp:with-lock (lock)
							      (enqueue socket clients)))
				  :service port
				  :wait t))
        (when condition
          (error 'socket-error :nested-error condition))
        (setf process new-process)))
    (values server port))) ;; we do not return the actual port when port was 0

(defun close-server (server)
  (with-slots (process)
      server
    (mp:process-kill process)
    (setf process nil)))

(defun accept-connection (server
                          &key
                          (external-format :default)
                          (element-type 'base-char))
  (unless (eql external-format :default)
    (error 'unsupported :feature `(:external-format, external-format)))
  (let (client-socket)
    (with-slots (process lock clients)
        server
      (unless process 
        (error 'socket-error :nested-error (make-instance 'simple-error :format-string "Server closed"))) 
      (loop 
       (mp:with-lock (lock)
         (unless (queue-empty-p clients)
           (setf client-socket (dequeue clients))
           (return)))
       (mp:process-wait "Waiting for incoming connections" 
                        #'(lambda (server)
                            (with-slots (lock clients)
                                server
                              (mp:with-lock (lock)
                                (not (queue-empty-p clients)))))
                        server)))
    (make-instance 'comm:socket-stream
                   :socket client-socket
                   :direction :io
                   :element-type element-type)))
