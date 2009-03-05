(in-package :araneida)

(defvar *sockets-for-fds* (make-hash-table :test 'eql))

(defun listen-to-inet-port (&key (port 0) (kind :stream) (reuse nil))
  "Create, bind and listen to an inet socket on *:PORT.
setsockopt SO_REUSEADDR if :reuse is not nil"
  (let ((socket (sockets:make-inet-socket
                 :stream
                 (sockets:get-protocol-by-name "tcp"))))
    (if reuse
        (setf (sockets:sockopt-reuse-address socket) t))
    (sockets:socket-bind socket (sockets:make-inet-address "0.0.0.0") port)
    (sockets:socket-listen socket 15)
    (let ((fd (sockets:socket-file-descriptor socket)))
      (setf (gethash fd *sockets-for-fds*) socket)
      fd)))
          

(defun accept-tcp-connection (&rest args)
  (let* ((s (sockets:socket-accept (gethash (car args) *sockets-for-fds*)))
         (fd (sockets:socket-file-descriptor s)))
    (setf (gethash fd *sockets-for-fds*) s)
    fd))
    
(defun close-socket (fd)
  (sockets:socket-close (gethash fd *sockets-for-fds*))
  (remhash fd *sockets-for-fds*))

;; XXX do we have the right stream element type
(defun make-stream-from-fd (fd &key (input t) (output t) (buffering :none))
  (sockets:socket-make-stream
   (gethash fd *sockets-for-fds*)
   :input input :output output :buffering buffering))


