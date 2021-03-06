;;;; $Id: armedbear.lisp 295 2007-09-17 19:53:12Z ehuelsmann $
;;;; $URL: svn+ssh://ehuelsmann@common-lisp.net/project/usocket/svn/usocket/tags/0.3.5/backend/armedbear.lisp $

;;;; See LICENSE for licensing information.

(in-package :usocket)


(defmacro jmethod-call (instance (method &rest arg-spec) &rest args)
  (let ((isym (gensym)))
    `(let* ((,isym ,instance)
            (class-name (java:jclass-name (java:jclass-of ,isym))))
       (java:jcall (java:jmethod class-name ,method ,@arg-spec)
              ,isym ,@args))))

(defmacro jnew-call ((class &rest arg-spec) &rest args)
  `(java:jnew (java:jconstructor ,class ,@arg-spec)
         ,@args))

(defun get-host-name ()
  (let ((localAddress (java:jstatic
                       (java:jmethod "java.net.InetAddress"
                                     "getLocalHost")
                       (java:jclass "java.net.InetAddress"))))
    (java:jcall (java:jmethod "java.net.InetAddress" "getHostName")
                localAddress)))

(defun handle-condition (condition &optional socket)
  (typecase condition
    (error (error 'unknown-error :socket socket :real-error condition))))

(defun socket-connect (host port &key (element-type 'character))
  (let ((usock))
    (with-mapped-conditions (usock)
       (let ((sock (ext:make-socket (host-to-hostname host) port)))
         (setf usock
               (make-stream-socket
                :socket sock
                :stream (ext:get-socket-stream sock
                                               :element-type element-type)))))))

(defun socket-listen (host port
                           &key reuseaddress
                           (reuse-address nil reuse-address-supplied-p)
                           (backlog 5)
                           (element-type 'character))
  (let* ((reuseaddress (if reuse-address-supplied-p reuse-address reuseaddress))
         (sock-addr (jnew-call ("java.net.InetSocketAddress"
                                "java.lang.String" "int")
                               (host-to-hostname host) port))
         (sock (jnew-call ("java.net.ServerSocket"))))
    (when reuseaddress
      (with-mapped-conditions ()
         (jmethod-call sock
                       ("setReuseAddress" "boolean")
                       (java:make-immediate-object reuseaddress :boolean))))
    (with-mapped-conditions ()
      (jmethod-call sock
                    ("bind" "java.net.SocketAddress" "int")
                    sock-addr backlog))
    (make-stream-server-socket sock :element-type element-type)))

(defmethod socket-accept ((socket stream-server-usocket) &key element-type)
  (let* ((jsock (socket socket))
         (jacc-sock (with-mapped-conditions (socket)
                       (jmethod-call jsock ("accept"))))
         (jacc-stream
          (ext:get-socket-stream jacc-sock
                                 :element-type (or element-type
                                                   (element-type socket)))))
    (make-stream-socket :socket jacc-sock
                        :stream jacc-stream)))

;;(defun print-java-exception (e)
;;  (let* ((native-exception (java-exception-cause e)))
;;    (print (jcall (jmethod "java.net.BindException" "getMessage") native-exception))))

(defmethod socket-close ((usocket usocket))
  (with-mapped-conditions (usocket)
    (ext:socket-close (socket usocket))))

;; Socket streams are different objects than
;; socket streams. Closing the stream flushes
;; its buffers *and* closes the socket.
(defmethod socket-close ((usocket stream-usocket))
  (with-mapped-conditions (usocket)
    (close (socket-stream usocket))))

(defmethod get-local-address ((usocket usocket))
  (dotted-quad-to-vector-quad (ext:socket-local-address (socket usocket))))

(defmethod get-peer-address ((usocket stream-usocket))
  (dotted-quad-to-vector-quad (ext:socket-peer-address (socket usocket))))

(defmethod get-local-port ((usocket usocket))
  (ext:socket-local-port (socket usocket)))

(defmethod get-peer-port ((usocket stream-usocket))
  (ext:socket-peer-port (socket usocket)))

(defmethod get-local-name ((usocket usocket))
  (values (get-local-address usocket)
          (get-local-port usocket)))

(defmethod get-peer-name ((usocket stream-usocket))
  (values (get-peer-address usocket)
          (get-peer-port usocket)))
