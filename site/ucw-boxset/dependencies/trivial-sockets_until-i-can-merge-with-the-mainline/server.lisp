(in-package :trivial-sockets)

(defmacro with-server ((name arguments) &body forms)
  `(let (,name)
     (unwind-protect 
	 (progn
	   (setf ,name (open-server ,@arguments))
	   (locally
               ,@forms))
       (when ,name (close-server ,name)))))
