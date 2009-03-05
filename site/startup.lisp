;; startup.lisp, version 1.0

;; ports that will be listened on (localhost):
;;
;;   8080 - Hunchentoot (via mod_lisp2)
;;   4005 - Swank (via ssh+Emacs)
;;   6440 - startup.lisp (via telnet)

(require 'asdf)
(require 'sb-bsd-sockets)
(require 'cl+ssl)

(defvar *swank-loaded* nil)

(if (and (probe-file #p"~/.sbcl/site/slime/swank-loader.lisp")
	 (load #p"~/.sbcl/slime/swank-loader"))
    (setf *swank-loaded* t))

(push :hunchentoot-no-ssl *features*)
(asdf:operate 'asdf:load-op :hunchentoot)
(asdf:operate 'asdf:load-op :cl-who)

(defun start-server ()
  (let (hunchentoot-server swank-server)

    ;; Start a Hunchentoot server listening for connections
    (setf hunchentoot-server
	  (hunchentoot:start-server :port 8080 :mod-lisp-p t))
    (princ "Hunchentoot server started on port 8080") (terpri)

    ;; Start a Swank server
    (when *swank-loaded*
      (setf swank-server
	    (swank:create-server :port 4005 :style :spawn :dont-close t))
      (princ "Swank server started on port 4005") (terpri))

    (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
				 :type :stream :protocol :tcp)))

      ;; Listen on local port 6440 for a TCP connection
      (sb-bsd-sockets:socket-bind socket #(127 0 0 1) 6440)
      (sb-bsd-sockets:socket-listen socket 1)
      (princ "Shutdown listener started on port 6440") (terpri)

      ;; When it comes, just tell the caller we're dumping core
      (multiple-value-bind (client-socket addr port)
	  (sb-bsd-sockets:socket-accept socket)
	(let ((stream
	       (sb-bsd-sockets:socket-make-stream client-socket
						  :element-type 'character
						  :input t
						  :output t
						  :buffering :none)))
	  (princ "Saving core and shutting down..." stream)
	  (terpri stream))

	;; Close up the sockets
	(sb-bsd-sockets:socket-close client-socket)
	(sb-bsd-sockets:socket-close socket)))

    ;; Shut down Hunchentoot
    (hunchentoot:stop-server hunchentoot-server)

    ;; Shut down Swank and anyone else by terminating all threads
    (dolist (thread (sb-thread:list-all-threads))
      (unless (equal sb-thread:*current-thread* thread)
	(sb-thread:terminate-thread thread)))
    (sleep 1)

    ;; Dump core and exit
    (sb-ext:save-lisp-and-die #p"/var/lib/hunchentoot/sbcl.core"
			      :toplevel #'start-server)))

;(start-server)

;; startup.lisp ends here
