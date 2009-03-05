;;;
;;; This file contains property of Mu Aps.
;;; Copyright (c) 2005.  All rights reserved.
;;;

					#| 

NOTE: Since sometimes all the regression tests are executed in rapid
succession, and since muprocs must have unique names, care should be
taken that the muprucs submitted in the regression tests have unique
names, to avoid inter-test interference.

|#

(in-package :muproc-test)

(defun log-stream ()
  *trace-output*)

(deftest simple-1 
    ;; Verify that we can send messages from one muproc to another, and back.
    (labels ((receiver-1 (mbox)
	       (muproc-with-timeout (1 (%enqueue% mbox :receiver-timed-out))
		 (loop
		  (mumsg-receive (from)
		    ((request) t
		     (mumsg-send from :reply (1+ request)))))))
	     (sender-1 (err-stream mbox)
	       (declare (special *muproc-errorstream*))
	       (muproc-with-timeout (1 (%enqueue% mbox :sender-timed-out))
		 (let ((newproc (muproc-spawn 'receiver-1
					       #'receiver-1
					       (list mbox)
					       :errorstream err-stream))
		       (result))
		   (loop 
		    for count from 1 to 10
		    do (progn
			 (mumsg-send newproc :request count)
			 (mumsg-receive (from)
			   ((reply) t
			    (if (and (integerp reply) (= reply (1+ count)))
				(push reply result)
				(%enqueue% mbox (cons :bad-reply reply)))))))
		   (%enqueue% mbox (nreverse result))))))
      (let ((mbox (%make-queue%)))
	(muproc-spawn 'sender-1
		       #'sender-1
		       (list (log-stream) mbox)
		       :errorstream (log-stream))
	(muproc-with-timeout (1 :timed-out)
	  (%dequeue% mbox))))
  (2 3 4 5 6 7 8 9 10 11))

(deftest simple-2
    ;; Verify that we can skip mumsgs whose fields do not match receive spec.
    (labels ((receiver-1 (mbox)
	       (muproc-with-timeout (1 (%enqueue% mbox :receiver-timed-out))
		 (loop
		  (mumsg-receive (from)
		    ((request) t
		     (mumsg-send from :reply (1+ request)))))))
	     (sender-1 (err-stream mbox)
	       (declare (special *muproc-errorstream*))
	       (muproc-with-timeout (1 (%enqueue% mbox :sender-timed-out))
		 (let ((newproc (muproc-spawn 'receiver-2
					       #'receiver-1
					       (list mbox)
					       :errorstream err-stream))
		       (result))
		   ;; These messages ought to be skipped
		   (loop for count from 1 to 5
			 do (mumsg-send newproc :skip-this count))
		   (loop 
		    for count from 1 to 10
		    do (progn
			 (mumsg-send newproc :request count)
			 (mumsg-receive (from)
			   ((reply) t
			    (if (and (integerp reply) (= reply (1+ count)))
				(push reply result)
				(%enqueue% mbox (cons :bad-reply reply)))))))
		   (%enqueue% mbox (nreverse result))))))
      (let ((mbox (%make-queue%)))
	(muproc-spawn 'sender-2
		       #'sender-1
		       (list (log-stream) mbox)
		       :errorstream (log-stream))
	(muproc-with-timeout (1 :timed-out)
	  (%dequeue% mbox))))
  (2 3 4 5 6 7 8 9 10 11))

(deftest simple-3
    ;; Verify that we can perform actions based on guards
    (labels ((receiver-1 (mbox)
	       (muproc-with-timeout (1 (%enqueue% mbox :receiver-timed-out))
		 (loop
		  (mumsg-receive (from)
		    ((request) (evenp request)
		     (mumsg-send from :reply (* request 10)))
		    ((request) (oddp request)
		     (mumsg-send from :reply (* request -10)))))))
	     (sender-1 (err-stream mbox)
	       (declare (special *muproc-errorstream*))
	       (muproc-with-timeout (1 (%enqueue% mbox :sender-timed-out))
		 (let ((newproc (muproc-spawn 'receiver-3
					       #'receiver-1
					       (list mbox)
					       :errorstream err-stream))
		       (result))
		   (loop 
		    for count from 1 to 10
		    do (progn
			 (mumsg-send newproc :request count)
			 (mumsg-receive (from)
			   ((reply) t
			    (push reply result)))))
		   (%enqueue% mbox (nreverse result))))))
      (let ((mbox (%make-queue%)))
	(muproc-spawn 'sender-3
		       #'sender-1
		       (list (log-stream) mbox)
		       :errorstream (log-stream))
	(muproc-with-timeout (2 :timed-out)
	  (%dequeue% mbox))))
  (-10 20 -30 40 -50 60 -70 80 -90 100))

(deftest simple-4
    ;; Verify that we can skip packets based on guards
    (labels ((receiver-1 (mbox)
	       (muproc-with-timeout (2 (%enqueue% mbox :receiver-timed-out))
		 (loop
		  (mumsg-receive (from)
		    ((request) (evenp request)
		     (mumsg-send from :reply (* request 10)))))))
	     (sender-1 (err-stream mbox)
	       (declare (special *muproc-errorstream*))
	       (muproc-with-timeout (2 (%enqueue% mbox :sender-timed-out))
		 (let ((newproc (muproc-spawn 'receiver-4
					       #'receiver-1
					       (list mbox)
					       :errorstream err-stream))
		       (result))
		   (loop ;; send numbers 1-10
		    for count from 1 to 10
		    do (mumsg-send newproc :request count))
		   (loop ;; receive number 2,4,6,8,10
		    for count from 1 to 5
		    do (mumsg-receive (from)
			 ((reply) t
			  (push reply result))))
		   (%enqueue% mbox (nreverse result))))))
      (let ((mbox (%make-queue%)))
	(muproc-spawn 'sender-4
		       #'sender-1
		       (list (log-stream) mbox)
		       :errorstream (log-stream))
	(muproc-with-timeout (3 :timed-out)
	  (%dequeue% mbox))))
  (20 40 60 80 100))

(deftest simple-5
    ;; Verify that we can have many pending packets
    (labels ((receiver-1 (mbox)
	       (muproc-with-timeout (2 (%enqueue% mbox :receiver-timed-out))
		 (loop
		  (mumsg-receive (from)
		    ((request) (evenp request)
		     (mumsg-send from :reply (* request 10)))))))
	     (sender-1 (err-stream mbox)
	       (declare (special *muproc-errorstream*))
	       (muproc-with-timeout (2 (%enqueue% mbox :sender-timed-out))
		 (let ((newproc (muproc-spawn 'receiver-5
					       #'receiver-1
					       (list mbox)
					       :errorstream err-stream))
		       (result))
		   (loop ;; send lots of packets
		    for count from 1 to 100
		    do (mumsg-send newproc :skip-this count))
		   ;; send the expected packet
		   (mumsg-send newproc :request 42)
		   ;; get the reply
		   (mumsg-receive (from)
		     ((reply) t
		      (push reply result)))
		   (%enqueue% mbox (nreverse result))))))
      (let ((mbox (%make-queue%)))
	(muproc-spawn 'sender-5
		       #'sender-1
		       (list (log-stream) mbox)
		       :errorstream (log-stream))
	(muproc-with-timeout (3 :timed-out)
	  (%dequeue% mbox))))
  (420))

(deftest muproc-exit-link-1
    ;; Verify that we get a message on #'muproc-exit when trapping exits
    (labels ((receiver-1 (mbox)
	       (muproc-exit :rcv-1-exit)) ; just return
	     (sender-1 (err-stream mbox)
	       (muproc-with-timeout (2 (%enqueue% mbox :sender-timed-out))
		 (muproc-set-trap-exits t)
		 (let ((newproc (muproc-spawn 'receiver-6
					       #'receiver-1
					       (list mbox)
					       :errorstream err-stream
					       :link t)))
		   (mumsg-receive (from)
		     ((terminated reason) t
		      (%enqueue% mbox (list :trapped reason))))))))
      (let ((mbox (%make-queue%)))
	(muproc-spawn 'sender-6
		       #'sender-1
		       (list (log-stream) mbox)
		       :errorstream (log-stream))
	(muproc-with-timeout (3 :starter-timed-out)
	  (%dequeue% mbox))))
  (:trapped :rcv-1-exit))

(deftest normal-exit-link-2
    ;; Verify that we get a message on 'normal' exit when trapping exits
    (labels ((receiver-1 (mbox)
	       (progn))
	     (sender-1 (err-stream mbox)
	       (muproc-with-timeout (2 (%enqueue% mbox :sender-timed-out))
		 (muproc-set-trap-exits t)
		 (let ((newproc (muproc-spawn 'receiver-7
					       #'receiver-1
					       (list mbox)
					       :errorstream err-stream
					       :link t)))
		   (mumsg-receive (from)
		     ((terminated reason) t
		      (%enqueue% mbox (list :trapped reason))))))))
      (let ((mbox (%make-queue%)))
	(muproc-spawn 'sender-7
		       #'sender-1
		       (list (log-stream) mbox)
		       :errorstream (log-stream))
	(muproc-with-timeout (3 :starter-timed-out)
	  (%dequeue% mbox))))
  (:trapped :normal-exit))

(deftest linked-exits-3
    ;; Verify that basic linked exits work
    (labels ((start-3 ()
	       (progn))
	     (start-2 (err-stream)
	       (muproc-spawn 'start-7 #'start-3 nil
			      :errorstream err-stream
			      :link t)
	       (sleep 2))
	     (start-1 (err-stream mbox)
	       (muproc-with-timeout (5 (%enqueue% mbox :sender-timed-out))
		 (muproc-set-trap-exits t)
		 (let ((newproc (muproc-spawn 'start-8
					       #'start-2
					       (list err-stream)
					       :errorstream err-stream
					       :link t)))
		   (mumsg-receive (from)
		     ((terminated reason) t
		      (%enqueue% mbox (list :trapped reason))))))))
      (let ((mbox (%make-queue%)))
        (sleep 1) ; allow previous test to finish
	(muproc-spawn 'start-9
		       #'start-1
		       (list (log-stream) mbox)
		       :errorstream (log-stream))
	(muproc-with-timeout (3 :starter-timed-out)
	  (%dequeue% mbox))))
  (:trapped (:linked-to-exit start-7 :for-reason :normal-exit)))

(deftest rippling-linked-exits-4
    ;; Verify that termination ripples through links
    (labels ((start-2 (num err-stream)
	       (when (> num 0)
		 (let ((newproc (muproc-spawn (intern (format nil "NEW-~d" num)
						       :muproc-test)
					       #'start-2
					       (list (1- num) err-stream)
					       :errorstream err-stream
					       :link t)))))
	       (sleep num)) ; sleep some time, more than next, but not too little
	     (start-1 (err-stream mbox)
	       (muproc-with-timeout (10 (%enqueue% mbox :sender-timed-out))
		 (muproc-set-trap-exits t)
		 (let ((newproc (muproc-spawn 'start-10
					       #'start-2
					       (list 5 err-stream)
					       :errorstream err-stream
					       :link t)))
		   (mumsg-receive (from)
		     ((terminated reason) t
		      (%enqueue% mbox (list :trapped reason))))))))
      (let ((mbox (%make-queue%)))
        (sleep 1) ; allow previous test to finish
	(muproc-spawn 'start-11
		       #'start-1
		       (list (log-stream) mbox)
		       :errorstream (log-stream))
	(muproc-with-timeout (10 :starter-timed-out)
	  (%dequeue% mbox))))
  (:trapped
   (:linked-to-exit
    new-5
    :for-reason
    (:linked-to-exit
     new-4
     :for-reason
     (:linked-to-exit
      new-3
      :for-reason
      (:linked-to-exit
       new-2
       :for-reason
       (:linked-to-exit
	new-1
	:for-reason
	:normal-exit)))))))

(deftest rippling-linked-exits-4a
    ;; Same as link-4 above, except first process is not linked.
    (labels ((start-2 (num err-stream)
	       (when (> num 0)
		 (let ((newproc (muproc-spawn (intern (format nil "START-~d" num)
						       :muproc-test)
					       #'start-2
					       (list (1- num) err-stream)
					       :errorstream err-stream
					       :link t)))))
	       (sleep (float (/ num 10))))
	     (start-1 (err-stream mbox)
	       (muproc-with-timeout (1 (%enqueue% mbox :sender-timed-out))
		 (muproc-set-trap-exits t)
		 (let ((newproc (muproc-spawn 'start-12
					       #'start-2
					       (list 5 err-stream)
					       :errorstream err-stream
					       :link nil)))
		   (mumsg-receive (from)
		     ((terminated reason) t
		      (%enqueue% mbox (list :trapped reason))))))))
      (let ((mbox (%make-queue%)))
	(muproc-spawn 'start-13
		       #'start-1
		       (list (log-stream) mbox)
		       :errorstream (log-stream))
	(muproc-with-timeout (2 :starter-timed-out)
	  (%dequeue% mbox))))
  :sender-timed-out)

(deftest register-1
    ; Verify that we can register a port name and subsequently use it.
    (labels ((start-2 (mbox)
	       (muproc-with-timeout (5 (%enqueue% mbox :two-timed-out)) 
		 (muproc-with-registered-port (:two)
		   (mumsg-receive (from)
		     ((request) (oddp request)
		      (mumsg-send from :reply (* 25 request)))
		     ((request) (evenp request)
		      (mumsg-send from :reply (* 10 request)))))))
	     (start-1 (mbox)
	       (muproc-with-timeout (5 (%enqueue% mbox :one-timed-out))
		 (sleep .5)
		 (let ((port (muproc-get-registered-port :two)))
		   (unless port
		     (%enqueue% mbox '(:bad-port port)))
		   (mumsg-send port :request 42))
		 (mumsg-receive (from)
		   ((reply) t
		    (%enqueue% mbox (cons :two-reply reply)))))))
      (let ((mbox (%make-queue%)))
	(muproc-spawn 'start-14
		       #'start-1
		       (list mbox)
		       :errorstream (log-stream))
	(muproc-spawn 'start-15
		       #'start-2
		       (list mbox)
		       :errorstream (log-stream))
	(muproc-with-timeout (2 :starter-timed-out)
	  (%dequeue% mbox))))
  (:two-reply . 420))

(deftest computation-1
    ;; This test performs a more 'realistic' test in that
    ;; it actually performs a verifiable, distributed 
    ;; computation: The test spawns three process generates
    ;; a random number code for each, generates a random sequence
    ;; of the addresses associated with each of the processes.
    ;; A packet is then sent back and forth between the three 
    ;; processes (which are identical, except for their name),
    ;; each one tagging the packet with their code, until the 
    ;; address sequence is exhausted, at which point the tagged
    ;; sequence is returned to the spawning process.
    ;;   Since the spawning process has both the generated codes
    ;; and address sequence, it can verify that the tagged sequence
    ;; is as expected.
    (let* ((addresses  '(:one :two :three))
	   (alist (loop for name in addresses
					; generate a random tag
			collect (cons name (random 1000)))))
      (labels ((worker-1 (mbox name tag addr-list)
		 (muproc-with-timeout (15 (%enqueue% mbox (cons name :timed-out)))
		   (assert (find :home addr-list))
		   (muproc-with-registered-port (name)
		     (sleep .5) ;; wait to ensure that other workers register ports
		     (let ((name-assoc (loop 
					for a in addr-list
					for port = (muproc-get-registered-port a)
					do (assert port)
					collect (cons a port))))
		       (loop	
			(mumsg-receive (from)
			  ((todo done) (null todo)
			   (let ((dest (cdr (assoc :home name-assoc))))
			     (mumsg-send dest
					  :done done :from name)))
			  ((todo done) t
			   (let ((dest (cdr (assoc (car todo) name-assoc))))
			     (mumsg-send dest
					  :todo (cdr todo)
					  :done (cons (list name tag)
						      done))))))))))
	       (home (mbox addr-list)
		 (muproc-with-timeout (15 (%enqueue% mbox :home-timed-out))
		   (muproc-set-trap-exits t)
		   (muproc-with-registered-port (:home)
		     (loop ;; for each process to be spawned
		      for (name . tag) in alist
		      do (progn
			   (muproc-spawn name
					  #'worker-1
					  (list mbox
						name
						tag 
						(cons :home addresses))	; arglist
					  :errorstream *trace-output*
					  :link t)))
		     (sleep 3)
		     (let ((name-assoc (loop 
					for a in addr-list
					for port = (muproc-get-registered-port a)
					do (assert port)
					collect (cons a port)))
			   (todo (loop 
				  ;; generate a random sequence of addresses:
				  for count from 1 to 10
				  collect (elt addresses (random (length addresses))))))
		       (mumsg-send (cdr (assoc (first todo) name-assoc))
				    :todo (cdr todo) :done nil)
		       (mumsg-receive (from)
			 ((terminate reason) t
			  (%enqueue% mbox (cons :home-got-terminate reason)))
			 ((done from) t
			  (let ((local-done (mapcar (lambda (o)
						      (list o (cdr (assoc o alist))))
						    (cdr (reverse todo)))))
			    (%enqueue% mbox 
					     (list :return-this
						   (equal done local-done)))))))))))
	(muproc-with-timeout (15 :starter-timed-out)
	  (let ((mbox (%make-queue%)))
	    (muproc-spawn 'home
			   #'home
			   (list mbox addresses)
			   :errorstream *trace-output*)
	    (%dequeue% mbox)))))
  (:return-this T))

(deftest unique-muproc-names-1
    (labels ((mup (mbox num)
	       (sleep .2)
	       (%enqueue% mbox (list :done num)))
	     (start-one (name mbox num)
	       (muproc-spawn name
			      #'mup
			      (list mbox num)
			      :errorstream (log-stream))))
      (let ((mbox (%make-queue%)))
	(loop repeat 5
	      for count from 1
	      collect (progn 
			(start-one 'mup-1 mbox count)
			(%dequeue% mbox)))))
  ((:done 1) (:done 2) (:done 3) (:done 4) (:done 5)))

(deftest unique-muproc-names-2
    (labels ((mup (mbox num)
	       (sleep .2)
	       (%enqueue% mbox (list :done num)))
	     (start-one (name mbox num)
	       (muproc-spawn name
			      #'mup
			      (list mbox num)
			      :errorstream (log-stream))))
      (let ((mbox (%make-queue%)))
	(list
	 (handler-case
	     (loop repeat 5
		   for count from 1
		   collect (progn 
			     (start-one 'mup-1 mbox count)
			     (%dequeue% mbox)
			     ))
	   (error (err)
	     :error-occurred))
	 (handler-case
	     (loop repeat 5
		   for count from 1
		   collect (progn 
			     (start-one 'mup-1 mbox count)
			     ;(%dequeue% mbox) ;; submit many without waiting
			     ))
	   (error (err)
	     :error-occurred)))))
  (((:done 1) (:done 2) (:done 3) (:done 4) (:done 5))
   :error-occurred))

(deftest send-to-terminated-1
    ;; It is not an error to send a message to a terminated muproc,
    ;; provided that we are not doing it via a named port.
    (labels ((term-1 (proc)
	       (mumsg-send proc :request :dummy))
	     (serv-1 (mbox)
	       (let ((report :initial-value))
		 (unwind-protect
		      (muproc-with-timeout (5 (setf report :timeout))
			(mumsg-receive (from)
			  ((request) t
			   (sleep 1) ;; term-1 should terminate while we sleep
			   (mumsg-send from :reply nil)))
			(setf report :done))
		   (%enqueue% mbox report)))))
      (let* ((mbox (%make-queue%))
	     (srv (muproc-spawn 'serv-1
				 #'serv-1
				 (list mbox)
				 :errorstream (log-stream))))
	(muproc-spawn 'term-1
		       #'term-1
		       (list srv)
		       :errorstream (log-stream))
	(%dequeue% mbox)))
  :done)

(deftest send-to-terminated-2
    ;; It is an error to send a message to a terminated muproc when we
    ;; do it via a named port.
    (labels ((term-1 (mbox proc)
	       (muproc-with-timeout (5 (%enqueue% mbox :term-1-timed-out))
		 (muproc-with-registered-port (:term-1)
		   (mumsg-send proc :request :term-1)
		   (mumsg-receive (from)
		     ((reply) t
		      (progn))))))
	     (serv-1 (mbox)
	       (let ((report :initial-value))
		 (unwind-protect
		      (handler-case
			  (muproc-with-timeout (5 (setf report :serv1-timeout))
			    (muproc-with-registered-port (:serv-1)
			      (let (addr)
				(mumsg-receive (from)
				  ((request) t
				   (setf addr request)
				   (mumsg-send addr :reply :first-one)))
				(sleep 1) ;; term-1 should terminate while we sleep
				(mumsg-send addr :reply :this-one-will-fail)))
			    (setf report :serv1-ok))
			(error (err)
			  (setf report :serv1-error)))
		   (%enqueue% mbox report)))))
      (let* ((mbox (%make-queue%))
	     (srv (muproc-spawn 'serv-2
				 #'serv-1
				 (list mbox)
				 :errorstream (log-stream))))
	(muproc-spawn 'term-2
		       #'term-1
		       (list mbox srv)
		       :errorstream (log-stream))
	(%dequeue% mbox)))
  :serv1-error)

(deftest muproc-send-1
    ;; verify that fundamental data sending works
    ;; verify that any data can be exchanged using muproc-send
    (labels ((muproc-rcv (mbox)
		 (let ((report :rcv-initial-value))
		   (unwind-protect
			(handler-case
			    (muproc-with-timeout (5 (setf report :rcv-timed-out))
			      (setf report
				    (multiple-value-bind (data type port sent-at)
					(muproc-receive)
				      (cond
					((not (typep port 'muproc-port))
					 (setf report (cons :rcv-bad-port port)))
					((not (integerp sent-at))
					 (setf report (cons :rcv-bad-sent-at sent-at)))
					(t
					 (setf report (list :rcv-ok data type)))))))
			  (error (err)
			    (setf report (cons :rcv-error err))))
		   (%enqueue% mbox report))))
	       (muproc-snd (mbox rcv)
		 (let ((report :snd-initial-value))
		   (unwind-protect
			(handler-case
			    (muproc-with-timeout (5 (setf report :snd-timeout))
			      (muproc-send rcv :plain :the-data)
			      (setf report :snd-ok))
			  (error (err)
			    (setf report (cons :snd-error err))))
		     (%enqueue% mbox report)))))
	(muproc-with-timeout (5 :timeout)
	  (let* ((mbox1 (%make-queue%))
		 (mbox2 (%make-queue%))
		 (rcv (muproc-spawn 'muproc-rcv-1
				     #'muproc-rcv
				     (list mbox1)
				     :errorstream (log-stream))))
	    (muproc-spawn 'muproc-snd-1
			   #'muproc-snd
			   (list mbox2 rcv)
			   :errorstream (log-stream))
	    (list 
	     (%dequeue% mbox1)
	     (%dequeue% mbox2)))))
  ((:rcv-ok :the-data :plain) :snd-ok))

(deftest muproc-monitor-1
    (labels ((muproc-1 (mbox pid two-mbox)
		 (let ((report :one-initial-value))
		   (unwind-protect
			(handler-case
			    (muproc-with-timeout (5 (setf report :one-timed-out))
			      (muproc-set-trap-exits t)
			      (muproc-monitor pid)
			      (%enqueue% two-mbox :go-ahead)
			      (mumsg-receive (from)
				((terminated reason) t
				 (setf report (cons :one-report reason)))))
			  (error (err)
			    (setf report (cons :one-error err))))
		   (%enqueue% mbox report))))
	       (muproc-2 (mbox)
		 (let ((report :two-initial-value))
		   (unwind-protect
			(handler-case
			    (muproc-with-timeout (5 (setf report :two-timeout))
			      (%dequeue% mbox)
			      (setf report :two-ok)
			      (muproc-exit :two-exit))
			  (error (err)
			    (setf report (cons :snd-error err))))
		     (%enqueue% mbox report)))))
	(muproc-with-timeout (5 :timeout)
	  (let* ((mbox1 (%make-queue%))
		 (mbox2 (%make-queue%))
		 (two (muproc-spawn 'two
				     #'muproc-2
				     (list mbox2)
				     :errorstream (log-stream))))
	    (muproc-spawn 'one
			   #'muproc-1
			   (list mbox1 two mbox2)
			   :errorstream (log-stream))
	    (list 
	     (%dequeue% mbox1)
	     (%dequeue% mbox2)))))
  ((:one-report . :two-exit) :two-ok))

(deftest interrupt-1
    (labels ((mup (mbox period)
	       (let ((report :mup-initial-value))
		 (unwind-protect
		      (muproc-with-timeout (period (setf report :mup-timeout))
			(let ((timer (muproc-make-interrupt-timer
				      (let ((count 10))
					(lambda ()
					  (declare (special *muproc-inport*))
					  (cond 
					    ((plusp count)
					     (mumsg-send *muproc-inport* :ruptus count)
					     (decf count))
					    (t (mumsg-send *muproc-inport* :done :done))))))))
			  (muproc-schedule-timer-relative timer .1 .1)
			  (let (data)
			    (loop
			     (mumsg-receive (from)
			       ((ruptus) t 
				;(muproc-log-errorstream "Got RUPTUS ~s." ruptus)
				(push ruptus data))
			       ((done) t
				;(muproc-log-errorstream "Got DONE ~s." done)
				(setf report (list :data data))
				(muproc-exit :done))
			       (() t
				(muproc-exit (setf report (list :mup-got *muproc-packet*)))))))))
		   (%enqueue% mbox report)
		   (%enqueue% mbox :done)))))
      (let ((period 5))
	(muproc-with-timeout (period :timeout)
	  (let* ((mbox (%make-queue%))
		 (mup (muproc-spawn 'mup
				     #'mup
				     (list mbox (1- period))
				     :errorstream (log-stream))))
	    (loop for data = (%dequeue% mbox)
		  until (eq data :done)
		  collect data)
	    ))))
  ((:data (1 2 3 4 5 6 7 8 9 10))))