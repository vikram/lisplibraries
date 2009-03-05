;;; Copyright (c) 2005-2006, Mu Aps. All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; WRITTEN BY: KLAUS HARBO klaus@harbo.net / klaus@mu.dk


(in-package :cl-muproc)

;;; DEBUG LEVEL
;;; ============================================================================

(defparameter *debug-level* 0
  "When NIL no debug output is generated, otherwise it must be a positive ~
      integer.  The higher the value, the more debug output is generated. ~
      Levels in use: 0 - Important messages.
                     1 - More details.
                     3 - Lots of debugging info.")

;;; CONSTANTS
;;; ============================================================================

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +tag-random-order+ 12
    "Order of magnitude for the pseudo-random numbers used to
uniquely identify messages using `muproc-with-message-tag'."))


;;; MACROS
;;; ============================================================================

(defmacro defdocstring (name type string)
  "Sets the documentation string of `name' of type `type' to `string'.
Normally docstrings are set as a side-effect of evaluating forms such
as `defvar' and `defparameter', etc.  However, we want to document
special variables for which we only want a value bound inside muprocs,
both `defvar' and `defparameter' bind a value, so we use
\`defdocstring' instead"
  `(eval-when (:compile-toplevel :load-toplevel :execute)
    (setf (documentation ',name ',type) ,string)))

(defmacro dbgformat (level &rest args)
  "Log `args' with `muproc-log-errorstream' iff `*debug-level*'
      is not NIL and `level' >= `*debug-level*'."
  `(when (and *debug-level* (>= *debug-level* ,level))
    (muproc-log-errorstream ,@args)))

(defmacro muproc-with-message-tag ((tag-name) &body body)
  `(let ((,tag-name (random #.(expt 10 +tag-random-order+))))
    ,@body))

(defmacro mumsg-receive ((from-bind) &body clauses)
  "Receive MUMSG from the input queue.  `clauses' must be a list of
packet matching forms, each consisting of a PACKET BINDING FORM, a
GUARD EXPRESSION, and zero or more ACTION FORMS.  Fundamentally,
`mumsg-receive' iterates over the contents of the input queue, trying
to match the packets in the queue the PACKET MATCHING FORMS in
`clauses'.  In each PACKET MATCHING FORM, the PACKET BINDING FORM
declares the names of fields which must be present in the MUMSG being
matched.  IFF they are all present in a MUMSG, the names in the PACKET
BINDING FORM are lexically bound to the values in the corresponding
fields in the MUMSG, and the GUARD EXPRESSION is evaluated.  IFF the
GUARD EXPRESSION evaluates a value which is not NIL, the ACTION FORMS
are evaluated, the value of which is returned `mumsg-receive'.  If no
packet can be matched, `mumsg-receive' continues matching the next
packet in the input queue.  If the input queue is exhausted, and
there's no more input available from the muproc's input port,
`mumsg-receive' will block until input does become available, at
which time the packet matching will continue.

EXAMPLE:
    (mumsg-receive (from)
      ((terminate reason) t
       (format t \"Received terminate message from ~a with reason ~a.\"
	       terminate reason) )
      ((number) (oddp number)
       (format t \"Received an ODD number: ~d.\" number))
      ((number) (evenp number)
       (format t \"Received an EVEN number: ~d.\" number)))
"
  (let ((+block+ (gensym))
	(+index+ (gensym))
	(+parts+))
    `(block ,+block+
      (let ((,from-bind))
	(declare (ignorable ,from-bind))
	,(progn
	  (loop
	   for form in clauses
	   do (destructuring-bind (name-list guard &body body) form
		(let ((bindings (loop
				 for name in name-list
				 collect (list name
					       (gensym)
					       `(get-field
						 ,(intern (symbol-name name)
							  :keyword)
						 *muproc-mumsg*)))))
		  (push `(let* ,(append (loop for (nil sym value) in bindings
					      collect (list sym value))
					(loop for (name sym) in bindings
					      collect `(,name (cdr ,sym))))
			  ,@(loop
			     for (name nil nil) in bindings
			     collect `(declare (ignorable ,name)))
			  (when (and ,@(loop for (nil sym) in bindings
					     collect sym)
				     ,guard)
			    (consume-input ,+index+)
			    (setf ,from-bind (cdr (get-field :from *muproc-packet*)))
			    (return-from ,+block+
			      (progn ,@body))))
			+parts+))))
	  `(loop
	    for ,+index+ from 1
	    for *muproc-packet* = (pending-input ,+index+)
	    for *muproc-mumsg* =  (cdr (get-field :data *muproc-packet*))
	    do (progn
		 (dbgformat 3 "Matching packet ~a.~%" *muproc-packet*)
		 ,@(reverse +parts+))))))))

(defmacro muproc-with-registered-port ((name) &body body)
  "Register a port under `name', evaluate `body', then unregister
`name' again."
  `(unwind-protect (progn
		     (muproc-register-port-name ,name)
		     ,@body)
    (muproc-unregister-port-name ,name)))

(defmacro muprocn ((&optional (timeout 10) (action :timeout)) &body body)
  "Execute `body' in a newly newly created muproc.  `muprocn' is used
to interface code executing in non-muproc LISP processes with code
requiring a muproc context (e.g. for message passing to/from
muprocs)."
  (let ((+mbox+ (gensym)))
    `(let ((,+mbox+ (%make-queue%)))
      (%with-timeout% (,timeout ,action)
	(muproc-spawn (gensym "MUPROCN-")
		      (lambda ()
			(%enqueue% ,+mbox+
				   (multiple-value-list
				    (progn ,@body))))
		      nil
		      :errorstream *trace-output*)
	(apply #'values (%dequeue% ,+mbox+))))))

(defmacro muproc-with-timeout ((timeout &body timeout-forms) &body body)
  `(%with-timeout% (,timeout ,@timeout-forms) ,@body))

(declaim (inline muproc-all-processes))
(defun muproc-all-processes ()
  "Returns a list of all currently running muprocs."
  (loop
   for proc in (%all-processes%)
   when (muproc-p proc)
   collect proc))


;;; EXPORTED VARIABLES
;;; ============================================================================

;; These symbols are documented this way because they should only be
;; defined inside muprocs, so we do not want `defvar' to bind a 
;; value globally

;; This symbol is lexically bound by `mumsg-receive'
(defvar *muproc-packet*) ;; unbound except in muprocs
(defdocstring *muproc-packet* variable
  "Lexically bound by `mumsg-receive' to be the ENTIRE DATA retrieved
from the muproc's input port.")

;; This symbol is lexically bound by `mumsg-receive'
(defvar *muproc-mumsg*)	;; unbound except in muprocs
(defdocstring *muproc-mumsg* variable
  "Lexically bound by `mumsg-receive' to be the PAYLOAD DATA of the
packet retrieved from the muproc's input port.")

(defvar *muproc-inport*) ;; unbound except in muprocs (cf. in-muproc-p)
(defdocstring *muproc-inport* variable
  "Variable, dynamically bound in every muproc, which holds the
`muproc-port' to which all messages to the muproc are sent.  Almost
always accessed indirectly, via `mumsg-send' and `mumsg-receive'.")

(defvar *muproc-errorstream*) ;; unbound except in muprocs
(defdocstring *muproc-errorstream* variable
  "Variable, dynamically bound to a stream in every muproc, to which
the muproc can write trace and debugging output, error
messages etc.")
(defvar *muproc-errorstream-lock*
  (%make-lock% :name "muproc-errorstream-lock"))

(defvar *named-ports*) ;; unbound except in muprocs
(defdocstring *named-ports* variable
  "Variable, dynamically bound in every muproc, which holds a map from
registered muproc names to the port of the muproc that registered that
name.  The ports are cached so that name lookup occurs only on first
use.  `*named-ports*' is normally not accessed directly, but
via `muproc-with-registered-port' and `mumsg-send'.")

(defvar *pending-input*) ;; unbound except in muprocs
(defdocstring *pending-input* variable
  "Variable, dynamically bound in every muproc, which hold input to
the muproc which has already been read from `*muproc-inport*', but has
not yet been consumed by `mumsg-receive'.  `*pending-input*'
is normally not accessed directly, but rather via `mumsg-receive' and
`muproc-discard-all-pending-input'.")

(defvar *timers*) ;; unbound except in muprocs
(defdocstring *timers* variable
  "A list of timers to be unscheduled before the muproc terminates.
Cf. `muproc-schedule-timer', `%schedule-timer-relative%',
`muproc-unschedule-timer', and `muproc-make-interrupt-timer'.")

;;; INTERNAL VARIABLES
;;; ============================================================================

(defvar *names* (make-hash-table :test #'eq)
  "Hash table containing the names of the currently running muprocs.
Muproc names must be unique.")

(defvar *links* nil
  "List of MUPROCs which are linked.  An association is one of two
types: Either two muproc are LINKed, which means that if either of
them is to terminate the other one will be notified; or one MONITORs
they other, which means that it will notified when/if they other one
terminates.  On `*links*' a LINK entry is represented by a list (:LINK
A B), and a MONITOR entry is represented by a link (:MONITOR A B).  A
and B in the above lists are both muproc objects.  The interpretation
of the lists is that in the former case, A and B are linked, and in
the latter that A is to monitor B \(and hence be notified in case B
terminates).")
(defvar *ports* (make-hash-table :test #'eq)
  "A muproc LISP process ID to muproc inport map.")
(defvar *registrations* (make-hash-table :test #'eq)
  "A name to port map of registered names.")
(defvar *name-counter* 0
  "Counter for uniquely identifying various objects.")

;;; MUPROC CONDITIONS
;;; ============================================================================

(define-condition muproc-condition ()
  ((id               :initarg :id
		     :initform nil
		     :reader condition-id)
   (aux              :initarg :aux
		     :initform nil
		     :reader condition-aux)))
(define-condition muproc-error (muproc-condition error) ())
(define-condition muproc-exit-condition (muproc-condition)
  ;; Exported from muproc package, could in theory be caught by muproc
  ;; code, so signalling this condition (as in `muproc-exit') is not a
  ;; guarantee that the muproc will terminate.
  ((condition-reason :initarg :condition-reason
		     :initform nil
		     :reader condition-reason)
   (condition-muproc :initarg :condition-muproc
		     :initform nil
		     :reader condition-muproc)))
(define-condition muproc-linked-exit-condition (muproc-exit-condition)
  ;; Signalled when a linked exit occurs.  Is NOT exported from muproc
  ;; package and should NEVER be caught by muproc code.  Guaranteed to
  ;; terminate muproc.
  ())
(define-condition muproc-externally-terminated-condition (muproc-exit-condition)
  ;; NOT exported from muproc package.  Should NEVER be caught by
  ;; user muproc, so signalling this condition (as in `muproc-kill') is
  ;; guaranteed to terminate muproc.
  ())
(define-condition muproc-send-to-terminated-muproc-condition (muproc-condition)
  ;; Signalled when a muproc tries to send a message to a muproc which
  ;; has already terminated.  May be caught by muproc code.  If not
  ;; handled, this condition is ignored.
  ((muproc-to-port :initarg :muproc-to-port
		   :initform :not-specified
		   :reader muproc-to-port)
   (muproc-to-data :initarg :muproc-to-data
		   :initform :not-specified
		   :reader muproc-to-data)))
(define-condition muproc-send-to-terminated-muproc-error (muproc-send-to-terminated-muproc-condition
							  muproc-error)
  ;; Signalled when a muproc tries to send a message to a muproc which
  ;; has already terminated, and the sending happens via a REGISTERED
  ;; PORT NAME.  May be caught by muproc code.  If not handled, this
  ;; condition cause an error to be signalled,
  ())
(defmethod print-object ((c muproc-exit-condition) stream)
  (print-unreadable-object (c stream)
    (format stream "EXIT ~a ~s"
	    (muproc-name (condition-muproc c))
	    (condition-reason c))))

(defun muproc-exit (reason &optional (muproc (muproc-current-process)))
  "Exit from `muproc' with reason `reason'.  If `muproc' is not equal ~
   to the current muproc, the effect may be sending a \(terminated, reason) ~
   MUMSG, depending on whether the muproc in question traps exits.  If ~
   `muproc' is equal to the current muproc, a MUMSG is never sent."
  (flet ((signal-it ()
	   (dbgformat 3 "muproc-exit signalled in ~a with reason ~a." (muproc-name) reason)
	   (signal (make-condition 'muproc-exit-condition
				   :condition-reason reason
				   :condition-muproc (muproc-current-process)))))
    (if (eq muproc (muproc-current-process))
	(signal-it)
	(%process-interrupt% 
	 muproc
	 (lambda ()
	   (if (muproc-trap-exits-p)
	       (mumsg-send *muproc-inport* :terminated nil :reason reason)
	       (signal-it))))))
  muproc)

;;; UTILITY FUNCTIONS
;;; ============================================================================

(eval-when (:load-toplevel :compile-toplevel :execute)
  ;; Create a unique value to distinguish wake-up packets
  ;; from any other kind of packet
  (let ((wakeup (load-time-value (cons 'wake 'up))))
    (defun wakeup-packet ()
      wakeup)
    (defun wakeup-packet-p (pkt)
      (eq pkt wakeup))))

(defun muproc-lock (&optional (muproc (muproc-current-process)))
  (getf (%process-plist% muproc) 'muproc-lock))

(defun (setf muproc-lock) (new muproc)
  ;; INV: Called ONLY from `muproc-spawn' to ensure that 
  ;;      `muproc-lock' can be called without enclosing
  ;;      `%with-exclusive-access%'.
  (setf (getf (%process-plist% muproc) 'muproc-lock) new))

(defmacro with-process-lock ((&optional (muproc nil)) &body body)
  (let ((lock-var (gensym "-lock")))
    `(let ((,lock-var (muproc-lock ,muproc)))
       (if ,lock-var
           (%with-lock% (,lock-var)
             ,@body)
           (progn ,@body)))))

(defun muproc-address-p (obj)
  (or (typep obj 'muproc-port)
      (muproc-p obj)
      (keywordp obj)))

;;; TRAPPING EXITS
;;; ============================================================================

(defun muproc-trap-exits-p (&optional (muproc (muproc-current-process)))
  "Returns T iff `muproc' traps exits, that is gets a message in its
input queue with fields `terminated' and `reason' when a linked
process terminates."
  (with-process-lock (muproc)
    (getf (%process-plist% muproc) 'muproc-trap-exits)))

(defun muproc-set-trap-exits (new-value &optional (muproc (muproc-current-process)))
  "Controls whether `muproc' traps exits, cf. `muproc-trap-exits-p'."
  (with-process-lock (muproc)
    (setf (getf (%process-plist% muproc) 'muproc-trap-exits)
	  (and new-value t))))

;;; MUPROC NAMES
;;; ============================================================================

(defun muproc-name-p (obj)
  "Returns T iff `obj' is a valid muproc name.  Muproc names must be a
symbol, but cannot be NIL.  Note: Muproc names uniquely identify each
muproc.  Muproc names are not the same as 'process names' of the
underlying LISP processes \(which are strings in many implementations)."
  (and obj				; not NIL
       (symbolp obj)))

(defun register-muproc-name (name muproc)
  (unless (muproc-name-p name) ;; NIL not allowed
    (error "Muproc name not a permitted symbol: ~s." name))
  (unless (%process-p% muproc)
    (error "Not a process: ~s." muproc))
  (%with-exclusive-access%
    (when (gethash name *names*)
      (error "Muproc name already registered to a muproc: ~s." name))
    (setf (gethash name *names*) muproc))
  (with-process-lock (muproc)
    (setf (getf (%process-plist% muproc) 'muproc-name) name)))

(defun unregister-muproc-name (muproc)
  (assert (%process-p% muproc))
  (%with-exclusive-access%
    (remhash (muproc-name muproc) *names*)))

(declaim (inline muproc-name))
(defun muproc-name (&optional (muproc (muproc-current-process)))
  "Return the muproc name of `muproc'.  Cf. `muproc-name-p'."
  (with-process-lock (muproc)
    (getf (%process-plist% muproc) 'muproc-name)))

(declaim (inline (setf muproc-name)))
(defun (setf muproc-name) (name muproc)
  "Sets the name of `muproc' to be `name'.  Cf. `muproc-name-p'."
  (register-muproc-name name muproc))

(declaim (inline next-muproc-id))
(defun next-muproc-id ()
  (%with-exclusive-access%
    (incf *name-counter*)))

(declaim (inline in-muproc-p))
(defun in-muproc-p ()
  "Returns T iff evaluated in a muproc."
  (boundp '*muproc-inport*))


;;; DEBUG OUTPUT
;;; ============================================================================

(defun muproc-log-errorstream (format-string &rest args)
  "Every muproc has a reference to a stream to which error messages
can be sent, in case of severe errors.  The stream is bound to the
special variable `*muproc-errorstream*'.  `muproc-log-errorstream'
uses `format' to send formatted output to the errorstream, so it takes
the same arguments \(except stream argument of `format', of course).
NOTE: `muproc-log-errorstream' is NOT intended for normal logging
operations, but error situations where normal logging operations may
not function (e.g. for errors in the muproc run-time system)."
  (assert (in-muproc-p))
  (multiple-value-bind (sec min hour)
      (decode-universal-time (get-universal-time))
    (%with-lock% (*muproc-errorstream-lock*)
      (format *muproc-errorstream* "~&~2,'0d:~2,'0d:~2,'0d.~3,'0d ~a "
	      hour min sec (mod (get-internal-real-time) 1000)
	      (muproc-name))
      (apply #'format *muproc-errorstream* format-string args)
      (finish-output *muproc-errorstream*))))

;;; MUMSGs
;;; ===================================================================

(declaim (inline pack-msg/packet))
(defun pack-msg/packet (tag plist)
  (cons tag
	(loop
	 for (kw val) on plist by #'cddr
	 do (unless (keywordp kw)
	      (error "MUMSG part name not a keyword: ~a." kw))
	 collect (cons kw val))))

;;; MUMSGs is the encoding of the PAYLOAD OF the messages sent between muprocs

(declaim (inline mumsg))
(defun mumsg (&rest plist)
  (unless (evenp (length plist))
    (error "Odd number of args, MUMSGs must have a defined value for each field: ~s." plist))
  (pack-msg/packet :mumsg plist))

(declaim (inline mumsg-p))
(defun mumsg-p (obj)
  (and (consp obj) (eq (car obj) :mumsg)))

(declaim (inline get-field))
(defun get-field (name msg)
  (assert (and (consp msg) (or (eq (car msg) :mumsg)
			       (eq (car msg) :mupacket))))
  (assoc name (cdr msg)))

(declaim (inline muproc-get-field))
(defun muproc-get-field (name msg)
  "Returns two values, the value of the field `name' in the MUMSG
`msg', and a value indicating whether the field was found or not."
  (let ((the-cons (get-field name msg)))
    (values (cdr the-cons)
	    (and the-cons t))))

;;; MUPACKETs is the encoding of the messages sent between between muprocs
;;;
;;; `:mumsg' MUPACKET:
;;;                                    ((:TO       .  <port-or-keyword-symbol>)
;;;  				        (:FROM     .  <port-or-keyword-symbol>)
;;;				        (:SENT-AT  .  <value returned by #'get-internal-real-time>)
;;;				        (:TYPE     .  :MUMSG)
;;;				        (:DATA     .  <mumsg-encoded-object>))
;;;

(declaim (inline mupacket))
(defun mupacket (&rest plist)
  (pack-msg/packet :mupacket plist))

(declaim (inline mupacket-p))
(defun mupacket-p (obj)
  (and (consp obj) (eq (car obj) :mupacket)))

(defun mupacket-field (name packet)
  (assert (and (consp packet) (eq (car packet) :mupacket)))
  (cdr (assoc name (cdr packet))))

(defun muproc-packet-age (&optional (packet *muproc-packet*))
  "Returns a `float' representing the age of the `packet' measured in
seconds."
  (assert (mupacket-p packet))
  (let ((the-cons (get-field :sent-at packet)))
    (assert the-cons)
    (coerce (/ (- (get-internal-real-time)
		  (cdr (get-field :sent-at packet)))
	       (load-time-value internal-time-units-per-second))
	    'float)))

;;; MUPROC-PORT
;;; ============================================================================

(defclass muproc-port ()
  ((queue           :initform (%make-queue%)
	            :reader port-queue
		    :documentation "The input queue of the muproc.
NOTE: It is an ERROR to keep a reference to the queue object itself,
keep only references to the muproc-port object, as the run-time system
relies on being able to disable input to a muproc when it terminates.")
   (name            :initarg :name
	            :initform (format nil "port-~d" (next-muproc-id))))
  (:documentation "`muproc-port' is the basic abstraction used for
sending messages to a muproc process.  Each muproc has exactly one
port, on which it receives all messages.  As such, it serves as an
input queue for the muproc."))

(defmethod print-object ((port muproc-port) stream)
  (print-unreadable-object (port stream :type t)
    (format stream "~a" (slot-value port 'name))))

(declaim (inline port-put))
(defun port-put (port packet)
  "Inserts `packet' into port.  `port' must be an instance of
`muproc-port'."
  (assert (typep port 'muproc-port))
  (dbgformat 3 "port-put called by ~a on port ~a msg ~a"
	     (muproc-name) port packet)
  (%enqueue% (port-queue port) packet))

(declaim (inline port-get))
(defun port-get (port)
  "Retrieve a message from `port'.  Returns the the first message in
queue, if any are available.  `port' must be an instance of `port'.
If no messages are available at `port', the process blocks."
  (assert (typep port 'muproc-port))
  (let ((the-packet (loop
		     for pkt = (%dequeue% (port-queue port))
		     while (wakeup-packet-p pkt)
		     finally (return pkt))))
    (dbgformat 3 "port-get called by ~a on port ~a msg ~a"
	       (muproc-name) port the-packet)
    the-packet))

(declaim (inline port-empty-p))
(defun port-empty-p (port)
  "Returns T if there are no input available on `port', and NIL
otherwise."
  (assert (typep port 'muproc-port))
  (%queue-empty-p% (port-queue port)))

;;; MUPROC-SEND
;;; ============================================================================

(defgeneric muproc-send (to type data)
  (:documentation "`muproc-send' is the fundamental data-sending
operator in muproc.  `to' must be an muproc, a muproc input port, or
the name of a registered port.  In almost all cases, using
`mumsg-send' is more appropriate, cf. this function.")
  (:method ((to muproc-port) type data)
    (unless (keywordp type)
      (error "Type not a keyword symbol: ~s." type))
    (handler-case
	(port-put to (mupacket :to to
			       :from *muproc-inport*
			       :sent-at (get-internal-real-time)
			       :type type
			       :data data))
      (error (err)
	(if (eq (slot-value to 'queue) 'muproc-terminated)
	    ;; This will be handled by the method dispatching on a
	    ;; symbol, that is the method for sending to a named
	    ;; ports, and could be handled by user code.  Sending an
	    ;; message to a terminated muproc is NOT an error, unless
	    ;; it was done via named port.
	    (signal (make-condition 'muproc-send-to-terminated-muproc-condition
				    :muproc-to-port to
				    :muproc-to-data data))
	    (error err)))))
  (:method (to type data)
    (if (muproc-p to)
	(muproc-send (muproc-port to) type data)
	(error "Unable to send to destination: ~a." to)))
  (:method ((to (eql 'muproc-terminated)) type data)
    (declare (ignore type))
    (signal (make-condition 'muproc-send-to-terminated-muproc-condition
			    :muproc-to-port to
			    :muproc-to-data data)))
  (:method ((to symbol) type data)
    (loop for count from 1
	  do (handler-case
		 (let ((port (gethash to *named-ports*)))
		   (unless port
		     (setf port (muproc-get-registered-port to))
		     (unless (typep port 'muproc-port)
		       (error "Cannot find port named ~s." to))
		     (setf (gethash to *named-ports*) port))
		   (return (muproc-send port type data)))
	       (muproc-send-to-terminated-muproc-CONDITION (err)
		 (if (= count 1) ;; first failure
		     (remhash to *named-ports*)
		     ;; NOTE: We're turning the condition into an error
		     (error (make-condition 'muproc-send-to-terminated-muproc-ERROR
					    :muproc-to-port (muproc-to-port err)
					    :muproc-to-data (muproc-to-data err)))))))))

;;; MUPROC-RECEIVE
;;; ============================================================================

(defun muproc-receive ()
  "Retrieves the first data element from pending input.  Returns four
values, \(1) the data retrieved, \(2) the type of data as specified in
corresponding `muproc-send' operation, and \(3) the port from which
the data was sent, and \(4) the time the data was sent. If there is no
data ready, `muproc-receive' will block until data arrives."
  (let ((packet (pending-input 1)))
    (values (cdr (get-field :data packet))
	    (cdr (get-field :type packet))
	    (cdr (get-field :from packet))
	    (cdr (get-field :sent-at packet)))))


;;; MUMSG-SEND
;;; ============================================================================

(defun mumsg-send (to &rest plist)
  "`mumsg-send' is used by muprocs to send data to muprocs.  The value
of `to' must be a `muproc-port', a muproc process, or a symbol naming
a registered port.  `data' can be any LISP data.  NOTE: The data sent
will likely end up in a different muproc, so arrangements need to be
made to ensure that several muprocs (LISP processes) do not update the
data without synchronization.  How this synchronization is achieved is
outside the scope of the muproc framework."
  (muproc-send to :mumsg (apply #'mumsg plist)))

;;; PENDING INPUT
;;; ============================================================================

(defun make-pending-input-array ()
  (make-array 0 :adjustable t :fill-pointer 0))

(defun muproc-discard-all-pending-input ()
  "Discard all pending input to process."
  ;; Discard input array
  (setf *pending-input* (make-pending-input-array))
  ;; Discard pending input on input port
  (loop
   while (not (port-empty-p *muproc-inport*))
   do (let ((object (port-get *muproc-inport*)))
	(declare (ignorable object))
	(dbgformat 3 "Discarded input ~s~%" object)
	))
  t)

(declaim (inline muproc-unmatched-input-count))
(defun muproc-unmatched-input-count ()
  "Returns the number of packets which have been read from the input
port of the muproc, but which has not yet be matched by any
`mumsg-receive' form."
  (fill-pointer *pending-input*))

(declaim (inline muproc-unmatched-input-p))
(defun muproc-unmatched-input-p ()
  "Returns T if `muproc-unmatched-input-count' evaluates to a
positive number, and NIL otherwise."
  (plusp (muproc-unmatched-input-count)))

(defun pending-input (index)
  "Get the `index'th element in the pending input queue, with first
element in the queue having index=1. NOTE: Should only be used via the
user interfaces `mumsg-receive' or `muproc-receive'."
  (assert (arrayp *pending-input*))
  (assert (integerp index))
  (assert (plusp index))
  (let ((size (muproc-unmatched-input-count)))
    (dbgformat 3 "pending-input (~a / ~a): ~a~%" index size *pending-input*)
    (assert (<= index (1+ size)))
    (when (= index (1+ size))
      ;; asking for input ONE beyond current pending input
      (vector-push-extend (port-get *muproc-inport*)
			  *pending-input*))
    (dbgformat 3 "pending-input: returning ~a.~%"
	       (aref *pending-input* (1- index)))
    (aref *pending-input* (1- index))))

(defun consume-input (index)
  "Extract and return the `index'th element in the input queue.
NOTE: Should only be used via the interface macro `mumsg-receive'."
  (dbgformat 3 "consume-input (~a): ~a~%" index *pending-input*)
  (let ((elem (pending-input index))
	(size (muproc-unmatched-input-count)))
    (loop for i from (1- index) to (- size 2)
	  do (setf (aref *pending-input* i)
		   (aref *pending-input* (1+ i))))
    (vector-pop *pending-input*)
    elem))

(declaim (inline muproc-msgtag=))
(defun muproc-msgtag= (t1 t2)
  (eql t1 t2))

;;; PROCESS-TO-PORT MAP
;;; ============================================================================

(defun muproc-port (muproc)
  "Retrieve the input port of a muproc."
  (assert (%process-p% muproc))
  (%with-exclusive-access%
    (gethash muproc *ports*)))

(defun (setf muproc-port) (port muproc)
  "Set the input port of a muproc.  NOTE: Should only be set by the
muproc run-time system."
  (assert (%process-p% muproc))
  (assert (or (typep port 'muproc-port) (null port)))
  (cond
    (port ;; we're setting a port in a muproc
     (%with-exclusive-access%
       (assert (not (gethash muproc *ports*)))
       (setf (gethash muproc *ports*) port)))
    (t ;; we're removing the port of muproc
     (%with-exclusive-access%
       (assert (gethash muproc *ports*))
       (remhash muproc *ports*)))))

;;; SPAWNING PROCESSES
;;; ============================================================================

(defun unwind-muproc (signalling-arg)
  "Performs the operations required to terminate a muproc."
  (macrolet ((protected (msg &body body)
	       `(handler-case
		 (progn
		   (dbgformat 5 "Evaluating protected form ~s." ',body)
		   ,@body)
		 (error (err)
		  (dbgformat 0 "Error occurred in protected form ~s." ',body)
		  (dbgformat 0 ,msg err)))))
      ;; unwinding: call terminate hook, if any
      (dbgformat 3 "Unwinding muproc - traps exits ~a: ~a."
		 (muproc-name) (muproc-trap-exits-p))
      (dbgformat 3 "Unwinding muproc - link-assoc ~a."
		 (%with-exclusive-access% *links*))
      (protected "Error occurred unregistering muproc-name: ~a."
		 (unregister-muproc-name (muproc-current-process)))
      (protected "Error occurred unregistering muproc port names: ~a."
		 (unregister-port-names (muproc-current-process)))
      (protected "Error occurred unscheduling timers: ~a."
		 (unschedule-timers))
      (protected "Error occurred unregistering port: ~a."
		 (setf (muproc-port (muproc-current-process)) nil))
      (%with-exclusive-access%
        (setf
         ;; ensure that error occurs if anyone tries
         ;; to send messages to process
         (slot-value *muproc-inport* 'queue) 'muproc-terminated))
        (dbgformat 1 "~a terminating.~%" (muproc-name))
        (signal-linked-muprocs signalling-arg)))

(defun muproc-process-function (inport errorstream initial-bindings start-signal init-fn arglist)
  "The LISP process function, that is `muproc-process-function'
executes in the newly created LISP process.  Sets up for muproc
process, calls the primary muproc function and termination hooks, and
detects and reports errors occurring in the `muproc' process.  NOTE:
Should only be called from `muproc-spawn'."
  (%dequeue% start-signal)
  (setf start-signal nil)
  (progv
      (append '(*muproc-inport*
		*muproc-errorstream*
		*named-ports*
		*pending-input*
		*muproc-packet*
		*muproc-mumsg*
		*timers*)
	      (mapcar #'car initial-bindings))
      (append (list inport
		    errorstream
		    (make-hash-table :test #'eq)
		    (make-pending-input-array)
		    :unbound-value ;; *muproc-packet*
		    :unbound-value ;; *muproc-mumsg*
		    nil)
	      (mapcar #'cdr initial-bindings))
    (let ((signalling-arg))
      (%with-debugging-stack%
	(handler-case ;; for reporting any errors occurring in the run-time system
	    (unwind-protect
		 (handler-case ;; handle error in muproc user code
		     (progn
		       (apply init-fn arglist)
		       (muproc-exit :normal-exit))
		   (muproc-linked-exit-condition (link-term-cond)
		     ;; INV: The muproc is linked to a muproc which is terminating.
		     (dbgformat 1 "Linked exit of ~s due to ~s." (muproc-name) link-term-cond)
		     (setf signalling-arg
			   (list :linked-to-exit (muproc-name (condition-muproc link-term-cond))
				 :for-reason (condition-reason link-term-cond))))
		   (muproc-externally-terminated-condition (term-cond)
		     ;; INV: (muproc-kill ...) was called
		     (dbgformat 0 "~a was terminated due to ~s." (muproc-name) term-cond)
		     (setf signalling-arg (condition-reason term-cond)))
		   (muproc-exit-condition (exit-cond) ;; handling...
		     ;; INV: (muproc-exit ...) was called
		     (dbgformat 1 "Got normal exit condition ~a." exit-cond)
		     (setf signalling-arg (condition-reason exit-cond)))
		   (error (err)
		     (dbgformat 0 "Error occurred in muproc ~a: ~a." (muproc-name) err)
		     (setf signalling-arg (cons :error-in-process err))))
	      (unwind-muproc signalling-arg))
	  (error (err)
	    (muproc-log-errorstream "Internal error in ~a: ~a.~%" (muproc-name) err)))))))

(defun muproc-spawn (name init-fn arglist &key errorstream
		     inport initial-bindings link trap-exits)
  "`muproc-spawn' is used to start new muproc processes.  `name' must
be a symbol, `init-fn' must be the process function, and `arglist'
must be a list matching the lambda list of `init-fn'.  `:errorstream'
must be specified and must be a stream.  `:initial-bindings' can be
used to specify initial bindings for the created process, in which
case it must be a list of conses, with the `car' being the name of the
special variable to be bound, and the `cdr' being a form whose value
when evaluated will be the initial binding of the symbol in the `car'.
`link' indicates whether the submitting process is to be linked to the
submitted process. `trap-exists' indicates whether the newly created
muproc is to trap exits messages from linked muprocs, or receive a
terminating condition signal on linked process exits."
  ;; ERRORSTREAM
  (when (not errorstream)
    (unless (boundp '*muproc-errorstream*)
      (error "Errorstream not specified and muproc:*muproc-errorstream* not bound."))
    (setf errorstream *muproc-errorstream*))
  (unless (typep errorstream 'stream)
    (error "Errorstream not a stream: ~a." errorstream))
  ;; NAME
  (unless (symbolp name)
    (error "Muproc name not a symbol: ~s." name))
  (let ((process-name (format nil "muproc-~d[~a~a]"
			      (next-muproc-id) (if (keywordp name) ":" "") name)))
    (setf inport (or inport (make-instance 'muproc-port :name (format nil "IN<~a>" name))))
    (unless (typep inport 'muproc-port)
      (error "Bad INPORT: ~a not a port." inport))
    (let* ((start-signal-queue (%make-queue%))
           (newproc (%process-run-function% process-name
					    #'muproc-process-function
					    inport errorstream initial-bindings start-signal-queue init-fn arglist)))
      (setf (muproc-port newproc) inport)
      (setf (muproc-lock newproc) (%make-lock%))
      (muproc-set-trap-exits trap-exits newproc)
      (register-muproc-name name newproc)
      (when link
        (muproc-link (muproc-current-process) newproc))
      (%enqueue% start-signal-queue :go) ; Allow the new thread to run
      newproc)))

(defun muproc-p (process)
  "Return T iff `process' is a muproc."
  (and (%process-p% process) (muproc-name process) t))

(defun muproc-current-process ()
  (%current-process%))

;;; REGISTERING NAMES OF MUPROC-PORTS
;;; ============================================================================

(defun muproc-port-name-p (obj)
  "Return T iff `obj' is a valid muproc port name.  Muproc port names
must be keyword symbols."
  (keywordp obj))

(defun muproc-register-port-name (name)
  "Register a muproc port under `name'.  Cf. `muproc-port-name-p' and
`muproc-with-registered-port'."
  (unless (muproc-port-name-p name)
    (error "Not a valid port-name: ~s." name))
  (unless (typep *muproc-inport* 'muproc-port)
    (error "Not a port: ~a." *muproc-inport*))
  (when (muproc-get-registered-port name)
    (error "Port name already registered: ~a." name))
  (%with-exclusive-access%
    (setf (gethash name *registrations*) *muproc-inport*))
  name)

(defun muproc-get-registered-port (name)
  "Get the port registered under `name'."
  (unless (muproc-port-name-p name)
    (error "Not a valid port name: ~s." name))
  (%with-exclusive-access%
    (gethash name *registrations*)))

(defun muproc-unregister-port-name (name)
  "Unregister `name', which must already be registered by the process
trying to unregister.  See also: `muproc-with-registered-port'."
  (unless (muproc-port-name-p name)
    (error "Not a valid port name: ~s." name))
  (unless (typep *muproc-inport* 'muproc-port)
    (error "Not a port: ~a." *muproc-inport*))
  (%with-exclusive-access%
    (let ((reg-port (gethash name *registrations*)))
      (unless reg-port
	(error "Not registered name: ~a." name))
      (unless (eq *muproc-inport* reg-port)
	(error "~a not registered to process ~a."
	       name (muproc-current-process))))
    (remhash name *registrations*)))

(defun unregister-port-names (muproc)
  "Unregister all port names associated with a muproc.  Note: Should
only be used by the muproc run-time system."
  (assert (%process-p% muproc))
  (assert (muproc-port muproc))
  (let ((port (muproc-port muproc)))
    (%with-exclusive-access%
      (loop for name being the hash-keys of *registrations*
	    for val = (gethash name *registrations*)
	    when (eq port val)
	    do (progn
		 (dbgformat 0 "Unreg-muproc: Found ~a under name ~a in ~a."
			    val name (muproc-current-process))
		 (remhash name *registrations*))))))

;;; TIMERS
;;; ============================================================================

(defun muproc-make-interrupt-timer (function &key dont-register)
  "Returns a timer which will interrupt the currently executing muproc
with a call to `function'.  The times is scheduled using
`muproc-schedule-timer' and `muproc-scheduled-timer-relative', and
unscheduled using `muproc-unschedule-timer'.  The timer is
automatically unscheduled when `muproc' terminates."
  (unless (functionp function)
    (error "Not a function: ~s" function))
  (let ((timer (%make-timer% (lambda (mup)
			       (%process-interrupt% mup function))
			     (muproc-current-process))))
    (unless dont-register
      (push timer *timers*))
    timer))

(defun unschedule-timers ()
  (assert (in-muproc-p))
  (when (and *debug-level* (>= *debug-level* 3))
    (loop for timer in *timers*
	  do (dbgformat 3 "Unscheduling timer ~a." timer)))
  (loop for timer in *timers*
	do (muproc-unschedule-timer timer)))

(defun muproc-schedule-timer-relative (timer relative-expiry-time &optional repeat-period)
  "Schedule `timer' to time out at `relative-expiry-time' from the
time of the call, and optionally repeat this each `repeat-time'
(measured in seconds)."
  (if repeat-period
      (%schedule-timer-relative% timer relative-expiry-time repeat-period)
      (%schedule-timer-relative% timer relative-expiry-time)))

(defun muproc-unschedule-timer (timer)
  (assert (in-muproc-p))
  (%unschedule-timer% timer)
  (delete timer *timers*))

(defun muproc-schedule (function universal-time &optional repeat-period)
  "Schedule `function' to run at `universal-time', optionally repeat
every `repeat-period' seconds.  To have more granular control over
scheduling, rescheduling, access to timers etc.,
cf. `muproc-make-interrupt-timer' and `muproc-schedule-timer'."
  (unless (functionp function)
    (error "Not a function: ~s." function))
  (if repeat-period
      (muproc-schedule-timer-relative
       (muproc-make-interrupt-timer function)
       (- universal-time (get-universal-time)) repeat-period)
      (muproc-schedule-timer-relative
       (muproc-make-interrupt-timer function) 
       (- universal-time (get-universal-time)))))

(defun muproc-schedule-relative (function expiry-time &optional repeat-period)
  "Schedule `function' to run `expiry-time' seconds from now,
optionally repeat every `repeat-period' seconds.  To have more
granular control over scheduling, rescheduling, access to timers etc.,
cf. `muproc-make-interrupt-timer' and `muproc-schedule-timer'."
  (unless (functionp function)
    (error "Not a function: ~s." function))
  (if repeat-period
      (muproc-schedule-timer-relative
       (muproc-make-interrupt-timer function)
       expiry-time repeat-period)
      (muproc-schedule-timer-relative
       (muproc-make-interrupt-timer function) 
       expiry-time)))

;;; MUPROC LINKING
;;; ============================================================================

(declaim (inline make-link))
(defun make-link (link-type muproc1 muproc2)
  (assert (find link-type '(:link :monitor)))
  (assert (%process-p% muproc1))
  (assert (%process-p% muproc2))
  (list link-type muproc1 muproc2))

(defun linkp (link)
  (and (consp link)
       (find (first link) '(:monitor :link))
       (%process-p% (second link))
       (%process-p% (third link))))

(defun link= (link1 link2)
  (and (linkp link1)
       (linkp link2)
       (or (and (eq (first link1) :link)
		(eq (first link2) :link)
		(or (and
		     (eq (second link1) (second link2))
		     (eq (third link1)  (third link2)))
		    (and
		     (eq (second link1) (third link2))
		     (eq (third link1)  (second link2)))))
	   (and (eq (first link1) :monitor)
		(eq (first link2) :monitor)
		(eq (second link1) (second link2))
		(eq (third link1)  (third link2))))))

(defun muproc-link (muproc1 &optional (muproc2 (muproc-current-process)))
  "Link two muprocs to each other.  When two muprocs are linked, one
process will be informed if the other terminates, and vice versa.  The
information is conveyed either by posting a MUMSG to the process
\(iff that process traps exits, cf. `muproc-set-trap-exits') or by
signalling a termination condition in the process."
  (assert (%process-p% muproc1))
  (assert (%process-p% muproc2))
  (assert (not (eq muproc1 muproc2)))
  (%with-exclusive-access%
    (pushnew (make-link :link muproc1 muproc2)
	     *links* :test #'link=)))

(defun muproc-monitor (object-muproc &optional (subject-muproc (muproc-current-process)))
  "Make `subject-muproc' monitor `object-muproc', meaning that
`subject-muproc' will be notified if/when `object-muproc'
terminates. Cf. `muproc-link' for details about the notification.  By
default, `subject-muproc' is the currently executing muproc."
  (assert (%process-p% object-muproc))
  (assert (%process-p% subject-muproc))
  (assert (not (eq object-muproc subject-muproc)))
  (%with-exclusive-access%
    (pushnew (make-link :monitor subject-muproc object-muproc)
	     *links* :test #'link=)))

(defun assoc-transitive-closure (cons-assoc start-elem &key (test #'eq))
  "Compute transitive closure over `cons-assoc', using `test' for equality.
`cons-assoc' must be an assoc list consisting of conses, e.g.
'((a . b) (b . c))."
  (let ((nodes)
	(pending (list start-elem)))
    (loop
     do (let ((cur (pop pending)))
	  (unless cur (return nodes))
	  (pushnew cur nodes)
	  (loop for (a . b) in cons-assoc
		when (eq cur a) do (unless (find b nodes :test test)
				     (pushnew b pending :test test))
		when (eq cur b) do (unless (find a nodes :test test)
				     (pushnew a pending :test test)))))))

(defun signal-linked-muprocs (reason)
  (dbgformat 3 "~d signalling linked muprocs for reason ~s"
	     (muproc-name) reason)
  (dbgformat 5 "Links=~s" (%with-exclusive-access% *links*))
  (let ((curproc (muproc-current-process)))
    (labels ((exit-signal-interrupt-fn (+exiting-muproc+ +reason+)
	       (dbgformat 3 "signalling linked muproc ~a for reason ~s"
			  (muproc-current-process)  reason)
	       (assert (typep *muproc-inport* 'muproc-port))
	       ;; INV: we hold a system-wide lock, so it's safe to access these bindings
	       (cond
		 ((and (in-muproc-p) (muproc-trap-exits-p))
		  (port-put *muproc-inport* (mupacket :to NIL
						      :from NIL
						      :type :MUMSG
						      :data (mumsg :terminated +exiting-muproc+
								   :reason +reason+))))
		 (t
		  (port-put *muproc-inport* (wakeup-packet))
		  (signal (make-condition 'muproc-linked-exit-condition
					  :condition-reason +reason+
					  :condition-muproc +exiting-muproc+)))))
	     (signal-it (rcv-muproc exiting-muproc reason)
	       (when (%process-alive-p% rcv-muproc)
		 (handler-case
		     (%process-interrupt% rcv-muproc
					  #'exit-signal-interrupt-fn
					  exiting-muproc
					  reason)
		   (error (err)
		     (dbgformat 0 "Error occurred signaling links: to: ~a re: ~a reason: ~a error: ~a."
				rcv-muproc exiting-muproc reason err))))))
      (let ((to-signal)
	    (new-links))
	(%with-exclusive-access%
	  (loop for (link-type subj obj) in *links*
		do (cond
		     ;; this code could be 'compacted', but it's more
		     ;; readable/maintainable this way...
		     ((and (eq link-type :link) (eq obj curproc))
		      (pushnew subj to-signal))
		     ((and (eq link-type :link) (eq subj curproc))
		      (pushnew obj to-signal))
		     ((and (eq link-type :monitor) (eq obj curproc))
		      (pushnew subj to-signal))
		     ((and (eq link-type :monitor) (eq subj curproc))
		      ;; don't keep - subj will no longer be monitoring
		      (progn))
		     (t	;; keep other links
		      (push (make-link link-type subj obj) new-links)))
		finally (setf *links* new-links)))
	(loop for proc in to-signal
	      do (signal-it proc curproc reason))))))

(defun muproc-kill (muproc reason)
  "Kill `muproc' with `reason'."
  (unless (%process-p% muproc)
    (error "Not a process: ~a." muproc))
  (unless (%process-alive-p% muproc)
    (warn "MUPROC: Tryning to kill process which is not alive.")
    (return-from muproc-kill nil))
  (flet ((kill-interrupt-fn (+muproc+ +reason+)
	   ;; TODO: Be able to deliver an EXIT message, with a
	   ;;       timeout which forcefully terminates the process.
	   (signal (make-condition 'muproc-externally-terminated-condition
				   :condition-reason +reason+
				   :condition-muproc +muproc+))))
    (%process-interrupt% muproc #'kill-interrupt-fn muproc reason))
  reason)

;;; FINDING A MUPROC BY NAME (depends on cl-ppcre)
;;; ============================================================================

(defgeneric muproc-find (criteria &key as-list-p)
  (:documentation "Find muprocs whose names matches the CL-PPCRE
expression `regex'.  Iff exactly one such muproc is found, it is
returned. If more than one is found, and `as-list-p' is not NIL, the
list of muprocs is returned.  If no muproc matching `regex' is found,
NIL is returned.")
  #+cl-ppcre
  (:method ((regex string) &key as-list-p)
    (let ((procs (loop for p in (muproc-all-processes)
		       when (and (muproc-p p)
				 (cl-ppcre:scan regex (symbol-name (muproc-name p))))
		       collect p)))
      (when (and (> (length procs) 1) (not as-list-p))
	(error "Muproc search regex ~s matches more than one process: ~a." regex procs))
      (if (or (> (length procs) 1) as-list-p)
	  procs
	  (first procs))))
  (:method ((sym symbol) &key as-list-p)
    (let ((m (%with-exclusive-access%
	       (gethash sym *names*))))
      (if as-list-p
	  (list m)
	  m))))

;;; RESET (INTERNAL)

(defun reset ()
  ;; INTERNAL FUNCTION
  ;; DO NOT CALL UNLESS YOU KNOW WHAT YOU ARE DOING!
  (%with-exclusive-access%
    (clrhash *names*)
    (clrhash *ports*)
    (clrhash *registrations*)
    (setf *links* nil)))
  


;;eof