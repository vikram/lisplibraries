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


(in-package :cl-muproc.supervisor)

;;; MACROS
;;; ============================================================================

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defparameter *debug-level* 0
    "Integer indicating level of debug output, the higher the number
the more debug output."))

(defmacro dbgformat (level &rest args)
  "Log `args' with `muproc-log-errorstream' iff `*muproc-dedbgformat-level*'
      is not NIL and `level' >= `*muproc-dedbgformat-level*'."
  `(when (and *debug-level* (>= *debug-level* ,level))
    (muproc-log-errorstream (format nil "SUPERVISOR(~a): ~a" ,level
			     (format nil ,@args)))))

(defmacro define-passthrough-methods ((class pt-reader-fn) &rest fns)
  (let ((+var+ (gensym)))
    `(progn
      ,@(loop for fn in fns
	      collect `(defmethod ,fn ((,+var+ ,class))
			(,fn (,pt-reader-fn ,+var+)))))))

(defmacro supervised-muproc (name function arglist)
  "Creates and returns a `muproc-start-spec' which is normally used in
`supervisor' 'spec' forms."
  `(make-instance 'muproc-start-spec
    :name ',name
    :muproc-function ,function
    :muproc-arglist ,arglist))

(defmacro supervisor (policy termination-count termination-count-period &rest specs)
  "Creates a `supervisor-spec' which is typically given to
`supervisor-start'.  `policy' must be either `:all-for-one' or
`:one-for-one', where the former means that the supervisor will
terminates all children (known as SUPERVISEEs) if one terminates, and
the latter means that the supervisor will simply restart a child if it
terminates.  `termination-count' how many terminations are permitted
with `termination-period' seconds before the supervisor should stop
restarting its children at terminate itself.  `specs' are instances of
subclasses of START-SPEC, either SUPERVISOR-START-SPEC (created using
`supervisor') or MUPROC-START-SPEC (created using `supervised-muproc',
cf. this)."
  (let ((+child-specs+ (gensym "CH-SPECS-"))
	(+child-types+ (gensym "CH-TYPES-"))
	(+child-meths+ (gensym "CH-METHS-"))
	(+spec-inst+ (gensym "SPEC-INST-")))
    `(let (,+child-specs+ ,+child-types+ ,+child-meths+)
      ,@(loop for spec in specs
	      unless (and (consp spec) (= (length spec) 3))
	      do (error "Invalid supervisee start-spec: ~s." spec)
	      collect (destructuring-bind (type meth start-spec) spec
			`(let ((,+spec-inst+ ,start-spec))
			  (push ,+spec-inst+ ,+child-specs+)
			  (push (cons (name ,+spec-inst+) ,type) ,+child-types+)
			  (push (cons (name ,+spec-inst+) ,meth) ,+child-meths+))))
      (make-instance 'supervisor-start-spec
	:restart-policy ,policy
	:term-count ,termination-count
	:term-period ,termination-count-period
	:child-specs (nreverse ,+child-specs+)
	:child-types (nreverse ,+child-types+)
	:child-meths (nreverse ,+child-meths+)))))

;;; TYPES
;;; ============================================================================

(defun restart-policy-checker (obj)
  (and (member obj '(:one-for-one :all-for-one)) t))
(deftype restart-policy () '(satisfies restart-policy-checker))

(defun supervisee-type-checker (obj)
  (and (member obj '(:permanent :transient :temporary)) t))
(deftype supervisee-type () '(satisfies supervisee-type-checker))

(defun terminate-method-checker (obj)
  "Either `:kill', `:wait-forever' or a positive number
indicating the period to wait before killing the muproc."
  (and (or (member obj '(:kill :wait-forever))
	   (and (numberp obj) (plusp obj)))
       t))
(deftype terminate-method () '(satisfies terminate-method-checker))

(defun supervisor-state-checker (obj)
  (and (member obj '(:up nil :down :finished)) t))
(deftype supervisor-state () '(satisfies supervisor-state-checker))

(defun supervisee-state-checker (obj)
  (and (member obj '(:alive :dead :shutting-down :dying :finished)) t))
(deftype supervisee-state () '(satisfies supervisee-state-checker))

(defun muproc-function-checker (obj)
  (and (or (functionp obj)
	   (and (symbolp obj) (fboundp obj)))
       t))
(deftype muproc-function () '(satisfies muproc-function-checker))

(defun muproc-arglist-checker (obj)
  (and (or (listp obj)
	   (functionp obj)
	   (and (symbolp obj) (fboundp obj)))
       t))
(deftype muproc-arglist () '(satisfies muproc-arglist-checker))

;;; GLOBALS
;;; ============================================================================

(defparameter *verify-period* 15) ;; fix-this: arbitrary
(defparameter *dying-period*  15) ;; fix-this: arbitrary

(defvar *state*)		    ; bound only in supervisor muprocs


;;; START-SPEC
;;; ============================================================================

(defclass start-spec ()
  ((name            :initarg :name
		    :reader name
		    :documentation "The name of the supervision node.  Given as ~
                                    argument to `muproc-spawn'.")))

(defclass supervisor-start-spec (start-spec)
  ((restart-policy  :initarg :restart-policy
		    :reader restart-policy
		    :documentation "How to react when terminations occur.  ~
                                    Either `:one-for-one' or `:all-for-one'.")
   (term-count      :initarg :term-count
		    :reader term-count
		    :documentation "How many terminations to allow during the ~
                                    period specified in slot `term-period'.")
   (term-period     :initarg :term-period
		    :reader term-period
		    :documentation "What period (measured in seconds) to consider ~
                                    when deciding if 'too many' terminations have ~
                                    occurred. The value of 'too many' is given in ~
                                    slot `term-count'.")
   (child-specs     :initarg :child-specs
		    :initform nil
		    :reader child-specs)
   (child-types     :initarg :child-types
		    :initform nil
		    :reader child-types)
   (child-meths     :initarg :child-meths
		    :initform nil
		    :reader child-meths)))

(defmethod initialize-instance :around ((sup supervisor-start-spec) &rest init-args)
  (let ((pol (getf init-args :restart-policy))
	(count (getf init-args :term-count))
	(period (getf init-args :term-period))
	(specs (getf init-args :child-specs)))
    (unless (typep pol 'restart-policy)
      (error "Not a valid restart policy: ~s." pol))
    (unless (and (integerp count) (plusp count))
      (error "Not a valid termination count: ~s" count))
    (unless (and (numberp period) (plusp period))
      (error "Not a valid termination period: ~s." period))
    (unless specs
      (error "A supervisor needs at least one child to make sense."))
    (unless (every (lambda (s) (typep s 'start-spec)) specs)
      (error "Child specs must be instances of subclasses START-SPEC : ~s." specs))
    (call-next-method)
    (setf (slot-value sup 'name)
	  (intern (format nil (ecase (restart-policy sup)
				(:all-for-one "{~{~a~^*~}}")
				(:one-for-one "{~{~a~^+~}}"))
			  (mapcar #'name (child-specs sup)))))))

(defmethod print-object ((spec supervisor-start-spec) stream)
  (print-unreadable-object (spec stream)
    (format stream "SUPERVISOR-SPEC ~s ~a:~a:~a"
	    (slot-value spec 'name)
	    (slot-value spec 'restart-policy)
	    (slot-value spec 'term-count)
	    (slot-value spec 'term-period))))

(defclass muproc-start-spec (start-spec)
  ((function        :initarg :muproc-function
		    :reader muproc-function
		    :documentation "The muproc function to start the child.  Given as ~
                                    argument to `muproc-spawn'.")
   (arglist         :initarg :muproc-arglist
		    :reader muproc-arglist
		    :documentation "The arglist to give to function in slot ~
                                    `function'.  Given as arglist to `muproc-spawn'.")))

(defmethod initialize-instance :around ((mss muproc-start-spec) &rest init-args)
  (let ((function (getf init-args :muproc-function))
	(arglist (getf init-args :muproc-arglist))
	(name (getf init-args :name)))
    (unless (typep function 'muproc-function)
      (error "Not a valid muproc-function: ~s." function))
    (unless (typep arglist 'muproc-arglist)
      (error "Not a valid muproc arglist: ~s" arglist))
    (unless (muproc-name-p name)
	(error "Not a valid muproc name: ~s." name))
    (call-next-method)))

(defmethod print-object ((spec muproc-start-spec) stream)
  (print-unreadable-object (spec stream)
    (format stream "MUPROC-SUPERVISEE-SPEC ~s ~a ~a"
	    (slot-value spec 'name)
	    (slot-value spec 'function)
	    (slot-value spec 'arglist))))

;;; SUPERVISION-NODE
;;; ============================================================================

(defclass supervision-node ()
  ((start-spec      :initarg :start-spec
		    :reader start-spec)
   (target-state    :initarg :target-state
		    :accessor target-state)))

(defmethod initialize-instance :around ((s supervision-node) &rest init-args)
  (let ((spec (getf init-args :start-spec)))
    (unless (typep spec 'start-spec)
      (error "Not valid start spec: ~s." spec)))
  (call-next-method))

;;; SUPERVISOR
;;; ============================================================================

(defclass supervisor (supervision-node)
  ((error-timestamps)
   (error-index     :initform 0)
   (supervisees     :initarg :supervisees
		    :accessor supervisees)))

(defmethod initialize-instance :around ((s supervisor) &rest init-args)
  (let ((children (getf init-args :supervisees)))
    (unless (every (lambda (s) (typep s 'supervisee)) children)
      (error "All children not instances of SUPERVISEE class: ~s."
	     children)))
  (setf (target-state s) :up)
  (call-next-method)
  (assert (and (numberp (term-count s))
	       (plusp (term-count s))))
  (assert (and (numberp (term-period s))
	       (plusp (term-period s))))
  (setf (slot-value s 'error-timestamps)
	(make-array (term-count s)
		    :initial-element 0
		    :element-type 'integer)))

;; wrt. `print-object' for supervisor: see further down

(define-passthrough-methods (supervisor start-spec)
    name restart-policy term-count term-period child-specs)

(defgeneric register-error-occurrence (sup)
  (:documentation "Register the occurrence of an error and check that
it is not 'one error too many' wrt. the `term-count' and `term-period'
of the supervisor.  Returns T iff the occurrence of the error is 'one
too many'.")
  (:method ((sup supervisor))
    (with-slots (error-timestamps error-index) sup
      (assert (= (length (array-dimensions error-timestamps)) 1))
      (assert (= (first (array-dimensions error-timestamps))
		 (term-count sup)))
      (let* ((term-period (* internal-time-units-per-second
			     (term-period sup)))
	     (size (term-count sup))
	     (now (get-internal-real-time))
	     (last-index (mod (1+ error-index) size))
	     (last (elt error-timestamps last-index)))
	(setf (elt error-timestamps error-index) now
	      error-index (mod  (1+ error-index) size))
	(< (- now last) term-period)))))

;;; SUPERVISEE
;;; ============================================================================

(defclass supervisee (supervision-node)
  ((current-state   :initform :dead)
   (supervisee-type :initarg :supervisee-type
		    :reader supervisee-type
		    :documentation "One of `:permanent', `:transient', or ~
                                    `:temporary'.  Determines what happens when ~
                                    a children terminates with reason `:normal'.")
   (term-method     :initarg :term-method
		    :reader term-method
		    :documentation "One of `:kill', `:wait-forever', or a ~
                                    positive number indicating the period
                                    (measured in seconds) to wait before ~
                                    killing the muproc.")
   (muproc          :initform nil
		    :accessor supervisee-muproc
		    :documentation "Holds a reference to the muproc representing this ~
                                    supervisee at any time.")))

(defmethod initialize-instance :around ((s supervisee) &rest init-args)
  (let ((type (getf init-args :supervisee-type))
	(term (getf init-args :term-method)))
    (unless (typep type 'supervisee-type)
      (error "Not valid supervisee type: ~s." type))
    (unless (typep term 'terminate-method)
      (error "Not valid supervisee termination method: ~s." term))
    (setf (target-state s) :dead)
    (call-next-method)))

(defmethod print-object ((spec supervisee) stream)
  (print-unreadable-object (spec stream :type t)
    (format stream "~s ~a:~a ~a:~a"
	    (name spec) ;; comes from start-spec
	    (supervisee-type spec)
	    (term-method spec)
	    (current-state spec)
	    (target-state spec))))

(define-passthrough-methods (supervisee start-spec)
    name muproc-function muproc-arglist)

;;; OPERATORS
;;; ============================================================================

(declaim (inline singletonp))
(defun singletonp (o)
  (and (consp o) (null (cdr o))))

(defgeneric state (a &optional b)
  (:method ((sn supervision-node) &optional dummy)
    (assert (null dummy))
    (cons (current-state sn) (target-state sn)))
  (:method (a &optional b)
    (cons a b)))

(defun state= (a b)
  (assert (and (consp a) (consp b)))
  (unless (every (lambda (o) (or (keywordp o) (null o)))
		 (list (car a) (cdr a)
		       (car b) (cdr b)))
    (error "Not all keywods ~s and ~s." a b))
  (and (eq (car a) (car b))
       (eq (cdr a) (cdr b))))

(let ((state-list (list (state :alive :alive)
			(state :dead  :dead)
			(state :finished :finished))))
  (defgeneric stablep (sn)
    (:method ((vs supervisee))
      (and (member (state vs) state-list :test #'state=) t))
    (:method ((vz supervisor))
      (every #'stablep (supervisees vz)))
    (:documentation "Indicates that all either supervisor or supervisee ~
		     are in stable condition, i.e. a supervisee is either ~
		     `:dead', `:finished', or `:alive' and for a supervisor ~
		     it holds that all its children are `stablep'.  When a ~
		     supervisor is `stablep', it is ready to either restart ~
		     children or terminate itself.")))

;; wrt. placement in file: needs stablep
(defmethod print-object ((vz supervisor) stream)
  (print-unreadable-object (vz stream :type t)
    (format stream "~a ~a:~a:~a ~a:~a ~a"
	    (name vz)  ;; comes from start-spec
	    (restart-policy vz)
	    (term-count vz)
	    (term-period vz)
	    (current-state vz) ;; function of current-state of supervisees
	    (target-state vz)
	    (if (stablep vz) "STABLE" "UNSTABLE"))))

(defun check-supervisees (vz)
  (loop for vs in (supervisees vz)
	when (and (eq (current-state vs) :alive)
		  (not (%process-alive-p% (supervisee-muproc vs))))
	do (muproc-log-errorstream "Muproc dead but marked as alive: ~s." vs)))

(defun find-dependents (vz ch)
  (assert (typep vz 'supervisor))
  (assert (typep ch 'supervisee))
  (ecase (restart-policy vz)
    (:one-for-one nil)
    (:all-for-one (remove-if (lambda (p) (eq p ch))
			     (supervisees vz)))))

(defgeneric find-supervisee (vz term)
  (:method ((vz supervisor) muproc)
    (assert (%process-p% muproc))
    (loop for ch in (supervisees vz)
	  when (eq (supervisee-muproc ch) muproc)
	  return ch))
  (:method ((vz supervisor) (name symbol))
    (loop for ch in (supervisees vz)
	  when (eq (name ch) name)
	  return ch)))

(defun ensure-supervisee (term &optional (vz *state*))
  (let ((ch (find-supervisee vz term)))
    (unless (typep ch 'supervisee)
      (error "Unable to locate supervisee from ~s." term))
    ch))

;;; STATE
;;; ============================================================================

(defgeneric current-state (sn)
  (:method ((vs supervisee))
    (slot-value vs 'current-state))
  (:method ((vz supervisor))
    (loop with alive = 0
	  with dead = 0
	  with finished = 0
	  with other = 0
	  with total = 0
	  for vs in (supervisees vz)
	  do (incf total)
	  do (case (current-state vs)
	       (:alive     (incf alive))
	       (:dead      (incf dead))
	       (:finished  (incf finished))
	       (otherwise  (incf other)))
	  finally (return
		    ;; notice that the number of :finished supervisees
		    ;; does not affect current-state of supervisor
		    (cond
		      ((= total finished) :finished)
		      ((and (plusp alive) (zerop dead)  (zerop other)) :up)
		      ((and (plusp dead)  (zerop alive) (zerop other)) :down)
		      (t nil))))))

(defgeneric (setf current-state) (newvalue vs)
  (:method (newvalue (vs supervisee))
    (setf (slot-value vs 'current-state) newvalue)))

(defgeneric new-current-state (vs state)
  ;; current-state cannot be set for supervisor because it is a
  ;; function of the state if it supervisees, cf. `current-state'
  (:method ((vs supervisee) state)
    (assert (typep state 'supervisee-state))
    (dbgformat 4 "NEW STATE ~s for ~a (was ~s)."
	       state (name vs) (current-state vs))
    (setf (current-state vs) state)))

(defgeneric new-target-state (sn state)
  (:method ((vs supervisee) state)
    (dbgformat 4 "NEW TARGET ~s for ~a (was ~s)."
	       state (name vs) (target-state vs))
    (assert (typep state 'supervisee-state))
    (setf (target-state vs) state))
  (:method ((vz supervisor) state)
    (dbgformat 4 "NEW SUPERVISOR TARGET ~s for ~a (was ~s)."
	       state (name vz) (target-state vz))
    (assert (typep state 'supervisor-state))
    (setf (target-state vz) state)))

(defun target-state-p (sn &rest states)
  (assert (typep sn 'supervision-node))
  (find (target-state sn) states :test #'eq))

(defun current-state-p (sn &rest states)
  (assert (typep sn 'supervision-node))
  (find (current-state sn) states :test #'eq))

;;; TIMEOUTS
;;; ============================================================================

(defun gen-event (&rest args)
  (dbgformat 4 "Generating event: ~s." args)
  (apply #'mumsg-send *muproc-inport* args))

(defun schedule-timeout (obj period)
  (let* ((placeholder)
	 (timer (muproc-make-interrupt-timer
		 (lambda () (gen-event :timeout obj
				       :timer placeholder)))))
    ;; This is a somewhat roundabout way of incorporating the timer in
    ;; the interrupt, but we need it in the packet because we need
    ;; have a registered timer and the same time we also need to
    ;; unschedule it to make sure we do not just accumulate timers in
    ;; the supervisor's timer registry
    (setf placeholder timer)
    (muproc-schedule-timer-relative timer period)))

;;; STARTING AND STOPPING SUPERVISEES
;;; ============================================================================

(defun just-start (vs)
  (dbgformat 3 "Starting ~s." vs)
  (new-current-state vs :alive)
  (setf (supervisee-muproc vs)
	(etypecase (start-spec vs)
	  (supervisor-start-spec 
	   (supervisor-start (start-spec vs)
			     :link t))
	  (muproc-start-spec
	   (dbgformat 5 "MUPROC-FUNCTION: ~s" (muproc-function vs))
	   (dbgformat 5 "MUPROC-ARGLIST: ~s" (muproc-arglist vs))
	   (muproc-spawn
	    (name vs)
	    (let ((f (muproc-function vs)))
	      (cond
		((functionp f) f)
		((and (symbolp f) (fboundp f))
		 (funcall (symbol-function f)))
		(t (error "Invalid muproc function: ~s." f))))
	    (let ((al (muproc-arglist vs)))
	      (cond
		((functionp al) 
		 (dbgformat 5 "FUNCALLING ARGLIST")
		 (funcall al))
		((and (symbolp al) (fboundp al))
		 (dbgformat 5 "FUNCALLING SYMBOL-FUNCTION ARGLIST")
		 (funcall (symbol-function al)))
		((listp al) 
		 (dbgformat 5 "REGULAR ARGLIST: ~s" al)
		 al)
		(t (error "Invalid worker-arglist: ~s." al))))
	    :link t)))))

(defun just-kill (vs)
  (cond
    ((%process-alive-p% (supervisee-muproc vs))
     (dbgformat 3 "Killing ~s." vs)
     (new-current-state vs :dying)
     (schedule-timeout (supervisee-muproc vs) *dying-period*)
     (ignore-errors
       (muproc-kill (supervisee-muproc vs) :supervisor-kill)))
    (t
     ;; We do not do anything if (muproc vs) is no longer alive-p,
     ;; because we'll get a (TERMINATED REASON) event on it later and
     ;; we'll update its state at that point in time
     (dbgformat 3 "NOT killing ~s -- it has already terminated." vs))))

(defun just-shutdown (vs &optional period)
  (cond
    ((%process-alive-p% (supervisee-muproc vs))
     (dbgformat 3 "Shutting down ~s (period ~s)." vs period)
     (new-current-state vs :shutting-down)
     (when period
       (schedule-timeout (supervisee-muproc vs) period))
     (ignore-errors
       (muproc-exit :supervisor-shutdown (supervisee-muproc vs))))
    (t
     ;; We do not do anything otherwise because we'll get
     ;; a TERMINATED event on this process at some point
     ;; and we'll update the state at that point in time
     (dbgformat 3 "NOT shutting down ~s -- it has already terminated." vs))))

(defun terminate (vs)
  (let ((meth (term-method vs)))
    (cond
      ((eq meth :kill)
       (just-kill vs))
      ((eq meth :wait-forever)
       (just-shutdown vs))
      ((numberp meth)
       (just-shutdown vs meth))
      (t (error "Internal error: Unknown termination method: ~s." meth)))))

;;; STARTING & STOPPING SUPERVISOR
;;; ============================================================================

(defun supervisor-start (supervisor-spec &key link errorstream)
  "Starts and returns a new SUPERVISOR which runs according to
`supervisor-spec'.  A `supervisor-spec' is normally created using 
`supervisor'."
  (unless (typep supervisor-spec 'supervisor-start-spec)
    (error "Not an instance of SUPERVISOR-START-SPEC: ~s."
	   supervisor-spec))
  (muproc-spawn (name supervisor-spec)
		#'supervisor-loop
		(list supervisor-spec)
		:errorstream errorstream
		:link link
		:trap-exits t
		:initial-bindings  '((*state* . nil))))

;;; PERFORM COMMAND
;;; ============================================================================

(defgeneric perform-command (cmd &rest args)
  (:method ((cmd (eql :exit)) &rest args)
    (dbgformat 3 "PERFORM - EXIT ~s." args)
    (assert (= (length args) 1))
    (muproc-exit (first args)))
  (:method ((cmd (eql :nop)) &rest args)
    (dbgformat 3 "PERFORM - NOOP: ~s." args))
  (:method ((cmd (eql :start)) &rest args)
    (dbgformat 3 "PERFORM - START: ~s." args)
    (assert (singletonp args))
    (let ((vs (first args)))
      (if (current-state-p vs :alive)
	  (dbgformat 3 "PERFORM - START COMMAND SKIPPED.")
	  (just-start vs))))
  (:method ((cmd (eql :terminate)) &rest args)
    (dbgformat 3 "PERFORM - TERMINATE: ~s." args)
    (assert (singletonp args))
    (let ((vs (first args)))
      (if (current-state-p vs :dead :finished)
	  (dbgformat 3 "PERFORM - TERMINATE COMMAND SKIPPED.")
	  (terminate vs)))))

;;; GENERATE COMMANDS
;;; ============================================================================

(defun generate-commands (vz)
  (dbgformat 4 "GEN - VZ state BEFORE actions: ~s" vz)
  (let (cmds)
    (flet ((cmd (&rest args)
	     (assert (and (consp args) (keywordp (car args))))
	     (push args cmds)))
      (block done
	(when (eq (current-state vz) :finished)
	  (cmd :exit :normal)
	  (return-from done))
	(ecase (target-state vz)
	  (:up
	   ;; do not generate cmds if everything is as we want it:
	   (unless (eq (current-state vz) (target-state vz))
	     ;; do not effect changes until no more asynchronous
	     ;; events are expected:
	     (when (stablep vz)
	       (loop for vs in (supervisees vz)
		     unless (current-state-p vs :finished)
		     ;; create pre-condition for vs being started:
		     do (new-target-state vs :alive)))
	     (loop for vs in (supervisees vz)
		   do (case (target-state vs)
			(:dead       (cmd :terminate vs))
			(:finished   (cmd :terminate vs))
			(:alive      (cmd :start vs))))))
	  (:down
	   (ecase (current-state vz)
	     ((:up nil) (loop for vs in (supervisees vz)
			      do (cmd :terminate vs)))
	     (:down (cmd :exit :normal)))))))
    (nreverse cmds)))



;;; UPDATE STATE
;;; ============================================================================

(defgeneric update-state (vs ev &optional vz reason)
  (:method :before (vs ev &optional vz reason)
	   (dbgformat 1 "EVENT: ~s ~s ~s" ev vs reason))
  (:method ((vs (eql nil)) (eq (eql :shutdown-supervisor)) &optional (vz *state*) reason)
    (dbgformat 2 "UPDATE - SHUTDOWN SUPERVISOR")
    (new-target-state vz :down)
    (loop for vs in (supervisees vz)
	  when (target-state-p vs :alive)
	  do (new-target-state vs :dead)))
  (:method ((vs supervisee) (ev (eql :has-terminated )) &optional (vz *state*) reason)
    (dbgformat 2 "UPDATE - HAS-TERMINATED: ~s." vs)
    (when (target-state-p vs :alive)
      ;; it is not terminating because we asked
      (when (register-error-occurrence *state*)
	(muproc-exit :one-error-too-many))
      (dbgformat 3 "UPDATE - DEPENDENTS=~{~a~^, ~}"
		 (mapcar #'name (find-dependents vz vs)))
      (loop for dep in (find-dependents vz vs)
	    ;; we don't touch :finished supervisees
	    unless (target-state-p dep :finished)
	    do (new-target-state dep :dead)))
    (let ((ns (if (or (eq (supervisee-type vs) :temporary)
		      (and (eq (supervisee-type vs) :transient)
			   (eq reason :normal)))
		  :finished :dead)))
      (new-current-state vs ns)
      (new-target-state vs ns)))
  (:method ((vs supervisee) (ev (eql :timeout)) &optional (vz *state*) placeholder)
    ;; `placeholder' is undefined for this event type
    (dbgformat 2 "UPDATE - TIMEOUT: ~s." vs)
    (case (current-state vs)
      (:shutting-down (new-target-state vs :dying))
      (:dying         (error "Muproc kill timed out for ~s (muproc ~s)."
			     (name vs) (supervisee-muproc vs)))
      (otherwise      (values)))))



;;; SUPERVISOR LOOP
;;; ============================================================================

(defun make-supervisor (spec)
  (make-instance 'supervisor
    :start-spec spec
    :supervisees (loop for s in (child-specs spec)
		       for name = (name s)
		       collect (make-instance 'supervisee
				 :supervisee-type (cdr (assoc name (child-types spec)))
				 :term-method (cdr (assoc name (child-meths spec)))
				 :start-spec s))))

(defun supervisor-loop (spec)
  (dbgformat 1 "SUPERVISOR ~s starting." spec)
  (setf *state* (make-supervisor spec))
  (muproc-schedule-timer-relative
   (muproc-make-interrupt-timer (lambda ()
				  (gen-event :verify-state nil)))
   *verify-period* *verify-period*)
  (unwind-protect
       (handler-case
	   (loop
	    (dbgformat 1 "---------------LOOP-TOP---------------")
	    (dbgformat 1 "MAIN - VZ: ~s" *state*)
	    (loop for vs in (supervisees *state*)
		  do (dbgformat 2 "  VS ~s." vs))
	    (let ((cmds (generate-commands *state*)))
	      (dbgformat 2 "MAIN - Actions ~{~s~^, ~}" cmds)
	      (loop for cmd in cmds
		    do (apply #'perform-command cmd)))
	    (mumsg-receive (from)
	      ((terminated reason) (find-supervisee *state* terminated)
	       (dbgformat 3 "MAIN - Got TERMINATED ~a with REASON ~a." terminated reason)
	       (update-state (ensure-supervisee terminated) :has-terminated *state* reason))
	      ((terminated reason) (not terminated)
	       (dbgformat 3 "MAIN - Got exit signal.")
	       (update-state nil :shutdown-supervisor))
	      ((terminated reason) t
	       (muproc-log-errorstream "Got TERMINATED signal from unknown: ~s." terminated)
	       (loop for ch in (supervisees *state*)
		     do (dbgformat 3 "  VS: ~s" ch)))
	      ((timeout timer) t
	       (dbgformat 3 "MAIN - Got timeout ~a from timer ~a." timeout timer)
	       (assert (%process-p% timeout))
	       (muproc-unschedule-timer timer)
	       (let ((vs (find-supervisee *state* timeout)))
		 (if vs
		     (update-state vs :timeout)
		     (dbgformat 3 "MAIN - Got timeout from obsolete muproc."))))
	      ((verify-state) t
	       (dbgformat 3 "MAIN - Got verify-state")
	       (check-supervisees *state*))
	      (() t
	       (muproc-log-errorstream "Unexpected packet: ~s." *muproc-packet*))))
	 (error (err)
	   (muproc-log-errorstream "Internal error in supervisor: ~a." err)
	   (muproc-log-errorstream "Supervisor start: ~s." *state*)
	   (loop for w in (supervisees *state*)
		 do (muproc-log-errorstream "  VS: ~s." w))))
    ;; forcefully kill supervisees that haven't terminated
    ;; if supervisor is forced to terminate
    (loop for vs in (supervisees *state*)
	  do (just-kill vs))
    (muproc-log-errorstream "SUPERVISOR ~s terminating." *state*)))
