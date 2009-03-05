;; -*- lisp -*-
;; See the file LICENCE for licence information.

(in-package :it.bese.ucw)

(defmacro with-lock-held-on-application (app &body body)
  (rebinding (app)
    `(prog2
      (ucw.rerl.ajax.dribble "Entering with-lock-held-on-application for app ~S in thread ~S" ,app (current-thread))
      (with-recursive-lock-held ((lock-of ,app))
        ,@body)
      (ucw.rerl.ajax.dribble "Leaving with-lock-held-on-application for app ~S in thread ~S" ,app (current-thread)))))

(defmacro with-lock-held-on-current-application (&body body)
  `(with-lock-held-on-application (context.application *context*)
    ,@body))

(defmacro with-lock-held-on-session (session &body body)
  (rebinding (session)
    `(prog2
      (ucw.rerl.ajax.dribble "Entering with-lock-held-on-session for ~S in thread ~S" ,session (current-thread))
      (with-recursive-lock-held ((lock-of ,session))
        ,@body)
      (ucw.rerl.ajax.dribble "Leaving with-lock-held-on-session for ~S in thread ~S" ,session (current-thread)))))

(defmacro with-session-variables ((&rest names) &body body)
  "Create local bindings for the listed NAMES and set them to
\(session.value ',name session\). If BODY gracefully completes then
save the values of the local variables back into the session."
  (with-unique-names (session)
    `(let ((,session (context.session *context*)))
      (let (,@(iter (for name in names)
                    (collect `(,name (session.value ',name ,session)))))
        ,@body
        ,@(iter (for name in names)
                (collect `(setf (session.value ',name ,session) ,name)))))))

