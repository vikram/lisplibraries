;;;; -*- lisp -*-

(in-package :it.bese.ucw)

;;;; Implementation of component control flow. NB: This file defines
;;;; the methods goto-component and call-component, the user macros
;;;; CALL and ANSWER (which are defined in terms of these methods) are
;;;; located in the standard-actions file.

(defmethod call-request-path ((component standard-component))
  (let ((seen '()))
    (loop
       for parent = component then (parent parent)
       while parent
       when (member parent seen)
         do (warn "Circular parent loop starting at ~S." component)
         and do (return (nreverse path))
       do (push parent seen)
       collect parent into path
       finally (return (nreverse path)))))

(defmethod/cc call-component ((from standard-component) (to standard-component))
  "Transfer control from FROM to the component TO.

This method grabs the current continuation (and so doesn't return
normally) passes it to TO and puts TO in FROM's place (by
modifying FROM's component.place).

This method does not return normally but simply stops the
execution of the current action and returns when TO answers."
  (let/cc k
    (setf (component.continuation to) k
          (component.calling-component to) from
          (component.place to) (component.place from)
	  (parent to) (parent from)
          (place (component.place from)) to)
    (ucw.component.info (format nil "CALL'ing to ~{/~A~} " (call-request-path to)))
    ;; don't continue, just return the TO component.
    to))

(defmethod/cc call-component ((from null) (to standard-component))
  "Passes control to the TO when there is no current (FROM)
component. This method is called in dispatchers, actions should
always have a FROM component."
  (let/cc k
    (setf (component.calling-component to) nil
          (component.place to) (make-place (context.window-component *context*))
          (component.continuation to) k
          (context.window-component *context*) to)
    (ucw.component.info (format nil "CALL'ing to ~{/~A~} " (call-request-path to)))
    to))

(defmethod was-called-p ((component standard-component))
  (and (slot-boundp component 'calling-component)
       (component.calling-component component)))

(defmethod answer-component ((target standard-component) value)
  (answer-component* (component.calling-component target) target value))

(defmethod answer-component* :before ((source standard-component) (target standard-component) value)
  (ucw.component.info "(ANSWER-COMPONENT* ~{/~A~} ~{/~A~} ~S)"
                      (call-request-path source)
                      (call-request-path target)
                      value))

(defmethod answer-component* ((source standard-component) (target standard-component) value)
  "Return control from TARGET to SOURCE, returning VALUE from the calling call form.

Calling this method causes the calling CALL form to return and
the action to continue."
  (setf (place (component.place target)) source
        (parent target) nil
        (component.calling-component target) nil)
  (kall (component.continuation target) value))

(defmethod answer-component* ((source null) (target standard-component) value)
  (setf (parent target) nil)
  (kall (component.continuation target) value))

(defmethod jump-to-component ((target standard-component))
  "Transfer control of the window to TARGET.

This method, unlike call-component, drops the backtrack and
component stack and clears the session table. TARGET will be
placed in the current frame's window-component (and so sholud be
a window-component or similar). Using the backbutton or accessing
previously cloned windows will no longer work."
  (setf (component.place target) (make-place (context.window-component *context*))
        (context.window-component *context*) target)
  (let ((this-frame (context.current-frame *context*))
        (this-session (context.session *context*)))
    ;; cleare out this frame so it contains no refernces to previous
    ;; stuff.
    (clrhash (frame.actions this-frame))
    (clrhash (frame.callbacks this-frame))
    (setf (frame.backtracks this-frame) '())
    ;; now drop everything in the session (except the current frame)
    (clrhash (session.object-pool this-session))
    (loop
       until (queue-empty-p (session.frames this-session))
       do (dequeue (session.frames this-session))
       finally (enqueue (session.frames this-session) this-frame))
    target))

;;;; Default component actions

(defgeneric/cc refresh-component (component)
  (:documentation "This generic action is should simply redraw COMPONENT without performing any action.

It can be used as the :action parameter to forms which have
multiple submit buttons and we don't want hitting the enter key
to call one of the actions."))

(defaction refresh-component ((c component))
  nil)

(defgeneric/cc ok (component &optional value)
  (:documentation "This generic action causes component to answer.

The answer macro can only appear within the body of defaction
methods, to avoid the creation of spurious actions this action
can be used whenever we want to cause a compoent ot answer.

VALUE should default to COMPOENT."))

(defaction ok ((c component) &optional (value c))
  (answer value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 2003-2005 Edward Marco Baringer
;;; All rights reserved. 
;;; 
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions are
;;; met:
;;; 
;;;  - Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 
;;;  - Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;; 
;;;  - Neither the name of Edward Marco Baringer, nor BESE, nor the names
;;;    of its contributors may be used to endorse or promote products
;;;    derived from this software without specific prior written permission.
;;; 
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
