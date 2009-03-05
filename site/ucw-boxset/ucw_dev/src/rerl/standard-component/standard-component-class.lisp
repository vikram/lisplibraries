;;;; -*- lisp -*-

(in-package :it.bese.ucw)

;;;; STANDARD-COMPONENT-CLASS meta object

(defclass standard-component-class (mopp:standard-class)
  ((backtrack-slots :initform nil :accessor component-class.backtrack-slots)
   (component-slots :initform nil :accessor component-class.component-slots)
   (place-slot :initform nil :initarg :place-slot
               :accessor component-class.place-slot))
  (:documentation "The meta class for standard-components.

Classes defined with this meta-class have extra slot options, see
the class STANDARD-COMPONENT-DIRECT-SLOT for details."))

(defmethod mopp:validate-superclass ((component-class standard-component-class)
                                     (super-class mopp:standard-class))
  "Declare that standard-component-classes can be sub classes of standard-class."
  t)

(defun initialize-component-class-after (class)
  ":after initialization function for standard-component-class objects.

Setup the proper values in component-class.backtrack-slots and
component-class.component-slots in CLASS based on the effective
slots of CLASS."
  (setf (component-class.backtrack-slots class) '()
	(component-class.component-slots class) '())
  (iterate
    (for slot in (mopp:class-slots class))
    (when (typep slot 'standard-component-effective-slot)
      (when (component-slot.backtrack slot)
	(collect (cons (mopp:slot-definition-name slot) (eval (component-slot.backtrack slot)))
	  into backtracked-slots))
      ;;we allow for the definition of component-slots by ":component nil"
      ;; so as long as it is unbound, add it to the list.
      (when (slot-boundp slot 'component)
	(collect (cons (mopp:slot-definition-name slot)
		       (component-slot.component slot))
	  into component-slots)))
    (finally
     (setf (component-class.backtrack-slots class) backtracked-slots
	   (component-class.component-slots class) component-slots)))
  (setf (component-class.place-slot class) (first (component-class.place-slot class))))

;;;; Code stolen from Pascal Costanza's AspectL
;;;;
;;;; AspectL - Copyright (c) 2003, 2004 Pascal Costanza.
;;;; License: See http://creativecommons.org/licenses/by/2.0/
;;;;

(defvar *preliminary-modified-classes* (make-hash-table :test #'equal)
  "Classes that have forward referenced classes as direct superclasses but no
   direct superclass that is a subtype of the required superclass are
   preliminary modified to have the required superclass as a direct superclass.
   This table maps those classes to the original list of direct superclasses
   so that the preliminary modification can be undone in case one of the
   forward referenced classes turns out as a subtype of the required
   superclass.")

(defun initialize-class-metaobject
       (call-next class metaclass required-superclass
                  &rest initargs
                  &key direct-superclasses
                  &allow-other-keys)
  "Can be used in an initialize-instace method specialized on a metaclass.
   Ensures that the required superclass will become part of the class's
   class precedence list, just below standard-object, by appending it to the
   list of direct superclasses if required. Correctly handles forward
   referenced classes. If used, reinitalize-instance must also be
   specialized and call reinitialize-class-metaobject accordingly. Should be
   used as follows, with appropriate replacements for 'metaclass' and
   'required-superclass':

   (defmethod initialize-instance :around
     ((class metaclass) &rest initargs)
     (declare (dynamic-extent initargs))
     (apply #'initialize-class-metaobject #'call-next-method
            class (find-class 'metaclass) (find-class 'required-superclass)
            initargs))"
  (declare (dynamic-extent initargs))
  (let ((direct-superclasses (remove required-superclass direct-superclasses))
        (class-key (list class metaclass)))
    (remhash class-key *preliminary-modified-classes*)
    (if (some (lambda (direct-superclass)
                (typep direct-superclass metaclass))
              direct-superclasses)
        (apply call-next class
               :direct-superclasses direct-superclasses
               initargs)
      (progn
        (when (some (lambda (direct-superclass)
                      (typep direct-superclass 'mopp:forward-referenced-class))
                    direct-superclasses)
          (setf (gethash class-key *preliminary-modified-classes*)
                direct-superclasses))
        (apply call-next class
               :direct-superclasses
               (append (remove (find-class 'standard-object)
                               direct-superclasses :test #'eq)
                       (list required-superclass))
               initargs)))))

(defun reinitialize-class-metaobject
       (call-next class metaclass required-superclass
                  &rest initargs
                  &key (direct-superclasses nil direct-superclasses-p)
                  &allow-other-keys)
  "Can be used in a reinitialize-instace method specialized on a metaclass.
   Ensures that the required superclass will become part of the class's
   class precedence list, just below standard-object, by appending it to the
   list of direct superclasses if required. Correctly handles forward
   referenced classes. If used, initalize-instance must also be
   specialized and call initialize-class-metaobject accordingly. Should be
   used as follows, with appropriate replacements for 'metaclass' and
   'required-superclass':

   (defmethod reinitialize-instance :around
     ((class metaclass) &rest initargs)
     (declare (dynamic-extent initargs))
     (apply #'reinitialize-class-metaobject #'call-next-method
            class (find-class 'metaclass) (find-class 'required-superclass)
            initargs))"
  (declare (dynamic-extent initargs)
           (ignore direct-superclasses))
  (prog1
      (if direct-superclasses-p
          (apply #'initialize-class-metaobject call-next
                 class metaclass required-superclass initargs)
        (let* ((class-key (list class metaclass))
               (org-direct-superclasses (gethash class-key *preliminary-modified-classes*)))
          (if org-direct-superclasses
              (apply #'initialize-class-metaobject call-next
                     class metaclass required-superclass
                     :direct-superclasses org-direct-superclasses initargs)
            (apply call-next class initargs))))
    (mapc #'reinitialize-instance (mopp:class-direct-subclasses class))))

;;;; End code by Pascal Costanza

(defmethod initialize-instance ((class standard-component-class) &rest initargs)
  (apply #'initialize-class-metaobject #'call-next-method
	 class (find-class 'standard-component-class) (find-class 'standard-component)
	 initargs)
  (when (mopp:class-finalized-p class)
    (initialize-component-class-after class)))

(defmethod reinitialize-instance ((class standard-component-class)
                                  &rest initargs)
  (apply #'reinitialize-class-metaobject #'call-next-method
	 class (find-class 'standard-component-class) (find-class 'standard-component)
	 initargs)
  (when (mopp:class-finalized-p class)
    (initialize-component-class-after class)))

(defmethod mopp:finalize-inheritance :after ((class standard-component-class))
  (initialize-component-class-after class))

;;; Component slot metaobjects

(defclass standard-component-direct-slot
    (mopp:standard-direct-slot-definition)
  ((backtrack :initarg :backtrack :accessor component-slot.backtrack)
   (component :initarg :component :accessor component-slot.component))
  (:documentation "The class for direct slots of standard-components.

Other than the initargs for standard slots the following
options can be passed to component slots:

:backtrack [ T | NIL | FUNCTION-NAME ] - Specify that this slot
should be backtracked (or not if NIL is passed as the value). If
the value is neither T nor NIL then it must be a function which
will be used as the copyer.

:component [ TYPE | ( TYPE &rest INITARGS ) ] - Specify that this
slot is actually a nested component of type TYPE. When instances
of the class are created this slot will be set to an instance of
type TYPE and it's place will be set to this slot. If a list is
passed to :component then TYPE (which isn't evaluated) will be
passed as the first argument to make-instance. The INITARGS will
be eval'd and apply'd to make-instance. The result of this call
to make-instance will be used as the effective component
object."))

(defgeneric standard-component-direct-slot-p (object)
  (:method ((o t)) nil)
  (:method ((o standard-component-direct-slot)) t))

(defmethod mopp:direct-slot-definition-class ((class standard-component-class)
                                              &rest initargs)
  (declare (ignore initargs))
  (find-class 'standard-component-direct-slot))

(eval-when (:compile-toplevel :load-toplevel :execute) 
  (defclass standard-component-effective-slot
    (mopp:standard-effective-slot-definition)
    ((backtrack :accessor component-slot.backtrack :initform nil)
     (component :accessor component-slot.component
                :documentation "When this slot is bound, then setf-ing it
later with a component will update the place of the component being set."))
    (:documentation "The class for effective slots of standard-components.

Exactly like STANDARD-COMPONENT-DIRECT-SLOT except the values
have been converted from what defclass passes as initargs to a
form more easily used by the other methods.")))

(defmethod mopp:effective-slot-definition-class ((class standard-component-class)
                                                 &rest initargs)
  (declare (ignore initargs))
  (find-class 'standard-component-effective-slot))

(defmethod mopp:compute-effective-slot-definition ((class standard-component-class)
                                                   slot-name
                                                   direct-slot-definitions)
  "Extract the first backtrack and component values."
  (declare (ignore slot-name))
  (let ((effective-slot (call-next-method)))
    (flet ((effective-backtrack-value (backtrack-spec)
             (if (eql t backtrack-spec)
                 '#'identity
                 backtrack-spec))
           (first-specifying-slot (slot-name)
             "Returns the first STANDARD-COMPONENT-DIRECT-SLOT
             slot in direct-slot-definitions with a slot named
             SLOT-NAME bound."
             (dolist (slot direct-slot-definitions)
               (when (and (standard-component-direct-slot-p slot)
                          (slot-boundp slot slot-name))
                 (return slot)))))
      ;; do we want backtracking for this slot?
      (when-bind slot (first-specifying-slot 'backtrack)
        (setf (component-slot.backtrack effective-slot)
              (effective-backtrack-value (slot-value slot 'backtrack))))
      ;; is this slot a component slot?
      (when-bind slot (first-specifying-slot 'component)
        (let ((component-spec (slot-value slot 'component)))
          ;; make sure the slot is bound. this will mean later, when putting
          ;; a component in this effective slot, to update the place slot
          ;; of the component being put. (with a slot-value-using-class override)
          (setf (component-slot.component effective-slot) component-spec)
          (when component-spec
            ;; component-slots also have backtracking (unless
            ;; explicitly overridden)
            (setf (component-slot.backtrack effective-slot)
                  (if (slot-boundp slot 'backtrack)
                      (effective-backtrack-value (slot-value slot 'backtrack))
                      '#'identity)))))
      effective-slot)))

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
