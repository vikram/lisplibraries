;; -*- lisp -*-

(in-package :it.bese.ucw)

;;;; ** Container

(defclass container ()
  ((contents :accessor container.contents :initform '()
             :initarg :contents
             :documentation "An alist of (key . component) holding the controlled components.")
   (key-generator :accessor container.key-generator
                  :initarg :key-generator
                  :initform #'identity
                  :documentation
                  "A lambda that generates the keys from a component when they are not specified")
   (key-test :accessor container.key-test
             :documentation "Function used to compare two keys."
             :initarg :key-test
             :initform #'eql))
  (:metaclass standard-component-class)
  (:documentation "Allow multiple components to share the same place.

The container component serves to manage a set of components.
It does not provide any render impementation, which is the
resposibility of the subclasses (e.g. switching-container or
list-container).

Each contained component has a \"key\" associated with it which
is used to retrieve a particular component. Keys are compared with
container.key-test.

The :contents inintarg, if provided, must be either a list of (key .
component) or a list of components. In the latter case it will
be converted into (component . component) form."))

(defmethod initialize-instance ((c container) &rest args)
  (when-bind contents (getf args :contents)
    (when (typep (first contents) 'component)
      (setf args (copy-list args))
      (setf (getf args :contents) (iter (for component in contents)
                                        (collect (cons component component))))))
  (apply #'call-next-method c args))

(defmethod shared-initialize :after ((c container) slot-names
                                     &rest initargs
                                     &key contents)
  "This method sets up any initial contents for backtacking. If
the contents are created via (setf find-component) then the
backtracking is done there."
  (declare (ignore initargs slot-names))
  (setf (container.contents c) nil)
  (dolist* ((key . comp) contents)
    (setf (find-component c key) comp)))

(defmethod child-components ((c container) &key predicate (key #'identity))
  (iter (for (nil . component) in (container.contents c))
        (when (or (null predicate)
                  (funcall predicate (funcall key component)))
          (collect component))))

(defmethod clear-container ((c container))
  (setf (container.contents c) '()))

(defun find-container-component-entry (c key)
  (assoc key (container.contents c)
         :test (container.key-test c)))

(defun remove-container-component-entry (c key)
  (when-bind entry (find-container-component-entry c key)
    (setf (container.contents c) (delete entry (container.contents c)))
    t))

(defmethod emptyp ((c container))
  (not (null (container.contents c))))

(defmethod find-component ((c container) key)
  "Returns the component object in C associated with KEY."
  (if-bind comp (find-container-component-entry c key)
    (values (cdr comp) t)
    (values nil nil)))

(defmethod remove-component ((c container) key)
  "Removes the component object in C associated with KEY.
Returns T when container was found and actually removed."
  (remove-container-component-entry c key))

(defmethod (setf find-component) ((component component)
                                  (container container)
                                  key)
  "Associates KEY with COMPONENT in the container CONTAINER."
  (with-slots (contents key-test) container
    (let ((index (position key contents :test key-test :key #'car)))
      (unless index
        (setf index (list-length contents)))
      (setf (component-at container index key) component)))
  component)

(defmethod add-component ((container container) (component component) &optional key)
  "Add component at the end of the component list."
  (setf (component-at container (list-length (container.contents container)) key)
        component))

(defmethod component-at ((container container) (index integer))
  "Returns the component object in CONTAINER associated at the given INDEX."
  (let ((contents (container.contents container)))
    (if (< index (list-length contents))
        (values (cdr (nth index contents)) t)
        (values nil nil))))

(defmethod (setf component-at) ((component component)
                                (container container)
                                (index integer) &optional key)
  "Associates KEY with COMPONENT in the container CONTAINER at the given index.
   If KEY is not provided use the key-generator lambda. (setf (c 0 \"optinal-key\") x)"
  (unless key
    (setf key (funcall (container.key-generator container) component)))
  (let ((contents (container.contents container)))
    (cond
      ((< index (list-length contents))
       (let ((comp-cons (car (nthcdr index contents))))
         (setf (parent (cdr comp-cons)) nil
               (cdr comp-cons) component
               (component.place component) (make-place (cdr comp-cons)))))
      ((= index (list-length contents))
       (let* ((container-cons (cons key component))
              (place (make-place (cdr container-cons))))
         (setf (component.place component) place)
         (if contents
             (nconc contents (list container-cons))
             (setf (container.contents container) (list container-cons)))
         (backtrack (context.current-frame *context*) place)))
      (t (error "Can't set component at index ~A (container size is ~A)"
                index (list-length contents))))
    (setf (parent component) container))
  component)

;;;; ** Switching Container

(defclass switching-container (container)
  ((current-component-key :accessor container.current-component-key
                          :initarg :current-component-key
                          :backtrack t
                          :documentation "The key of the current component."
                          :initform nil))
  (:metaclass standard-component-class)
  (:documentation "A simple renderable container component.

This component is like the regular CONTAINER but serves to manage a set
of components which share the same place in the UI. Therefore it provides
an implementation of RENDER which simply renders its current component.

The switching-container component class is generally used as the super
class for navigatation components and tabbed-pane like
components."))

(defmacro initialize-container ((container &key key-test current-component)
				&body contents)
  (rebinding (container key-test current-component)
    `(progn
       (when (and (not (typep ,container 'switching-container))
                  (or ,key-test ,current-component))
         (error "Tried to use initialize-container on a (not (typep x 'switching-container)) with key-test and/or current-component arguments"))

       ,(when key-test `(setf (container.key-test ,container) ,key-test))

       ,(when current-component `(setf (container.current-component-key ,container) ,current-component))

       ,@(iterate
          (for (key component-type . initargs) in contents)
          (collect `(setf (find-component ,container ,key)
                          (make-instance ',component-type ,@initargs))))
       
       ,container)))

(defmethod container.current-component ((container switching-container))
  (find-component container (container.current-component-key container)))

(defmethod (setf container.current-component) ((value null) (container switching-container))
  (when (container.current-component-key container)
    (setf (container.current-component-key container) nil))
  (values nil t))

(defmethod (setf container.current-component) ((value component) (container switching-container))
  "This method assumes that the container was initialized with #'identity as key generator.

Returns two values: (VALUE T) if the set actually happenend,
or (VALUE NIL) if the given component is not a membor of this
container."
  (if (find-container-component-entry container value)
      (if (funcall (container.key-test container)
                   (container.current-component-key container) value)
          (values value nil)
          (progn
            (setf (container.current-component-key container) value)
            (values value t)))
      (error "~S is not in the container ~S, so it can't be made current" value container)))

(defmethod ensure-valid-component-key ((container container) key)
  "Returns T if KEY names one of the component in CONTAINER.

Otherwise a restartable error is signaled."
  (if (find-component container key)
      t
      (restart-case
          (error "No component named ~S in container ~S [~S]."
                 key container (container.contents container))
        (use-first ()
          :report (lambda (stream)
                    (format stream "Use the first component in the container (~S)"
                            (cdr (first (container.contents container)))))
          (cdr (first (container.contents container)))))))

(defmethod remove-component :around ((c switching-container) key)
  (let ((position (position key (container.contents c) :test (container.key-test c) :key #'car)))
    (prog1
        (call-next-method)
      (when position
        (setf position (min position (1- (length (container.contents c)))))
        (setf (container.current-component-key c) (when (>= position 0)
                                                    (car (elt (container.contents c) position))))))))

(defaction switch-component ((container switching-container) key)
  (ensure-valid-component-key container key)
  (setf (container.current-component-key container) key))

(defmethod map-contents (func (container container))
  (mapcar (lambda (comp-spec)
            (funcall func (car comp-spec) (cdr comp-spec)))
          (container.contents container)))

(defmethod render ((container switching-container))
  (awhen (container.current-component container)
    (render it)))

;;;; ** List Container

(defclass list-container (container widget-component)
  ((orientation :initform :horizontal :initarg :orientation :accessor orientation))
  (:metaclass standard-component-class)
  (:default-initargs :css-class "list-container" :orientation :horizontal)
  (:documentation "A simple renderable container component.

This component is exactly like the regular CONTAINER but provides
an implementation of RENDER which renders its contents in <:ol/<:li tags"))

(defmethod css-class :around ((self list-container))
  ;; dynamically append the orientation css class
  (let ((result (call-next-method))
        (orientation (ecase (orientation self)
                       (:horizontal "horizontal")
                       (:vertical "vertical"))))
    (if (consp result)
        (cons orientation result)
        (list orientation result))))

(defmethod initialize-instance :before ((instance list-container)
                                        &key orientation
                                        &allow-other-keys)
  (unless (or (eql orientation :horizontal)
              (eql orientation :vertical))
    (error "Illegal orientation ~A" orientation)))

(defmethod render ((container list-container))
  (when-bind contents (container.contents container)
    (if (cdr contents)
        (<:ol (iter (for (nil . component) in contents)
                    (<:li (render component))))
        (render (cdar contents)))))

;; Copyright (c) 2003-2005 Edward Marco Baringer
;; All rights reserved. 
;; 
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;; 
;;  - Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 
;;  - Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 
;;  - Neither the name of Edward Marco Baringer, nor BESE, nor the names
;;    of its contributors may be used to endorse or promote products
;;    derived from this software without specific prior written permission.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
