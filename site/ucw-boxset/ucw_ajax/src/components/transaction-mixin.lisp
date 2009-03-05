;; -*- lisp -*-

(in-package :it.bese.ucw)

(defcomponent transaction-mixin ()
  ((transaction-stack :initarg :transaction-stack
                      :accessor component.transaction-stack
                      :initform nil
                      :backtrack t)))

(defmethod answer-component* ((source transaction-mixin) (target standard-component) value)
  (if-bind trans (car (component.transaction-stack source))
    (if (eql trans :open)
        ;; ok, the transaction is open, perform a "normal" answer
        (call-next-method)
        ;; bad, the transaction has been closed, TRANS
        ;; should be the continuation which will just
        ;; simulate the last step of the transaction.
        (with-call/cc (funcall trans t)))
    (call-next-method)))

(defmethod/cc open-transaction ((comp transaction-mixin))
  (push :open (component.transaction-stack comp)))

(defmethod/cc close-transaction ((comp transaction-mixin))
  (let/cc k
    (with-slots (transaction-stack)
        comp
      ;; in order to understand this form you need to remember that
      ;; (component.transaction-stack ex) is backtracked, but it's car
      ;; cell isn't. so if we ever "go back" we'll end up with the old
      ;; component.transaction-stack, but it's car will be k, not
      ;; :open (or whatever it was).
      (let (fixed-k)
        (setf fixed-k (lambda (v)
                        (declare (ignore v))
                        (setf (car transaction-stack) fixed-k
                              transaction-stack (cdr transaction-stack))
                        (kall k t)))
        (funcall fixed-k :whatever)))))


