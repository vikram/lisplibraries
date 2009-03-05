(in-package :cl-muproc.compat)

(defstruct (mbox (:conc-name mbox.)) 
  (lock (make-lock))
  (waitqueue  (make-condition-variable))
  (capacity 0 :type (or fixnum null))
  (queue (list) :type list))

(defun %mbox-fullp (mbox)
  (and (mbox.capacity mbox)
       (> (length (mbox.queue mbox)) (mbox.capacity mbox))))

(defun mbox-fullp (mbox)
  (with-lock-held ((mbox.lock mbox))
    (%mbox-fullp mbox)))

(defun mbox-emptyp (mbox)
  (with-lock-held ((mbox.lock mbox))
    (zerop (length (mbox.queue mbox)))))

(defun mbox-send (mbox message &key (blockp t))
  (with-lock-held ((mbox.lock mbox))
    (when (and (%mbox-fullp mbox) (not blockp))
      (return-from mbox-send (values nil nil)))
    (loop
       while (%mbox-fullp mbox)
       do (condition-wait (mbox.waitqueue mbox) (mbox.lock mbox)))
    
    (let ((lmsg (list message))
          (q (mbox.queue mbox)))
      (setf (mbox.queue mbox) (if q (nconc q lmsg) lmsg))
      (condition-notify (mbox.waitqueue mbox))
      (values message t))))

(defun mbox-read (mbox &key (blockp t))
  (with-lock-held ((mbox.lock mbox))
    (when (and (zerop (length (mbox.queue mbox))) (not blockp))
      (return-from mbox-read (values nil nil)))
    (loop
       for q = (mbox.queue mbox)
       while (zerop (length q))
       do (condition-wait (mbox.waitqueue mbox) (mbox.lock mbox)))

    (multiple-value-prog1
        (values (pop (mbox.queue mbox)) t)
      ;; we haven't broadcast in bordeaux-threads
      (condition-notify (mbox.waitqueue mbox)))))
