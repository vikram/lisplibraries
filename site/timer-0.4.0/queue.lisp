;;;; 
;;;; queue.lisp
;;;; 
;;;; Created: 2003-11-07 by Zach Beane <xach@xach.com>
;;;; 
;;;; Data structures for managing scheduled timers.
;;;; 
;;;;
;;;; See the file COPYING for license information.
;;;; queue.lisp,v 1.5 2003/11/19 17:22:58 xach Exp

(in-package #:timer)

;;;
;;; Heap (for the priority queue)
;;;

(defun heap-parent (i)
  (ash i -1))


(defun heap-left (i)
  (ash i 1))


(defun heap-right (i)
  (1+ (ash i 1)))


(defun heap-size (heap)
  (1- (length heap)))


(defun heapify (heap start &key (key #'identity) (test #'>=))
  (declare (function key test))
  (flet ((key (obj) (funcall key obj))
         (ge (i j) (funcall test i j)))
    (let ((l (heap-left start))
          (r (heap-right start))
          (size (heap-size heap))
          largest)
      (setf largest (if (and (<= l size)
                             (not (ge (key (aref heap start))
                                      (key (aref heap l)))))
                        l
                        start))
      (when (and (<= r size)
                 (not (ge (key (aref heap largest))
                          (key (aref heap r)))))
        (setf largest r))
      (when (/= largest start)
        (rotatef (aref heap largest) (aref heap start))
        (heapify heap largest :key key :test test)))
    heap))
                                    


(defun heap-insert (heap new-item &key (key #'identity) (test #'>=))
  (declare (function key test))
  (flet ((key (obj) (funcall key obj))
         (ge (i j) (funcall test i j)))
    (incf (fill-pointer heap))
    (loop for i = (heap-size heap) then parent-i
          for parent-i = (heap-parent i)
          while (and (> i 0)
                     (not (ge (key (aref heap parent-i))
                              (key new-item))))
          do (setf (aref heap i) (aref heap parent-i))
          finally (setf (aref heap i) new-item))
    heap))
    


(defun heap-maximum (heap)
  (unless (zerop (length heap))
    (aref heap 0)))


(defun heap-extract (heap i &key (key #'identity) (test #'>=))
  (when (< (length heap) i)
    (error "Heap underflow"))
  (prog1
      (aref heap i)
    (setf (aref heap i) (aref heap (heap-size heap)))
    (decf (fill-pointer heap))
    (heapify heap i :key key :test test)))


(defun heap-extract-maximum (heap &key (key #'identity) (test #'>=))
  (heap-extract heap 0 :key key :test test))


;;;
;;; Priority queue
;;;

(defstruct (priority-queue
             (:conc-name %pqueue-)
             (:constructor %make-priority-queue))
  contents
  keyfun)


(defun make-priority-queue (&key (key #'identity) (element-type t))
  (let ((contents (make-array 100
                              :adjustable t
                              :fill-pointer 0
                              :element-type element-type)))
    (%make-priority-queue :keyfun key
                          :contents contents)))


(defmethod print-object ((object priority-queue) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~[empty~:;~:*~D item~:P~]"
            (length (%pqueue-contents object)))))


(defun priority-queue-maximum (priority-queue)
  "Return the item in PRIORITY-QUEUE with the largest key."
  (with-slots (contents)
      priority-queue
    (unless (zerop (length contents))
      (heap-maximum contents))))


(defun priority-queue-extract-maximum (priority-queue)
  "Remove and return the item in PRIORITY-QUEUE with the largest key."
  (with-slots (contents keyfun)
      priority-queue
    (unless (zerop (length contents))
      (heap-extract-maximum contents :key keyfun :test #'<=))))


(defun priority-queue-insert (priority-queue new-item)
  "Add NEW-ITEM to PRIOIRITY-QUEUE."
  (with-slots (contents keyfun)
      priority-queue
    (heap-insert contents new-item :key keyfun :test #'<=)))


(defun priority-queue-empty-p (priority-queue)
  (zerop (length (%pqueue-contents priority-queue))))


(defun priority-queue-remove (priority-queue item &key (test #'eq))
  "Remove and return ITEM from PRIORITY-QUEUE."
  (with-slots (contents keyfun)
      priority-queue
    (let ((i (position item contents :test test)))
      (when i
        (heap-extract contents i :key keyfun :test #'<=)))))


