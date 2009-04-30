;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; repeater-day.lisp

;;; See the LICENSE file for licensing information.

(cl:in-package #:chronicity)

(defclass repeater-day (repeater)
  ((current :initform nil)))

(defmethod r-next ((repeater repeater-day) pointer)
  (with-slots (current now)
      repeater
    (when (not current)
      (setf current (copy-date now)))
    (let ((direction (if (eql pointer :future) 1 -1)))
      (setf current (datetime-incr current :day direction))
      (make-span current (datetime-incr current :day) t))))

(defmethod r-this ((repeater repeater-day) pointer)
  (with-slots (current now)
      repeater
    (destructuring-bind (day-start day-end)
        (ecase pointer
          (:future (list (start-of-hour (datetime-incr now :hour))
                         (start-of-day (datetime-incr now :day))))
          (:past (list (copy-date now)
                       (copy-datetime now :minute 0 :sec 0)))
          (:none (list (copy-date now)
                       (start-of-day (datetime-incr now :day)))))
      (make-span day-start day-end t))))

(defmethod r-offset ((repeater repeater-day) span amount pointer)
  (let ((offset (* (if (eql pointer :future) 1 -1) amount)))
    (span+ span offset :day)))

(defmethod r-width ((repeater repeater-day))
  +day-seconds+)