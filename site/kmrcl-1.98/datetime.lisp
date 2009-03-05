;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          datetime.lisp
;;;; Purpose:       Date & Time functions for KMRCL package
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Apr 2000
;;;;
;;;; $Id$
;;;;
;;;; This file, part of KMRCL, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;;
;;;; KMRCL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:kmrcl)


;;; Formatting functions

(defun pretty-date (year month day &optional (hour 12) (m 0) (s 0))
  (multiple-value-bind (sec min hr dy mn yr wkday)
    (decode-universal-time
     (encode-universal-time s m hour day month year))
    (values (elt '("Monday" "Tuesday" "Wednesday" "Thursday"
                   "Friday" "Saturday" "Sunday")
                 wkday)
            (elt '("January" "February" "March" "April" "May" "June"
                   "July" "August" "September" "October" "November"
                   "December")
                 (1- mn))
            (format nil "~A" dy)
            (format nil "~A" yr)
            (format nil "~2,'0D:~2,'0D:~2,'0D" hr min sec))))

(defun pretty-date-ut (&optional (tm (get-universal-time)))
  (multiple-value-bind (sec min hr dy mn yr) (decode-universal-time tm)
    (pretty-date yr mn dy hr min sec)))

(defun date-string (&optional (ut (get-universal-time)))
  (if (typep ut 'integer)
      (multiple-value-bind (sec min hr day mon year dow daylight-p zone)
          (decode-universal-time ut)
        (declare (ignore daylight-p zone))
        (format nil "~[Mon~;Tue~;Wed~;Thu~;Fri~;Sat~;Sun~] ~d ~[Jan~;Feb~;Mar~;Apr~;May~;Jun~;Jul~;Aug~;Sep~;Oct~;Nov~;Dec~] ~d ~2,'0d:~2,'0d:~2,'0d"
                dow
                day
                (1- mon)
                year
                hr min sec))))

(defun print-seconds (secs)
  (print-float-units secs "sec"))

(defun print-float-units (val unit)
  (cond
    ((< val 1d-6)
     (format t "~,2,9F nano~A" val unit))
    ((< val 1d-3)
     (format t "~,2,6F micro~A" val unit))
    ((< val 1)
     (format t "~,2,3F milli~A" val unit))
    ((> val 1d9)
     (format t "~,2,-9F giga~A" val unit))
    ((> val 1d6)
     (format t "~,2,-6F mega~A" val unit))
    ((> val 1d3)
     (format t "~,2,-3F kilo~A" val unit))
    (t
     (format t "~,2F ~A" val unit))))

(defconstant +posix-epoch+
  (encode-universal-time 0 0 0 1 1 1970 0))

(defun posix-time-to-utime (time)
  (+ time +posix-epoch+))

(defun utime-to-posix-time (utime)
  (- utime +posix-epoch+))

;; Monthnames taken from net-telent-date to support lml2

(defvar *monthnames*
  '((1 . "January")
    (2 . "February")
    (3 . "March")
    (4 . "April")
    (5 . "May")
    (6 . "June")
    (7 . "July")
    (8 . "August")
    (9 . "September")
    (10 . "October")
    (11 . "November")
    (12 . "December")))

(defun monthname (stream arg colon-p at-p &optional width (mincol 0) (colinc 1) (minpad 0) (padchar #\Space))
  "Print the name of the month (1=January) corresponding to ARG on STREAM.  This is intended for embedding in a FORMAT directive: WIDTH governs the number of characters of text printed, MINCOL, COLINC, MINPAD, PADCHAR work as for ~A"
  (declare (ignore colon-p))
  (let ((monthstring (cdr (assoc arg *monthnames*))))
    (if (not monthstring) (return-from monthname nil))
    (let ((truncate (if width (min width (length monthstring)) nil)))
      (format stream
              (if at-p "~V,V,V,V@A" "~V,V,V,VA")
              mincol colinc minpad padchar
              (subseq monthstring 0 truncate)))))

(defconstant* +zellers-adj+ #(0 3 2 5 0 3 5 1 4 6 2 4))

(defun day-of-week (year month day)
  "Day of week calculation using Zeller's Congruence.
Input: The year y, month m (1 ≤ m ≤ 12) and day d (1 ≤ d ≤ 31).
Output: n - the day of the week (Sunday = 0, Saturday = 6)."

  (when (< month 3)
    (decf year))
  (mod
   (+ year (floor year 4) (- (floor year 100)) (floor year 400)
      (aref +zellers-adj+ (1- month)) day)
   7))

;;;; Daylight Saving Time calculations

;; Daylight Saving Time begins for most of the United States at 2
;; a.m. on the first Sunday of April. Time reverts to standard time at
;; 2 a.m. on the last Sunday of October. In the U.S., each time zone
;; switches at a different time.

;; In the European Union, Summer Time begins and ends at 1 am
;; Universal Time (Greenwich Mean Time). It starts the last Sunday in
;; March, and ends the last Sunday in October. In the EU, all time
;; zones change at the same moment.

;; Spring forward, Fall back
;; During DST, clocks are turned forward an hour, effectively moving
;; an hour of daylight from the morning to the evening.

;; United States                  European Union

;; Year  DST Begins DST Ends     Summertime     Summertime
;;       at 2 a.m.  at 2 a.m.    period begins  period ends
;;                               at 1 a.m. UT   at 1 a.m. UT
;; ----------------------------------------------------------
;; 2000  April 2   October 29    March 26       October 29
;; 2001  April 1   October 28    March 25       October 28
;; 2002  April 7   October 27    March 31       October 27
;; 2003  April 6   October 26    March 30       October 26
;; 2004  April 4   October 31    March 28       October 31
;; 2005  April 3   October 30    March 27       October 30
;; 2006  April 2   October 29    March 26       October 29
;; 2007  April 1   October 28    March 25       October 28
;; 2008  April 6   October 26    March 30       October 26


