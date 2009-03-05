(in-package :net.telent.date)

(defvar *day-names*
  #("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"))
    
(defun dayname (stream arg colon-p at-p &optional width (mincol 0) (colinc 1) (minpad 0) (padchar #\Space))
  "Print the day of the week (0=Sunday) corresponding to ARG on STREAM.  This is intended for embedding in a FORMAT directive: WIDTH governs the number of characters of text printed, MINCOL, COLINC, MINPAD, PADCHAR work as for ~A"
  (let ((daystring (elt *day-names* (mod arg 7))))
    (if (not daystring) (return-from dayname nil))
    (let ((truncate (if width (min width (length daystring)) nil)))
      (format stream
              (if at-p "~V,V,V,V@A" "~V,V,V,VA")
              mincol colinc minpad padchar
              (subseq daystring 0 truncate)))))

(defvar *month-names*
  #("???"  "January" "February" "March" "April" "May" "June"
    "July" "August" "September" "October" "November" "December"))
    
(defun monthname (stream arg colon-p at-p &optional width (mincol 0) (colinc 1) (minpad 0) (padchar #\Space))
  "Print the name of the month (1=January) corresponding to ARG on STREAM.  This is intended for embedding in a FORMAT directive: WIDTH governs the number of characters of text printed, MINCOL, COLINC, MINPAD, PADCHAR work as for ~A"
  (let ((monthstring (elt  *month-names* arg)))
    (if (not monthstring) (return-from monthname nil))
    (let ((truncate (if width (min width (length monthstring)) nil)))
      (format stream
              (if at-p "~V,V,V,V@A" "~V,V,V,VA")
              mincol colinc minpad padchar
              (subseq monthstring 0 truncate)))))

;;; the second of these functions is an attempt to fix the wrong
;;; problem (emacs likes to indent it strangely) with the first.  The
;;; real problem with the first is, as c.l.l will rush to tell you,
;;; that it introduces anaphora - and lots of them.  I don't really
;;; advise the use of either in user code, and they're only exported
;;; for historical reasons

(defmacro with-date (utime zone &body body)
  "UTIME is a universal-time, ZONE is a number of hours offset from UTC, or NIL to use local time.  Execute BODY in an environment where SECOND MINUTE HOUR DAY-OF-MONTH MONTH YEAR DAY-OF-WEEK DAYLIGHT-P ZONE are bound to the decoded components of the universal time"
  `(multiple-value-bind
    (second minute hour day-of-month month year day-of-week daylight-p zone)
    (decode-universal-time ,utime  ,@(if zone (list zone)))
    (declare (ignorable second minute hour day-of-month month year day-of-week daylight-p zone))
    ,@body))

(defmacro with-decoding ((utime &optional zone) &body body)
  "UTIME is a universal-time, ZONE is a number of hours offset from UTC, or NIL to use local time.  Execute BODY in an environment where SECOND MINUTE HOUR DAY-OF-MONTH MONTH YEAR DAY-OF-WEEK DAYLIGHT-P ZONE are bound to the decoded components of the universal time"
  `(multiple-value-bind
    (second minute hour day-of-month month year day-of-week daylight-p zone)
    (decode-universal-time ,utime ,@(if zone (list zone)))
    (declare (ignorable second minute hour day-of-month month year day-of-week daylight-p zone))
    ,@body))

(defun decode-universal-time/plist (utime &optional zone)
  (multiple-value-bind (s m h d month y wd d-p zone)
      (decode-universal-time utime zone)
    (list :second s :minute m :hour h
	  :day d :day-of-month d :month month :year y 
	  :weekday wd :day-of-week wd
	  :weekday-name (elt *day-names* wd)
	  :month-name (elt *month-names* month)
	  :daylight-p d-p :zone zone)))

(defun universal-time-to-http-date (utime)
  "Decode the universal time UTIME and return a date suitable for use in HTTP 1.0 applications (RFC-822, but GMT)"
  (declare (optimize (speed 3)))
  (with-date
      utime 0
      (format nil
              (formatter "~3/net.telent.date:dayname/, ~2,'0D ~3/net.telent.date:monthname/ ~D ~2,'0D:~2,'0D:~2,'0D GMT")
              day-of-week day-of-month month year hour minute second)))

;;; follow the decode-universal-time rules for handling a time-zone parameter.
;;; CL timezones increase westward; RFC timezone specifications increase
;;; _eastward_.  I love standards
(defun universal-time-to-rfc2822-date (utime &optional time-zone)
  "Decode the universal time UTIME and return an RFC-2822-format string.  TIME-ZONE is a CL time zone.   If not supplied, it defaults to the current time zone adjusted for daylight saving time.  If TIME-ZONE is supplied, daylight saving time information is ignored."
  (declare (optimize (speed 3)))
  (with-decoding (utime time-zone)
    (let ((daylight-zone (if daylight-p (1- zone) zone)))
      (multiple-value-bind (z-h z-m) (truncate (abs daylight-zone))
	(format nil
		(formatter "~3/net.telent.date:dayname/, ~2,'0D ~3/net.telent.date:monthname/ ~D ~2,'0D:~2,'0D:~2,'0D ~A~2,'0D~2,'0D")
		day-of-week day-of-month month year hour minute second 
		(if (minusp daylight-zone) #\+ #\-) 
		z-h (floor (* 60 z-m)))))))
#|

Timezones are _so_ easy to get wrong that I strongly advise always using UTC
for anything that a machine might someday want to parse.  When testing
changes to universal-time-to-rfc2822-date, make sure to cover

1) Explicit timezone, in summer and in winter

2) Local time, in summer and in winter
2a) In a zone west of Greenwich (e.g. any US zone)
:; TZ=US/Eastern date -R --date='23 Aug 2003 12:00:00 GMT'
Sat, 23 Aug 2003 08:00:00 -0400
:; TZ=US/Eastern date -R --date='05 Jan 2003 12:00:00 GMT'
Sun, 05 Jan 2003 07:00:00 -0500

2b) In a zone east of Greenwich (e.g. Europe/Paris)
:; TZ=Europe/Paris date -R --date='23 Aug 2003 12:00:00 GMT'
Sat, 23 Aug 2003 14:00:00 +0200
:; TZ=Europe/Paris date -R --date='05 Jan 2003 12:00:00 GMT'
Sun, 05 Jan 2003 13:00:00 +0100

2c) In Canada/Newfoundland or other non-integer zone
:; TZ=Canada/Newfoundland date -R --date='23 Aug 2003 12:00:00 GMT'
Sat, 23 Aug 2003 09:30:00 -0230
:; TZ=Canada/Newfoundland date -R --date='05 Jan 2003 12:00:00 GMT'
Sun, 05 Jan 2003 08:30:00 -0330

Note signs on the timezones, and that the offset for a half-hour is
xx30 not xx50.  The universal times in question are

3250756800 is 05 Jan 2003, probably not DST
3270628800 is 23 Aug 2003 (daylight savings in effect most places)

You probably need to change the TZ environment variable and restart your
Lisp to do most of the tests in (2).  

|#


;;; for backward compatibility (with araneida, mostly)
(setf (symbol-function 'universal-time-to-rfc-date)
      #'universal-time-to-http-date)
