(in-package #:metatilities)

(defconstant +minutes-per-hour+ 60
  "The number of minutes in one hour.")

(defconstant +seconds-per-minute+ 60
  "The number of seconds in one minute.")

(defconstant +usual-days-per-year+ 365
  "The number of days in an ordinary year.")

(defconstant +seconds-per-hour+ (* +seconds-per-minute+ +minutes-per-hour+)
  "The number of seconds in one hour.")

(defconstant +hours-per-day+ 24
  "The number of hours in one day.")

(defconstant +seconds-per-day+
  (* +hours-per-day+ +seconds-per-hour+)
  "The number of seconds in one day.")

(defparameter +days-per-month+
  '(31 28 31 30 31 30 31 31 30 31 30 31))

(eval-always 
  (defmacro generate-time-part-function (part-name position)
    (let ((function-name (form-symbol (symbol-name 'time) "-" part-name)))
      `(eval-always
         (export ',function-name)
         (defun ,function-name
                (&optional (universal-time (get-universal-time))
                           (time-zone nil))
           ,(format nil "Returns the ~(~A~) part of the given time." part-name)
           (nth-value ,position (apply #'decode-universal-time universal-time time-zone))))))

  (generate-time-part-function second 0)
  (generate-time-part-function minute 1)
  (generate-time-part-function hour 2)
  (generate-time-part-function date 3)
  (generate-time-part-function month 4)
  (generate-time-part-function year 5)
  (generate-time-part-function day-of-week 6)
  (generate-time-part-function daylight-savings-time-p 7))

(defun days-in-month (month &optional leap-year?)
  "Returns the number of days in the specified month. The month should be
between 1 and 12."
  (+ (nth (1- month) +days-per-month+) (if (and (= month 2) leap-year?) 1 0)))

(defun leap-year-p (year)
  "Returns t if the specified year is a leap year. I.e. if the year
is divisible by four but not by 100 or if it is divisible by 400."
  (or (and (= (mod year 4) 0)               ; logand is faster but less perspicuous
           (not (= (mod year 100) 0)))
      (= (mod year 400) 0)))

(defun day-of-year (date)
  "Returns the day of the year [1 to 366] of the specified date [which must be \(CL\) universal time format.]" 
  (let ((leap-year? (leap-year-p (time-year date))))
    (+ (loop for month from 1 to (1- (time-month date)) sum
             (days-in-month month leap-year?))
       (time-date date))))

;;; format-date

(defun format-date (format date &optional stream time-zone)
  "Formats universal dates using the same format specifiers as NSDateFormatter. The format is:

%% - A '%' character
%a - Abbreviated weekday name
%A - Full weekday name
%b - Abbreviated month name
%B - Full month name
%c - Shorthand for \"%X %x\", the locale format for date and time
%d - Day of the month as a decimal number [01-31]
%e - Same as %d but does not print the leading 0 for days 1 through 9 
     [unlike strftime[], does not print a leading space]
%F - Milliseconds as a decimal number [000-999]
%H - Hour based on a 24-hour clock as a decimal number [00-23]
%I - Hour based on a 12-hour clock as a decimal number [01-12]
%j - Day of the year as a decimal number [001-366]
%m - Month as a decimal number [01-12]
%M - Minute as a decimal number [00-59]
%p - AM/PM designation for the locale
%S - Second as a decimal number [00-59]
%w - Weekday as a decimal number [0-6], where Sunday is 0
%x - Date using the date representation for the locale, including 
     the time zone [produces different results from strftime[]]
%X - Time using the time representation for the locale [produces 
     different results from strftime[]]
%y - Year without century [00-99]
%Y - Year with century [such as 1990]
%Z - Time zone name [such as Pacific Daylight Time; 
     produces different results from strftime[]]
%z - Time zone offset in hours and minutes from GMT [HHMM]

None of %c, %F, %p, %x, %X, %Z, %z are implemented."
  (declare (ignore time-zone))
  (let ((format-length (length format)))
    (format 
     stream "~{~A~}"
     (loop for index = 0 then (1+ index) 
	while (< index format-length) collect 
	  (let ((char (aref format index)))
	    (cond 
	      ((char= #\% char)
	       (setf char (aref format (incf index)))
	       (cond 
		 ;; %% - A '%' character
		 ((char= char #\%) #\%)
                            
		 ;; %a - Abbreviated weekday name
		 ((char= char #\a) (day->string (time-day-of-week date) :short))
                            
		 ;; %A - Full weekday name
		 ((char= char #\A) (day->string (time-day-of-week date) :long))
                            
		 ;; %b - Abbreviated month name
		 ((char= char #\b) (month->string (time-month date) :short))
                            
		 ;; %B - Full month name
		 ((char= char #\B) (month->string (time-month date) :long))
                            
		 ;; %c - Shorthand for "%X, %x", the locale format for date and time
		 ((char= char #\c) (nyi))
                            
		 ;; %d - Day of the month as a decimal number [01-31]
		 ((char= char #\d) (format nil "~2,'0D" (time-date date)))
                            
		 ;; %e - Same as %d but does not print the leading 0 for days 1 through 9 
		 ;;      Unlike strftime, does not print a leading space
		 ((char= char #\e) (format nil "~D" (time-date date)))
                            
		 ;; %F - Milliseconds as a decimal number [000-999]
		 ((char= char #\F) (nyi))
                            
		 ;; %H - Hour based on a 24-hour clock as a decimal number [00-23]
		 ((char= char #\H) (format nil "~2,'0D" (time-hour date)))
                            
		 ;; %I - Hour based on a 12-hour clock as a decimal number [01-12]
		 ((char= char #\I) (format nil "~2,'0D" (1+ (mod (time-hour date) 12))))
                            
		 ;; %j - Day of the year as a decimal number [001-366]
		 ((char= char #\j) (format nil "~3,'0D" (day-of-year date)))
                            
		 ;; %m - Month as a decimal number [01-12]
		 ((char= char #\m) (format nil "~2,'0D" (time-month date)))
                            
		 ;; %M - Minute as a decimal number [00-59]
		 ((char= char #\M) (format nil "~2,'0D" (time-minute date)))
                            
		 ;; %p - AM/PM designation for the locale
		 ((char= char #\p) (nyi))
                            
		 ;; %S - Second as a decimal number [00-59]
		 ((char= char #\S) (format nil "~2,'0D" (time-second date)))
                            
		 ;; %w - Weekday as a decimal number [0-6], where Sunday is 0
		 ((char= char #\w) (format nil "~D" (time-day-of-week date)))
                            
		 ;; %x - Date using the date representation for the locale, 
		 ;;      including the time zone [produces different results from strftime]
		 ((char= char #\x) (nyi))
                            
		 ;; %X - Time using the time representation for the locale 
		 ;;      [produces different results from strftime]
		 ((char= char #\X) (nyi))
                            
		 ;; %y - Year without century [00-99]
		 ((char= char #\y) 
		  (let ((year-string (format nil "~,2A" (time-year date))))
		    (subseq year-string (- (length year-string) 2))))
                            
		 ;; %Y - Year with century [such as 1990]
		 ((char= char #\Y) (format nil "~D" (time-year date)))
                            
		 ;; %Z - Time zone name (such as Pacific Daylight Time; 
		 ;;      produces different results from strftime.
		 ((char= char #\Z) (nyi))
                            
		 ;; %z - Time zone offset in hours and minutes from GMT [HHMM]
		 ((char= char #\z) (nyi))
                            
		 (t
		  (error "Ouch - unknown formatter '%~c" char))))
	      (t char)))))))

(defconstant +longer-format-index+ 0)
(defconstant +shorter-format-index+ 1)

(defparameter +month-output-list+
  '(("January" "February" "March" "April" "May" "June" "July" "August" "September"
     "October" "November" "December")
    ("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")))

(defparameter +dow-output-list
  '(("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday")
    ("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")))

(defun day->string (day-of-the-week &optional (format :long))
  "Returns the name of `day-of-the-week`. The parameter should be a number between 0 and 6 where 0 represents Sunday and 6 repressents Saturday. The optional format argument can be either :long or :short. In the latter case, the return string will be of length three; in the former it will be the complete name of the appropriate day."
  (check-type day-of-the-week (mod 7))
  (check-type format (member :long :short))
  (nth day-of-the-week 
       (case format
	 (:long (nth +longer-format-index+ +dow-output-list))
	 (:short (nth +shorter-format-index+ +dow-output-list)))))

(defun month->string (month &optional (format :long))
  "Returns the name \(in English\) of the month. Format can be :long or :short."
  (check-type month (integer 1 12))
  (check-type format (member :long :short))
  (nth (1- month) 
       (case format
	 (:long (nth +longer-format-index+ +month-output-list+))
	 (:short (nth +shorter-format-index+ +month-output-list+)))))

