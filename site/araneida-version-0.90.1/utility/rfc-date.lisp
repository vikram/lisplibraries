(in-package :araneida)

(defun universal-time-to-rfc-date (utime)
  "Format and return UTIME in something akin to the RFC date formatting"
  ;; this conses 4k per call, almost entirely due to the call to FORMAT.
  ;; one possible optimization would be to cache computed dates
  (declare (optimize (speed 3)))
  (multiple-value-bind (sec min hour date month year day)
       (decode-universal-time utime 0)
    (let ((days-of-week '((0 . "Mon") (1 . "Tue") (2 . "Wed") (3 . "Thu")
                          (4 . "Fri") (5 . "Sat") (6 . "Sun")))
          (months-of-year '((1 . "Jan") (2 . "Feb") (3 . "Mar") (4 . "Apr")
                            (5 . "May") (6 . "Jun") (7 . "Jul") (8 . "Aug")
                            (9 . "Sep") (10 . "Oct") (11 . "Nov")
                            (12 . "Dec"))))
      (format nil "~A, ~2,'0D ~A ~D ~2,'0D:~2,'0D:~2,'0D"
                    (cdr (assoc day days-of-week))
                    date (cdr (assoc month months-of-year)) year
              hour min sec))))

