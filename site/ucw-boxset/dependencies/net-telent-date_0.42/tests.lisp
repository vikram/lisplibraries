(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage #:net.telent.date.test (:use #:net.telent.date #:rt)))

;;; This is not presently supposed to work out of the box.  When we've
;;; figured out something approximating a standard test architecture
;;; for cclan, we'll revisit it

(in-package :net.telent.date.test)

(deftest print-date
    (with-date 3200663765 0
               (format nil
                       (formatter "~3/net.telent.date:dayname/, ~2,'0D ~3/net.telent.date:monthname/ ~D ~2,'0D:~2,'0D:~2,'0D")
                       day-of-week day-of-month month year hour minute second))
  "Mon, 04 Jun 2001 17:16:05")

(deftest parse-date
    (date:parse-time "Mon, 04 Jun 2001 17:16:05")
  3200663765)
