;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: kmrcl-tests -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          kmrcl-tests.lisp
;;;; Purpose:       kmrcl tests file
;;;; Author:        Kevin M. Rosenberg
;;;; Date Started:  Apr 2003
;;;;
;;;; $Id$
;;;;
;;;; This file is Copyright (c) 2000-2006 by Kevin M. Rosenberg
;;;;
;;;; *************************************************************************

(in-package #:cl)
(defpackage #:kmrcl-tests
  (:use #:kmrcl #:cl #:rtest))
(in-package #:kmrcl-tests)

(rem-all-tests)


(deftest :str.0 (substitute-chars-strings "" nil) "")
(deftest :str.1 (substitute-chars-strings "abcd" nil) "abcd")
(deftest :str.2 (substitute-chars-strings "abcd" nil) "abcd")
(deftest :str.3 (substitute-chars-strings "abcd" '((#\j . "ef"))) "abcd")
(deftest :str.4 (substitute-chars-strings "abcd" '((#\a . "ef"))) "efbcd")
(deftest :str.5
    (substitute-chars-strings "abcd" '((#\a . "ef") (#\j . "ghi")))
  "efbcd")
(deftest :str.6
    (substitute-chars-strings "abcd" '((#\a . "ef") (#\d . "ghi")))
  "efbcghi")

(deftest :str.7 (escape-xml-string "") "")
(deftest :str.8 (escape-xml-string "abcd") "abcd")
(deftest :str.9 (escape-xml-string "ab&cd") "ab&amp;cd")
(deftest :str.10 (escape-xml-string "ab&cd<") "ab&amp;cd&lt;")
(deftest :str.12 (string-trim-last-character "") "")
(deftest :str.13 (string-trim-last-character "a") "")
(deftest :str.14 (string-trim-last-character "ab") "a")
(deftest :str.15 (nstring-trim-last-character "") "")
(deftest :str.16 (nstring-trim-last-character "a") "")
(deftest :str.17 (nstring-trim-last-character "ab") "a")

(deftest :str.18 (delimited-string-to-list "ab|cd|ef" #\|)
                                          ("ab" "cd" "ef"))
(deftest :str.19 (delimited-string-to-list "ab|cd|ef" #\| t)
                                          ("ab" "cd" "ef"))
(deftest :str.20 (delimited-string-to-list "") (""))
(deftest :str.21 (delimited-string-to-list "" #\space t) (""))
(deftest :str.22 (delimited-string-to-list "ab") ("ab"))
(deftest :str.23 (delimited-string-to-list "ab" #\space t) ("ab"))
(deftest :str.24 (delimited-string-to-list "ab|" #\|) ("ab" ""))
(deftest :str.25 (delimited-string-to-list "ab|" #\| t) ("ab"))

(deftest :sdstl.1 (string-delimited-string-to-list "ab|cd|ef" "|a")
  ("ab|cd|ef"))
(deftest :sdstl.2 (string-delimited-string-to-list "ab|cd|ef" "|")
  ("ab" "cd" "ef"))
(deftest :sdstl.3 (string-delimited-string-to-list "ab|cd|ef" "cd")
  ("ab|" "|ef"))
(deftest :sdstl.4 (string-delimited-string-to-list "ab|cd|ef" "ab")
  ("" "|cd|ef"))

(deftest :hexstr.1 (binary-sequence-to-hex-string ())
  "")

(deftest :hexstr.2 (binary-sequence-to-hex-string #())
  "")

(deftest :hexstr.3 (binary-sequence-to-hex-string #(165))
  "a5"
)

(deftest :hexstr.4 (binary-sequence-to-hex-string (list 165))
  "a5")

(deftest :hexstr.5 (binary-sequence-to-hex-string #(165 86))
  "a556")

(deftest :apsl.1 (append-sublists '((a b) (c d))) (a b c d))
(deftest :apsl.2 (append-sublists nil) nil)
(deftest :apsl.3 (append-sublists '((a b))) (a b))
(deftest :apsl.4 (append-sublists '((a))) (a))
(deftest :apsl.5 (append-sublists '((a) (b) (c d (e f g)))) (a b c d (e f g)))

(deftest :pss.0 (with-output-to-string (s) (print-separated-strings s "|" nil))
  "")

(deftest :pss.1
    (with-output-to-string (s) (print-separated-strings s "|" '("ab")) )
  "ab")

(deftest :pss.2
    (with-output-to-string (s) (print-separated-strings s "|" '("ab" "cd")))
    "ab|cd")

(deftest :pss.3
    (with-output-to-string (s) (print-separated-strings s "|" '("ab" "cd") nil))
    "ab|cd")

(deftest :pss.4
    (with-output-to-string (s)
      (print-separated-strings s "|" '("ab" "cd") nil nil))
    "ab|cd")

(deftest :pss.5
    (with-output-to-string (s)
      (print-separated-strings s "|" '("ab" "cd") nil '("ef") nil))
    "ab|cd|ef")

(deftest :css.0 (concat-separated-strings "|" nil) "")
(deftest :css.1 (concat-separated-strings "|" nil nil) "")
(deftest :css.2 (concat-separated-strings "|" '("ab")) "ab")
(deftest :css.3 (concat-separated-strings "|" '("ab" "cd")) "ab|cd")
(deftest :css.4 (concat-separated-strings "|" '("ab" "cd") nil) "ab|cd")
(deftest :css.5 (concat-separated-strings "|" '("ab" "cd") nil '("ef")) "ab|cd|ef")

(deftest :f.1 (map-and-remove-nils #'(lambda (x) (when (oddp x) (* x x)))
                     '(0 1 2 3 4 5 6 7 8 9)) (1 9 25 49 81))
(deftest :f.2 (filter #'(lambda (x) (when (oddp x) (* x x)))
                     '(0 1 2 3 4 5 6 7 8 9)) (1 3 5 7 9))
(deftest :an.1 (appendnew '(a b c d) '(c c e f)) (a b c d e f))


(deftest :pxml.1
  (xml-tag-contents "tag1" "<tag>Test</tag>")
  nil nil nil)

(deftest :pxml.2
  (xml-tag-contents "tag" "<tag>Test</tag>")
  "Test" 15 nil)

(deftest :pxml.3
  (xml-tag-contents "tag" "<tag  >Test</tag>")
  "Test" 17 nil)

(deftest :pxml.4
    (xml-tag-contents "tag" "<tag a=\"b\"></tag>")
  "" 17 ("a=\"b\""))

(deftest :pxml.5
    (xml-tag-contents "tag" "<tag a=\"b\" >Test</tag>")
  "Test" 22 ("a=\"b\""))

(deftest :pxml.6
    (xml-tag-contents "tag" "<tag a=\"b\"  c=\"ab\">Test</tag>")
  "Test" 29 ("a=\"b\"" "c=\"ab\""))

(deftest :pxml.7
    (xml-tag-contents "tag" "<taga a=\"b\"  c=\"ab\">Test</taga>")
  nil nil nil)

(deftest :pxml.8
    (xml-tag-contents "tag" "<taga a=\"b\"  c=\"ab\">Test<tag>ab</tag></taga>")
  "ab" 37 nil)

(deftest :pxml.9
    (xml-tag-contents "tag" "<taga a=\"b\"  c=\"ab\">Test<tag>ab</ag></taga>")
  nil nil nil)

(deftest :fss.1 (fast-string-search "" "" 0 0 0) 0)
(deftest :fss.2 (fast-string-search "" "abc" 0 0 2) 0)
(deftest :fss.3 (fast-string-search "abc" "" 3 0 0) nil)
(deftest :fss.4 (fast-string-search "abc" "abcde" 3 0 4) 0)
(deftest :fss.5 (fast-string-search "abc" "012abcde" 3 0 7) 3)
(deftest :fss.6 (fast-string-search "abc" "012abcde" 3 0 7) 3)
(deftest :fss.7 (fast-string-search "abc" "012abcde" 3 3 7) 3)
(deftest :fss.8 (fast-string-search "abc" "012abcde" 3 4 7) nil)
(deftest :fss.9 (fast-string-search "abcde" "012abcde" 5 3 8) 3)
(deftest :fss.9b (cl:search "abcde" "012abcde" :start2 3 :end2 8) 3)
(deftest :fss.10 (fast-string-search "abcde" "012abcde" 5 3 7) nil)
(deftest :fss.10b (cl:search "abcde" "012abcde" :start2 3 :end2 7) nil)

(deftest :stlsd.1 (string-to-list-skip-delimiter "") ())
(deftest :stlsd.2 (string-to-list-skip-delimiter "abc") ("abc"))
(deftest :stlsd.3 (string-to-list-skip-delimiter "ab c") ("ab" "c"))
(deftest :stlsd.4 (string-to-list-skip-delimiter "ab  c") ("ab" "c"))
(deftest :stlsd.5 (string-to-list-skip-delimiter "ab   c") ("ab" "c"))
(deftest :stlsd.6 (string-to-list-skip-delimiter "ab   c ") ("ab" "c"))
(deftest :stlsd.7 (string-to-list-skip-delimiter "  ab   c  ") ("ab" "c"))
(deftest :stlsd.8 (string-to-list-skip-delimiter "ab,,c" #\,) ("ab" "c"))
(deftest :stlsd.9 (string-to-list-skip-delimiter "ab,,c,," #\,) ("ab" "c"))
(deftest :stlsd.10 (string-to-list-skip-delimiter " ab") ("ab"))

(deftest :csc.1 (count-string-char "" #\a) 0)
(deftest :csc.2 (count-string-char "abc" #\d) 0)
(deftest :csc.3 (count-string-char "abc" #\b) 1)
(deftest :csc.4 (count-string-char "abcb" #\b) 2)

(deftest :duqs.1 (decode-uri-query-string "") "")
(deftest :duqs.2 (decode-uri-query-string "abc") "abc")
(deftest :duqs.3 (decode-uri-query-string "abc+") "abc ")
(deftest :duqs.4 (decode-uri-query-string "abc+d") "abc d")
(deftest :duqs.5 (decode-uri-query-string "abc%20d") "abc d")

(deftest :sse.1 (string-strip-ending "" nil) "")
(deftest :sse.2 (string-strip-ending "abc" nil) "abc")
(deftest :sse.3 (string-strip-ending "abc" "ab") "abc")
(deftest :sse.4 (string-strip-ending "abc" '("ab")) "abc")
(deftest :sse.5 (string-strip-ending "abcd" '("a" "cd")) "ab")

(deftest :rcs.1 (remove-char-string #\space "") "")
(deftest :rcs.2 (remove-char-string #\space "a") "a")
(deftest :rcs.3 (remove-char-string #\space "ab") "ab")
(deftest :rcs.4 (remove-char-string #\space "a b") "ab")
(deftest :rcs.5 (remove-char-string #\space " a b") "ab")
(deftest :rcs.6 (remove-char-string #\space "a b ") "ab")
(deftest :rcs.7 (remove-char-string #\space "a  b   c  ") "abc")
(deftest :rcs.8 (remove-char-string #\space "a  b   c  d") "abcd")


(defun test-color-conversion ()
  (dotimes (ih 11)
    (dotimes (is 11)
      (dotimes (iv 11)
        (let ((h (* ih 30))
              (s (/ is 10))
              (v (/ iv 10)))
          (multiple-value-bind (r g b) (hsv->rgb h s v)
            (multiple-value-bind (h2 s2 v2) (rgb->hsv r g b)
              (unless (hsv-equal h s v h2 s2 v2)
                (warn "Colors not equal: ~4D ~4D ~4D | ~6D:~6D ~6D:~6D ~6D:~6D~%"
                        (float r) (float g) (float b)
                        (when (typep h 'number) (float h))
                        (when (typep h2 'number) (float h2))
                        (float s) (float s2) (float v) (float v2))
                (return-from test-color-conversion nil))))))))
  t)

(defun test-color-conversion-float-255 ()
  (dotimes (ih 11)
    (dotimes (is 11)
      (dotimes (iv 11)
        (let ((h (* ih 30))
              (s (/ is 10))
              (v (/ iv 10)))
          (multiple-value-bind (r g b) (hsv->rgb h s v)
            (setf r (round (* 255 r))
                  g (round (* 255 g))
                  b (round (* 255 b)))
            (multiple-value-bind (h2 s2 v2) (rgb255->hsv255 r g b)
              (unless (hsv-similar h s v h2 (/ s2 255) (/ v2 255)
                                   :hue-range 10 :saturation-range .1
                                   :value-range 1 :black-limit 0 :gray-limit 0)
                (warn "Colors not equal: ~4D ~4D ~4D | ~6D:~6D ~6D:~6D ~6D:~6D~%"
                      r g b
                      (when (typep h 'number) (float h))
                      (when (typep h2 'number) (float h2))
                      (float s) (float (/ s2 255)) (float v) (float (/ v2 255)))
                (return-from test-color-conversion-float-255 nil))))))))
  t)

(defun test-color-conversion-255-float ()
  (dotimes (ih 11)
    (dotimes (is 11)
      (dotimes (iv 11)
        (let ((h (* ih 30))
              (s (/ is 10))
              (v (/ iv 10)))
          (multiple-value-bind (r g b) (hsv255->rgb255 h (truncate (* 255 s))
                                                       (truncate (* 255 v)))
            (setf r (/ r 255)
                  g (/ g 255)
                  b (/ b 255))

            (multiple-value-bind (h2 s2 v2) (rgb->hsv r g b)
              (unless (hsv-similar h s v h2 s2 v2
                                   :hue-range 10 :saturation-range .1
                                   :value-range 1 :black-limit 0 :gray-limit 0)
                (warn "Colors not equal: ~4D ~4D ~4D | ~6D:~6D ~6D:~6D ~6D:~6D~%"
                      r g b
                      (when (typep h 'number) (float h))
                      (when (typep h2 'number) (float h2))
                      (float s) (float (/ s2 255)) (float v) (float (/ v2 255)))
                (return-from test-color-conversion-255-float nil))))))))
  t)

(defun test-color-conversion-255 ()
  (dotimes (ih 11)
    (dotimes (is 11)
      (dotimes (iv 11)
        (let ((h (* ih 30))
              (s (truncate (* 255 (/ is 10))))
              (v (truncate (* 255 (/ iv 10)))))
          (multiple-value-bind (r g b) (hsv255->rgb255 h s v)
            (multiple-value-bind (h2 s2 v2) (rgb255->hsv255 r g b)
              (unless (hsv255-similar h s v h2 s2 v2 :hue-range 10 :saturation-range 5
                                      :value-range 5 :black-limit 0 :gray-limit 0)
                (warn "Colors not equal: ~D ~D ~D |~
 ~3,'0D:~3,'0D ~3,'0D:~3,'0D ~3,'0D:~3,'0D~%"
                      r g b
                      h h2 s s2 v v2)
                (return-from test-color-conversion-255 nil))))))))
  t)

(deftest :color.conv (test-color-conversion) t)
(deftest :color.conv.float.255 (test-color-conversion-float-255) t)
(deftest :color.conv.255.float (test-color-conversion-255-float) t)
(deftest :color.conv.255 (test-color-conversion-255) t)

(deftest :hue.diff.1 (hue-difference 10 10) 0)
(deftest :hue.diff.2 (hue-difference 10 9) -1)
(deftest :hue.diff.3 (hue-difference 9 10) 1)
(deftest :hue.diff.4 (hue-difference 10 nil) 360)
(deftest :hue.diff.5 (hue-difference nil 1) 360)
(deftest :hue.diff.7 (hue-difference 10 190) 180)
(deftest :hue.diff.8 (hue-difference 190 10) -180)
(deftest :hue.diff.9 (hue-difference 1 359) -2)
(deftest :hue.diff.10 (hue-difference 1 182) -179)
(deftest :hue.diff.11 (hue-difference 1 270) -91)

(deftest :hsv.sim.1 (hsv-similar 100 .5 .5 110 .5 .5 :hue-range 5
                                :value-range 0 :saturation-range 0
                                :black-limit 0 :gray-limit 0) nil)
(deftest :hsv.sim.2 (hsv-similar 100 .5 .5 110 .5 .5 :hue-range 15
                                :value-range 0 :saturation-range 0
                                :black-limit 0 :gray-limit 0) t)
(deftest :hsv.sim.3 (hsv-similar 100 .5 .5 110 .5 .6 :hue-range 15
                                :value-range .2 :saturation-range 0
                                :black-limit 0 :gray-limit 0) t)
(deftest :hsv.sim.4 (hsv-similar 100 .5 .5 110 .5 .8 :hue-range 15
                                :value-range 0.2 :saturation-range 0
                                :black-limit 0 :gray-limit 0) nil)
(deftest :hsv.sim.5 (hsv-similar 100 .5 .5 110 .6 .6 :hue-range 15
                                :value-range 0.2 :saturation-range .2
                                :black-limit 0 :gray-limit 0) t)
(deftest :hsv.sim.6 (hsv-similar 100 .5 .5 110 .6 .8 :hue-range 15
                                :value-range 0.2 :saturation-range .2
                                :black-limit 0 :gray-limit 0) nil)
(deftest :hsv.sim.7 (hsv-similar 100 .5 .05 110 .6 .01 :hue-range 0
                                :value-range 0 :saturation-range 0
                                :black-limit .1 :gray-limit 0) t)
(deftest :hsv.sim.8 (hsv-similar 100 .01 .5 110 .09 .6 :hue-range 0
                                :value-range 0.2 :saturation-range 0
                                :black-limit 0 :gray-limit .1) t)
(deftest :hsv.sim.9 (hsv-similar 100 .01 .5 110 .09 .6 :hue-range 0
                                :value-range 0.05 :saturation-range 0
                                :black-limit 0 :gray-limit .1) nil)

#+ignore
(progn
(deftest :dst.1
    (is-dst-change-usa-spring-utime
     (encode-universal-time 0 0 0 2 4 2000)) t)
(deftest :dst.2
    (is-dst-change-usa-spring-utime
     (encode-universal-time 0 0 0 1 4 2000)) nil)
(deftest :dst.3
    (is-dst-change-usa-spring-utime
     (encode-universal-time 0 0 0 3 4 2000)) nil)
(deftest :dst.4
    (is-dst-change-usa-fall-utime
     (encode-universal-time 0 0 0 31 10 2004)) t)
(deftest :dst.5
    (is-dst-change-usa-fall-utime
     (encode-universal-time 0 0 0 30 10 2004)) nil)
(deftest :dst.6
    (is-dst-change-usa-fall-utime
     (encode-universal-time 0 0 0 1 11 2000)) nil)
)


(deftest :ekdc.1
    (ensure-keyword-default-case (read-from-string "TYPE")) :type)

(deftest :ekdc.2
    (ensure-keyword-default-case (read-from-string "type")) :type)


(deftest :se.1
    (string-elide "A Test string" 10 :end) "A Test ..." )

(deftest :se.2
    (string-elide "A Test string" 13 :end) "A Test string")

(deftest :se.3
    (string-elide "A Test string" 11 :end) "A Test s..." )

(deftest :se.4
    (string-elide "A Test string" 2 :middle) "...")

(deftest :se.5
    (string-elide "A Test string" 11 :middle) "A Te...ring")

(deftest :se.6
    (string-elide "A Test string" 12 :middle) "A Tes...ring")

(deftest :url.1
    (make-url "pg")
  "pg")

(deftest :url.2
    (make-url "pg" :anchor "now")
  "pg#now")

(deftest :url.3
    (make-url "pg" :vars '(("a" . "5")))
  "pg?a=5")

(deftest :url.4
    (make-url "pg" :anchor "then" :vars '(("a" . "5") ("b" . "pi")))
  "pg?a=5&b=pi#then")

(defclass test-unique ()
  ((a :initarg :a)
   (b :initarg :b)))


(deftest :unique.1
    (let ((list (list (make-instance 'test-unique :a 1 :b 1)
                      (make-instance 'test-unique :a 2 :b 2)
                      (make-instance 'test-unique :a 3 :b 2))))
      (values
       (unique-slot-values list 'a)
       (unique-slot-values list 'b)))
  (1 2 3) (1 2))

(deftest :unique.2
    (unique-slot-values nil 'a)
  nil)

(deftest :nwp.1
       (numbers-within-percentage 1. 1.1 9)
  nil)

(deftest :nwp.2
       (numbers-within-percentage 1. 1.1 11)
  t)

(deftest :pfs.1 (prefixed-fixnum-string 0 #\A 5) "A00000")

(deftest :pfs.2 (prefixed-fixnum-string 1 #\A 5) "A00001")

(deftest :pfs.3 (prefixed-fixnum-string 21 #\B 3) "B021")

(deftest :pis.4 (prefixed-integer-string 234134 #\C 7) "C0234134")

 ;;; MOP Testing

;; Disable attrib class until understand changes in sbcl/cmucl
;; using COMPUTE-SLOT-ACCESSOR-INFO and defining method
;; for slot access of ALL-ATTRIBUTES. Does this work on Allegro/LW?

#+ignore
(progn
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (find-package '#:kmr-mop)
    (pushnew :kmrtest-mop cl:*features*)))

#+kmrtest-mop
(setf (find-class 'monitored-credit-rating) nil)
#+kmrtest-mop
(setf (find-class 'credit-rating) nil)

#+kmrtest-mop
(defclass credit-rating ()
  ((level :attributes (date-set time-set))
   (id :attributes (person-setting)))
  #+lispworks (:optimize-slot-access nil)
  (:metaclass attributes-class))


#+kmrtest-mop
(defclass monitored-credit-rating ()
  ((level :attributes (last-checked interval date-set))
   (cc :initarg :cc)
   (id :attributes (verified)))
  (:metaclass attributes-class))

#+kmrtest-mop
(deftest :attrib.mop.1
         (let ((cr (make-instance 'credit-rating)))
           (slot-attribute cr 'level 'date-set))
         nil)

#+kmrtest-mop
(deftest :attrib.mop.2
         (let ((cr (make-instance 'credit-rating)))
           (setf (slot-attribute cr 'level 'date-set) "12/15/1990")
           (let ((result (slot-attribute cr 'level 'date-set)))
             (setf (slot-attribute cr 'level 'date-set) nil)
             result))
         "12/15/1990")

#+kmrtest-mop
(deftest :attrib.mop.3
         (let ((mcr (make-instance 'monitored-credit-rating)))
           (setf (slot-attribute mcr 'level 'date-set) "01/05/2002")
           (let ((result (slot-attribute mcr 'level 'date-set)))
             (setf (slot-attribute mcr 'level 'date-set) nil)
             result))
         "01/05/2002")


#+kmrtest-mop
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setq cl:*features* (delete :kmrtest-mop cl:*features*)))

) ;; progn
