(defpackage :closure-common-system
  (:use :asdf :cl)
  (:export #:*utf8-runes-readtable*))

(in-package :closure-common-system)

(defvar *utf8-runes-readtable*)

(defclass closure-source-file (cl-source-file) ())

#+sbcl
(defmethod perform :around ((o compile-op) (s closure-source-file))
  ;; shut up already.  Correctness first.
  (handler-bind ((sb-ext:compiler-note #'muffle-warning))
    (let (#+sbcl (*compile-print* nil))
      (call-next-method))))

#-(or rune-is-character rune-is-integer)
(progn
  (format t "~&;;; Checking for wide character support...")
  (force-output)
  (pushnew (dotimes (x 65536
                      (progn
                        (format t " ok, characters have at least 16 bits.~%")
                        :rune-is-character))
             (unless (or (<= #xD800 x #xDFFF)
			 (and (< x char-code-limit) (code-char x)))
               (format t " no, reverting to octet strings.~%")
               (return :rune-is-integer)))
           *features*))

#-rune-is-character
(format t "~&;;; Building Closure with (UNSIGNED-BYTE 16) RUNES~%")

#+rune-is-character
(format t "~&;;; Building Closure with CHARACTER RUNES~%") 

(defsystem :closure-common
    :default-component-class closure-source-file
    :serial t
    :components
    ((:file "package")
     (:file "definline")
     (:file runes
            :pathname
             #-rune-is-character "runes"
             #+rune-is-character "characters")
     #+rune-is-integer (:file "utf8")
     (:file "syntax")
     #-x&y-streams-are-stream (:file "encodings")
     #-x&y-streams-are-stream (:file "encodings-data")
     #-x&y-streams-are-stream (:file "xstream")
     #-x&y-streams-are-stream (:file "ystream")
     #+x&y-streams-are-stream (:file #+scl "stream-scl")
     (:file "hax"))
    :depends-on (#-scl :trivial-gray-streams))
