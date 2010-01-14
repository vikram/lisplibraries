;;; Authored by Frank Duncan (frank@nekthuth.com), Copyright 2008
;;;
;;; This software is licensed under the Library General Public License
;;; as extended to be more applicable to Lisp.  See for more information:
;;;
;;; http://opensource.franz.com/preamble.html
;;;
;;; Unless required by applicable law or agreed to in writing, software distributed
;;; under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
;;; CONDITIONS OF ANY KIND, either express or implied. See the License for the
;;; specific language governing permissions and limitations under the License. 
;;;
(in-package :nekthuth)

(eval-when (:compile-toplevel :load-toplevel :execute)
 (require 'sb-introspect))

(defvar *hyperspec-location* nil)

(defmacro reg-rep-all (look-for repl str)
 `(cl-ppcre:regex-replace-all ,look-for ,str ,repl))

(defun get-html (addr)
 (html-parse:parse-html
  (with-open-file (str addr)
   (let
    ((return-val (make-string (file-length str))))
    (read-sequence return-val str)
    return-val))))

(defmacro hs-html-helper (html &key hr-reached in-pre in-a)
 `(hs-html-to-help ,html
   ,@(if hr-reached (list :hr-reached hr-reached) (list :hr-reached 'hr-reached))
   ,@(if in-a (list :in-a in-a) (list :in-a 'in-a))
   ,@(if in-pre (list :in-pre in-pre) (list :in-pre 'in-pre))))

(defun convert-string (html in-pre)
 (let ((retn (if (position #\& html)
                 (reg-rep-all "\&gt;" ">"
                  (reg-rep-all "\&lt;" "<"
                   (reg-rep-all "\&quot;" "\""
                     (reg-rep-all "\&amp;" "&" html))))
                 html)))
  (if in-pre
      retn
      (reg-rep-all "\\n" ""
       (reg-rep-all "   *" "  " retn)))))


(defun hs-html-to-help-wrap (html)
 (format nil "             For Vim version 7.1.  Last change: 2008 Jul 28

                       HYPERSPEC TRANSFORMED

This is a runtime translation of the Common Lisp Hyperspec to a format that
is easier to read in vim.  All of the documentation is copyright LispWorks
Ltd. and you can find more information at: >

  http://www.lispworks.com/documentation/HyperSpec/Front/Help.htm#Legal

=============================================================================

  ~A
=============================================================================

 Copyright 1996-2005, LispWorks Ltd. All rights reserved."
  (reg-rep-all "\\n\\n\\n*" (format nil "~%~%") (hs-html-to-help html))))

(defun hs-html-to-help (html &key hr-reached in-pre in-a)
 (cond
  ((consp html)
   (cond
    ((and (consp (car html)) (eql :a (caar html)))
     (cond
      ((cl-ppcre:scan "glo_" (getf (cdar html) :href)) (hs-html-helper (cdr html) :in-a t))
      ((getf (cdar html) :href) (format nil "|~A|" (hs-html-helper (cdr html) :in-a t)))
      (t (hs-html-helper (cdr html) :in-a t))))
    (t
     (case (car html)
      (:!doctype "")
      (:comment "")
      (:rel (hs-html-helper (cddr html)))
      (:name (hs-html-helper (cddr html)))
      (:p (format nil "~A~%~%" (hs-html-helper (cdr html))))
      (:head "")
      (:h1 "")
      (:b (if in-a (hs-html-helper (cdr html)) (format nil "*~A*" (hs-html-helper (cdr html)))))
      (:bgcolor (hs-html-helper (cddr html)))
      (:pre (format nil "~A~%" (hs-html-helper (cdr html) :in-pre t)))
      (:hr (if hr-reached "" (hs-html-helper (cdr html) :hr-reached t)))
      (t (format nil "~A~A"
          (hs-html-helper (car html))
          (hs-html-helper (cdr html))))))))
  ((not html) "")
  ((symbolp html) "")
  ((stringp html)
   (convert-string html in-pre))
  (t html)))

(defun do-help (msg)
 (cond
  ((not *hyperspec-location*) "Error: nekthuth:*hyperspec-location* not set")
  ((not (probe-file (merge-pathnames #p"HyperSpec/Front/X_AllSym.htm" (pathname *hyperspec-location*))))
    (format nil "Error: nekthuth:*hyperspec-location* invalid (~A)" *hyperspec-location*))
  (t (hs-html-to-help-wrap
      (get-html 
       (format nil "~A/HyperSpec/Body/~A" *hyperspec-location* msg))))))

(register-command #\H #'do-help)

(register-command #\M (lambda (msg) (format nil "~S" (macroexpand-1 msg))))

(defun substr= (test possible)
 (and
  (>= (length possible) (length test))
  (string= test (subseq possible 0 (length test)))))

(defun string=-via-hyphens (test possible)
 (let
  ((test-hyphen (position #\- test))
   (poss-hyphen (position #\- possible)))
  (if test-hyphen
      (if (not poss-hyphen)
          nil
          (when (substr= (subseq test 0 test-hyphen) possible)
                (string=-via-hyphens (subseq test (1+ test-hyphen) (length test))
                                     (subseq possible (1+ poss-hyphen) (length possible))))) (substr= test possible))))

(defvar *external-symbol* (cl-ppcre:create-scanner "^([^:]*):([^:]*)$"))

(defun find-completes (start)
 (if (cl-ppcre:scan *external-symbol* start)
     (cl-ppcre:register-groups-bind (pkg-str symb-str) (*external-symbol* start)
      (let
       ((symbols nil))
       (mapit
        (do-external-symbols (symb it)
         (when (and (string=-via-hyphens (string-upcase pkg-str) (package-name it))
                    (string=-via-hyphens (string-upcase symb-str) (symbol-name symb)))
               (setf symbols (cons symb symbols))))
        (list-all-packages))
       (format nil "[~{~('~A'~)~^, ~}]" (mapit (format nil "~A:~A" (package-name (symbol-package it)) it) symbols))))
     (format nil "[~{~{{'word': ~('~A'~), 'menu': '~A'}~}~^, ~}]"
      (mapit
       (list it (package-name (symbol-package it)))
       (remove-if-not
        (lambda (item)
         (string=-via-hyphens (string-upcase start) (symbol-name item)))
        (get-all-symbols))))))

(register-command #\C #'find-completes)

(defun find-source-location (symb)
 (let
  ((source-loc (ignore-errors  ; Ignore reader errors for misspelled packages
                (or (car (sb-introspect:find-definition-sources-by-name symb :function))
                    (car (sb-introspect:find-definition-sources-by-name symb :macro))))))
  (cond
   ((not source-loc) (format nil "'Could not find definition for symbol ~A.'" symb))
   ((not (sb-introspect:definition-source-pathname source-loc)) (format nil "'Symbol ~A seems to have been defined in the top-level, not from a compiled file, therefore could not be found.'" symb))
   (t (format nil "(~S, ~A)" (namestring (translate-logical-pathname (sb-introspect:definition-source-pathname source-loc)))
                             (or (sb-introspect:definition-source-character-offset source-loc) 0))))))

(register-command #\L #'find-source-location)
