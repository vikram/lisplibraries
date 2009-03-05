;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10; encoding: utf-8 -*-
;; See the file LICENCE for licence information.
(in-package #:cl-l10n)

(defparameter *hungarian-plural-overrides*
  (read-key->value-text-file-into-hashtable
   (merge-pathnames (make-pathname :directory '(:relative "languages")
                                   :name "hungarian-plural-overrides"
                                   :type "text")
                    (asdf:component-pathname (asdf:find-system :cl-l10n)))))


(defun hungarian-plural-of (word &optional (uppercase nil uppercase-provided-p))
  "Returns the hungarian plural of the given word."
  (declare (type (simple-array character) word)
           (optimize (speed 3) (debug 0)))
  (awhen (gethash word *hungarian-plural-overrides*)
    (return-from hungarian-plural-of
      (if uppercase
          (string-upcase it)
          it)))
  (let ((length (length word)))
    (when (< length 3)
      (return-from hungarian-plural-of
        (if uppercase
            (string-upcase word)
            word)))
    (let* ((original-last-letter (elt word (1- length)))
           (last-letter (char-downcase original-last-letter))
           (last-letter2 (char-downcase (elt word (- length 2))))
           (last-letter3 (char-downcase (elt word (- length 3)))))
      (unless uppercase-provided-p
        (setf uppercase (upper-case-p original-last-letter)))
      (macrolet ((all-but-last (&optional (count 1))
                   `(subseq word 0 (- length ,count)))
                 (emit (body &rest pieces)
                   `(return-from hungarian-plural-of
                     (concatenate 'string ,body
                      ,@(iter (for piece in pieces)
                              (collect `(if uppercase
                                         (string-upcase ,piece)
                                         ,piece)))))))
        (if (vowelp last-letter)
            (cond ((eq last-letter #\e) (emit (all-but-last) "ék"))
                  ((eq last-letter #\a) (emit (all-but-last) "ák"))
                  #+nil((and (eq last-letter #\i) (melleknev -i kepzovel)) (emit word "ek"))
                  (t (emit word "k")))
            (when-bind last-vowel (last-vowel-of word)
              (when (eq #\k last-letter)
                (emit word "ok"))
              ;; handle -zat,-zet,-zás,-zés
              (when (and (eq #\z last-letter3)
                         (or (and (or (eq #\a last-letter2)
                                      (eq #\e last-letter2))
                                  (eq #\t last-letter))
                             (and (or (eq #.(code-char 225) last-letter2)
                                      (eq #.(code-char 233) last-letter2))
                                  (eq #\s last-letter))))
                (emit word (if (or (eq #\a last-letter2)
                                   (eq #.(code-char 225) last-letter2))
                               "ok"
                               "ek")))
              ;; handle -at
              (when (and (eq #\a last-letter2)
                         (eq #\t last-letter))
                (emit word "ok"))
              (if (high-vowel-p last-vowel)
                  (cond ((eq last-vowel #.(code-char 246))
                         (emit word "ök"))
                        (t (emit word "ek")))
                  (cond ((eq last-vowel #\o)
                         (emit word "ok"))
                        (t (emit word "ak"))))))
        (emit word "-k")))))
