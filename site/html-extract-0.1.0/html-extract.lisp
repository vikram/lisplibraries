;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/html-extract/html-extract.lisp,v 1.1.1.1 2005/09/22 22:09:22 edi Exp $

;;; Copyright (c) 2003, Dr. Edmund Weitz. All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package #:html-extract)

(defun html-extract ()
  "Reads a \(X)HTML document from *STANDARD-INPUT* and writes it back
to *STANDARD-OUTPUT* with tags and comments stripped and replaced by
whitespace.

This function aims to yield correct results for correct \(X)HTML input
and it also tries hard to never signal an error although it may warn
if it encounters syntax errors. It will NOT detect any possible error
nor is there any warranty that it will work correctly with faulty
input."
  (loop
    (read-while (lambda (c)
                  (and (char/= c #\<)
                       (char/= c #\&)))
                :write-through t)
    (peek-cond (peek-char)
      ((char= peek-char #\&)
       (read-char)
       (peek-cond (peek-char (warn "EOF after '&'")
                             (write-char #\&))
         ((char= peek-char #\#)
          ;; probably a character reference
          (read-char)
          (peek-cond (peek-char (warn "EOF after '&#'")
                                (write-string "&#"))
            (t
             (let* ((radix (cond ((char= peek-char #\x)
                                   (read-char)
                                   16)
                                 (t 10)))
                    (ref-string (read-while (lambda (c)
                                              (digit-char-p c radix))
                                            :skip nil)))
               (cond ((zerop (length ref-string))
                       ;; it wasn't a character
                       ;; reference - we just emit what
                       ;; we've read
                       (write-string "&#x")
                       (write-string ref-string))
                     (t
                       (let* ((char-code (parse-integer ref-string
                                                        :radix radix))
                              (char (and (< char-code 256)
                                         (code-char char-code))))
                         (flet ((no-semicolon ()
                                  (warn "Character reference '&#~A~A' without trailing semicolon"
                                        (if (= radix 16) "x" "")
                                        ref-string))
                                (emit-char ()
                                  (cond (char
                                          (write-char char))
                                        (t
                                          (warn "Skipping character with code #x~x"
                                                char-code)))))
                           (peek-cond (peek-char (no-semicolon) (emit-char))
                             ((char= peek-char #\;)
                              (read-char)
                              (emit-char))
                             (t
                              (no-semicolon)
                              (emit-char)))))))))))
         ((letterp peek-char)
          ;; a named entity (like '&nbsp;')
          (let* ((name (read-name :skip nil))
                 (value (cdr (assoc name *named-entities*
                                    :test #'string=))))
            (flet ((no-semicolon ()
                     (warn "Character reference '&~A' without trailing semicolon"
                           name))
                   (emit-char ()
                     (cond (value
                             (write-char value))
                           (t
                             (warn "Unknown entity '~A'" name)
                             (write-char #\&)
                             (write-string name)))))
              (peek-cond (peek-char (no-semicolon) (emit-char))
                ((char= peek-char #\;)
                 (read-char)
                 (emit-char))
                (t
                 (no-semicolon)
                 (emit-char))))))
         (t
          (warn "Treating '&' before character with code #x~x like '&amp;'"
                (char-code peek-char)
          (write-char #\&)))))
      ((char= peek-char #\<)
       (read-char)
       (peek-cond (peek-char)
         ((char= peek-char #\!)
          (read-char)
          ;; we've seen "<!" so this might be a markup declaration
          ;; peek at next char
          (peek-cond (peek-char)
            ((char= peek-char #\>)
             ;; "<!>" is nothing special, just write the
             ;; string and go back to the start of the loop
             (read-char)
             (write-string "<!>"))
            ((letterp peek-char)
             ;; a letter, so this should be something like
             ;; <!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2
             ;; Final//EN"> - we just check for names and
             ;; delimited strings separated by whitespace
             ;; until we see the next #\>
             (read-name)
             (skip-whitespace)
             (block parameter-loop
               (loop
                 (peek-cond (peek-char (warn "EOF in markup declaration"))
                   ((char= peek-char #\>)
                    ;; a #\> ends the markup
                    ;; declaration - exit the loop
                    (read-char)
                    (return-from parameter-loop))
                   ((or (letterp peek-char)
                        (char= peek-char #\')
                        (char= peek-char #\"))
                    ;; a delimiter or a letter, so
                    ;; we expect a delimited string
                    (read-delimited-string)
                    (skip-whitespace))
                   ((char= peek-char #\-)
                    ;; might be a comment
                    (read-char)
                    (peek-cond (peek-char (warn "EOF in markup declaration"))
                      ((char= peek-char #\-)
                       ;; a comment - skip it
                       (skip-comment))
                      (t
                       ;; something else - this is an error
                       ;; so we warn and exit the loop
                       (warn "Unexpected character with code #x~x in markup declaration"
                             (char-code peek-char))
                       (return-from parameter-loop))))
                   (t
                    ;; something else - this is an error
                    ;; so we warn and exit the loop
                    (warn "Unexpected character with code #x~x in markup declaration"
                          (char-code peek-char))
                    (return-from parameter-loop))))))
            ((char= peek-char #\-)
             ;; might be a comment
             (read-char)
             (block comment-loop
               (loop
                 (peek-cond (peek-char (warn "EOF in comment declaration"))
                   ((char= peek-char #\-)
                    (skip-comment)
                    (skip-whitespace)
                    (peek-cond (peek-char (warn "EOF in comment declaration"))
                      ((char= peek-char #\>)
                       ;; a #\> ends the comment
                       ;; declaration - exit the loop
                       (read-char)
                       (return-from comment-loop))
                      ((char= peek-char #\-)
                       ;; might be another comment - repeat
                       (read-char))
                      (t
                       ;; something else - this is an error
                       ;; so we warn and exit the loop
                       (warn "Unexpected character with code #x~x in comment declaration - leaving comment scope"
                             (char-code peek-char))
                       (return-from comment-loop))))
                   (t
                    ;; something else - this is an error
                    ;; so we warn and exit the loop
                    (warn "Unexpected character with code #x~x in comment declaration - leaving comment scope"
                          (char-code peek-char))
                    (write-char #\-)
                    (return-from comment-loop))))))
            (t
             ;; neither markup declaration nor comment declaration,
             ;; so this was just "<!"
             (read-char)
             (warn "Stray '<!' found")
             (write-string "<!"))))
         ((char= peek-char #\/)
          (read-char)
          (peek-cond (peek-char (warn "EOF in end-tag"))
            ((letterp peek-char)
             ;; a letter, so this is supposed to start a name -
             ;; read it and skip whitespace following it
             (let ((name (read-name :skip nil)))
               (skip-whitespace)
               (peek-cond (peek-char (warn "EOF after '</~A'" name)
                                     (write-string name))
                 ((char/= (peek-char*) #\>)
                  ;; we expect to see #\> here
                  (warn "Expected '#\>' after '</~A'" name))
                 (t
                  ;; end of end tag, just consume the #\>
                  (when (space-tag-p name)
                    (fresh-line))
                  (read-char)))))
            (t
             ;; not a letter, so this is an error -
             ;; we warn and ignore this
             (warn "Unexpected character with code #x~x after '</'"
                   (char-code peek-char)))))
         ((letterp peek-char)
          ;; a letter so we expect a start tag, possibly followed by
          ;; attributes
          (let ((name (read-name :skip nil)))
            (skip-whitespace)
            (block attribute-loop
              (loop
                (peek-cond (peek-char (warn "EOF before '~A' tag was closed" name))
                  ((char= peek-char #\>)
                   ;; end of tag - exit attribute loop
                   (read-char)
                   (when (space-tag-p name)
                     (fresh-line))
                   (return-from attribute-loop))
                  ((char= peek-char #\/)
                   ;; we've seen #\/, so this might be the XHTML way
                   ;; to end a stand-alone tag
                   (read-char)
                   (peek-cond (peek-char (warn "EOF after '/' in '~A' tag"
                                               name))
                     ((char= peek-char #\>)
                      ;; yes, it is - exit this loop
                      (read-char)
                      (when (space-tag-p name)
                        (fresh-line)))
                     (t
                      ;; no, it's not - so this is an error
                      (warn "Unexpected '/' in '~A' tag" name)))
                   ;; exit attribute loop in any case
                   (return-from attribute-loop))
                  ((letterp peek-char)
                   ;; a letter - this should start an attribute
                   (read-attribute)
                   (skip-whitespace))
                  (t
                   ;; an error - exit the attribute loop
                   (warn "Unexpected character with code #x~x after '<~A'"
                         (char-code peek-char) name)
                   (return-from attribute-loop)))))))
         (t
          ;; anything else means this is just #\<, no markup
          (write-char #\<)))))))

#+:build (html-extract)