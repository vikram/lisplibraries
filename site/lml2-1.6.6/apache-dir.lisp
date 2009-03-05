;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          lml2.asd
;;;; Purpose:       ASDF definition file for Lisp Markup Language Version 2
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Aug 2002
;;;;
;;;; $Id: lml2.asd 7061 2003-09-07 06:34:45Z kevin $
;;;;
;;;; This file, part of LML2, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;;
;;;; LML2 users are granted the rights to distribute and use this software
;;;; as governed by the terms of the GNU General Public License v2
;;;; (http://www.gnu.org/licenses/gpl.html)
;;;; *************************************************************************

(in-package #:lml2)

(defparameter *apache-name-width* 24)

(defun write-name-trailing-spaces (stream name)
  (let* ((spaces (- *apache-name-width* (length name))))
    (when (plusp spaces)
      (print-n-chars #\space spaces stream))))

(defun write-name-link (stream link name)
  (html-stream
   stream
   ((:a :href link) (:princ (string-maybe-shorten name *apache-name-width*))))
  (write-name-trailing-spaces stream name))

(defun universal-time-to-apache-date (utime)
  (multiple-value-bind
        (second minute hour day-of-month month year day-of-week daylight-p zone)
      (decode-universal-time utime)
    (declare (ignore second day-of-week daylight-p zone))
    (format nil
            (formatter "~2,'0D-~3/kmrcl::monthname/-~4,'0D ~2,'0D:~2,'0D")
            day-of-month month year hour minute)))

(defun sort-dir-entries (entries sort-field direct)
  (case sort-field
    (:name
     (sort entries
           (lambda (a b)
             (funcall (if (eq direct :asc) #'string-lessp #'string-greaterp)
                      (aif (third a) it "")
                      (aif (third b) it "")))))
    (:modified
     (sort entries
           (lambda (a b)
             (funcall (if (eq direct :asc) #'< #'>)
                      (aif (fourth a) it 0)
                      (aif (fourth b) it 0)))))
    (:size
     (sort entries
           (lambda (a b)
             (funcall (if (eq direct :asc) #'< #'>)
                      (aif (fifth a) it 0)
                      (aif (fifth b) it 0)))))
    (:description
     (sort entries
           (lambda (a b)
             (funcall (if (eq direct :asc) #'string-lessp #'string-greaterp)
                      (aif (sixth a) it "")
                      (aif (sixth b) it "")))))
    (t
     entries)))

(defun write-html-apache-directory (stream title entries this-url &key parent address query-string
                                    icon-base)
  (let* ((query (when query-string (split-uri-query-string query-string)))
         (sort-field (if query
                         (cond
                           ((string-equal (caar query) "N") :name)
                           ((string-equal (caar query) "M") :modified)
                           ((string-equal (caar query) "S") :size)
                           ((string-equal (caar query) "D") :description)
                           (t :name))
                         :name))
         (dir (cond
                ((and query (string-equal (cdr (first query)) "D") :desc))
                (t :asc))))
    (setq entries (sort-dir-entries entries sort-field dir))

    (html-stream
     stream
     "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0 Transitional//EN\" \"http://www.w3.org/TR/REC-html40/loose.dtd\">"
     :newline
     (:html
      :newline
      (:head
       :newline
       (:title (:princ title)))
      :newline
      ((:body :bgcolor "#FFFFFF" :text "#000000")
       :newline
       (:table
        (:tr
         ((:td :bgcolor "#FFFFFF" :class "title")
          ((:font :size "+3" :face "Hevetica,Arial,sans-serif")
           (:b (:princ title))))))
       :newline
       (:pre
        (when icon-base
          (html-stream
           stream
           ((:img :border "0"
                  :src (format nil "~Ablank.png" icon-base)
                  :alt "     "))))
        " "
        ((:a :href (format nil "~A?N=~A" this-url
                           (if (and (eq sort-field :name) (eq dir :asc))
                               "D" "A")))
         "Name")
        (:princ (format nil "~20A" ""))
        " "
        ((:a :href (format nil "~A?M=~A" this-url
                           (if (and (eq sort-field :modified) (eq dir :asc))
                               "D" "A")))
         "Last modified")
        "      "
        ((:a :href (format nil "~A?S=~A" this-url
                           (if (and (eq sort-field :size) (eq dir :asc))
                               "D" "A")))
         "Size")
        "   "
        ((:a :href (format nil "~A?D=~A" this-url
                           (if (and (eq sort-field :description) (eq dir :asc))
                               "D" "A")))
         "Description")
        :newline
        (:princ "<hr noshade align=\"left\" width=\"80%\">")
        :newline
        (when parent
          (html-stream
           stream
           (when icon-base
             (html-stream
              stream
              ((:img :border "0"
                     :src (format nil "~Aback.png" icon-base)
                     :alt "[DIR]"))))
           " "
           (write-name-link stream (first parent) (second parent))
           " "
           (print-n-chars #\space 17 stream)
           "     -"
           :newline))
        (dolist (entry entries)
          (html-stream
           stream
           (when icon-base
             (html-stream
              stream
              ((:img :border "0"
                     :src
                     (case (car entry)
                       (:dir (format nil "~Afolder.png" icon-base))
                       (:text (format nil "~Atext.png" icon-base))
                       (t (format nil "~Af.png" icon-base)))
                     :alt
                     (case (car entry)
                       (:dir "[DIR]")
                       (:text "[TXT]")
                       (t "[FIL]"))))))
           " "
           (write-name-link stream (second entry) (third entry))
           " "
           (:princ (universal-time-to-apache-date (fourth entry)))
           (:princ
            (cond
              ((or (eq :dir (first entry))
                   (null (fifth entry)))
               "     -")
              ((< (fifth entry) (* 1024 1024))
               (format nil "~5,' Dk" (round (fifth entry) 1024)))
              ((< (fifth entry) (* 1024 1024 1024))
               (format nil "~5,' Dm" (round (fifth entry) (* 1024 1024))))
              (t
               (format nil "~5,' Dg" (round (fifth entry) (* 1024 1024 1024))))
              ))
           " "
           (:princ
            (if (sixth entry)
                (sixth entry)
                ""))
           :newline)))
       (:princ "<hr noshade align=\"left\" width=\"80%\">")
       :newline
       (when address
         (html-stream
          stream
          (:address address))))))))

