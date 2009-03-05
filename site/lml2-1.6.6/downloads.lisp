;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          downloads.lisp
;;;; Purpose:       Generate downloads page
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Aug 2002
;;;;
;;;; $Id$
;;;;
;;;; This file, part of LML2, is Copyright (c) 2000-2003 by Kevin Rosenberg.
;;;; Rights of modification and redistribution are in the LICENSE file.
;;;;
;;;; *************************************************************************

(in-package #:lml2)


(defstruct dl-data base url name indent signed)

(defun list-files (files dl-data)
  "List files in a directory for downloading"
  ;;files.sort()
  (mapcar (lambda (f) (print-file f dl-data)) files))

(defun strip-dl-base (file base)
  (let ((fdir (pathname-directory file))
        (bdir (pathname-directory base)))
    (make-pathname
     :name (pathname-name file)
     :type (pathname-type file)
     :directory
     (when (> (length fdir) (length bdir))
       (append '(:absolute)
               (subseq fdir (length bdir) (length fdir)))))))

(defun print-file (file dl-data)
  (let ((size 0)
        (modtime (date-string (file-write-date file)))
        (basename (namestring
                   (make-pathname :name (pathname-name file)
                                  :type (pathname-type file))))
        (dl-name (strip-dl-base file (dl-data-base dl-data)))
        (sig-path (concatenate 'string (namestring file) ".asc")))
    (when (plusp (length basename))
      (with-open-file (strm file :direction :input)
                      (setq size (round (/ (file-length strm) 1024))))
      (lml-format "<a href=\"~A~A\">~A</a>"
                  (dl-data-url dl-data) dl-name basename)
      (lml-princ "<span class=\"modtime\">")
      (lml-format " (~A, <b>~:D <span style=\"font-size:90%;\">KB</span></b>)</span>" modtime size)
      (when (probe-file sig-path)
        (setf (dl-data-signed dl-data) t)
        (lml-format " [<a href=\"~A~A.asc\">Signature</a>]"
                    (dl-data-url dl-data) dl-name))
      (html :br))))

(defun display-header (name url)
  (lml-princ "<h1>Download</h1>")
  (lml-princ "<div class=\"mainbody\">")
  (lml-format "<h3>Browse ~A Download Site</h3>" name)
  (let ((*print-circle* nil))
    (lml-format "<a style=\"padding-left:20pt;\" href=\"~A\">~A</a>" url url)))

(defun display-footer (dl-data)
  (when (dl-data-signed dl-data)
    (lml-princ "<h3>GPG Public Key</h3>")
    (lml-princ "Use this <a href=\"https://www.b9.com/kevin.gpg.asc\">key</a> to verify file signtatures"))
  (lml-princ "</div>"))

(defun print-sect-title (title dl-data)
  (lml-format "<h~D>~A</h~D>"
              (dl-data-indent dl-data) title (dl-data-indent dl-data)))

(defun match-base-name? (name base-name)
  (let ((len-base-name (length base-name)))
    (when (>= (length name) len-base-name)
      (string= name base-name :end1 len-base-name :end2 len-base-name))))

(defun match-base-name-latest? (name base-name)
  (let* ((latest (concatenate 'string base-name "-latest"))
         (len-latest (length latest)))
    (when (>= (length name) len-latest)
      (string= name latest :end1 len-latest :end2 len-latest))))

(defun filter-against-base (files base-name)
  (delete-if-not
   (lambda (f) (match-base-name? (pathname-name f) base-name))
   files))

(defun filter-latest (files base-name)
  (delete-if
   (lambda (f) (match-base-name-latest? (pathname-name f) base-name))
   files))

(defun sort-pathnames (list)
  (sort list (lambda (a b) (string< (namestring a) (namestring b)))))

(defun display-one-section (title pat dl-data)
  (let ((files (sort-pathnames
                (filter-latest
                 (filter-against-base (directory pat) (dl-data-name dl-data))
                 (dl-data-name dl-data)))))
    (when files
      (print-sect-title title dl-data)
      (lml-princ "<div style=\"padding-left: 20pt;\">")
      (list-files files dl-data)
      (lml-princ "</div>"))))

(defun display-sections (sects dl-data)
  (when sects
    (let ((title (car sects))
          (value (cadr sects)))
      (if (consp title)
          (dolist (sect sects)
            (display-sections sect dl-data))
        (if (consp value)
            (progn
              (print-sect-title title dl-data)
              (incf (dl-data-indent dl-data))
              (display-sections value dl-data)
              (decf (dl-data-indent dl-data)))
          (display-one-section title value dl-data))))))

(defun display-page (pkg-name pkg-base dl-base dl-url giturl gitweb sects)
  (let ((dl-data (make-dl-data :indent 3
                               :base dl-base
                               :url dl-url
                               :name pkg-base
                               :signed nil)))
    (display-header pkg-name dl-url)
    (dolist (sect sects)
      (display-sections sect dl-data))
    (when giturl
      (lml-format "<h2>Git Repository</h2><tt>~A</tt>" giturl)
      (when gitweb
        (lml-format "&nbsp;&nbsp;[<a href=\"~A\">Browse</a>]" gitweb)))
    (display-footer dl-data)))

(defun std-dl-page (pkg-name pkg-base dl-base dl-url &optional giturl gitweb)
  (let ((base (parse-namestring dl-base)))
    (let ((tgz-path (make-pathname :defaults base :type "gz" :name :wild))
          (zip-path (make-pathname :defaults base :type "zip" :name :wild))
          (doc-path (make-pathname :defaults base :type "pdf" :name :wild)))
      (display-page pkg-name pkg-base dl-base dl-url giturl gitweb
                    `(("Manual" ,doc-path)
                      ("Source Code"
                       (("Unix (.tar.gz)" ,tgz-path)
                        ("Windows (.zip)" ,zip-path))))))))

(defun full-dl-page (pkg-name pkg-base dl-base dl-url &optional giturl gitweb)
  (let ((base (parse-namestring dl-base)))
    (let ((tgz-path (make-pathname :defaults base :type "gz" :name :wild))
          (zip-path (make-pathname :defaults base :type "zip" :name :wild))
          (doc-path (make-pathname :defaults base :type "pdf" :name :wild))
          (deb-path (merge-pathnames
                     (make-pathname :directory '(:relative "linux-debian")
                                    :type :wild :name :wild)
                     base))
          (rpm-path (merge-pathnames
                     (make-pathname :directory '(:relative "linux-rpm")
                                    :type :wild :name :wild)
                     base))
          (w32-path (merge-pathnames
                     (make-pathname :directory '(:relative "win32")
                                    :type :wild :name :wild)
                     base)))
      (display-page pkg-name pkg-base dl-base dl-url giturl gitweb
                    `(("Manual" ,doc-path)
                      ("Source Code"
                       (("Unix (.tar.gz)" ,tgz-path)
                        ("Windows (.zip)" ,zip-path)))
                      ("Binaries"
                       (("Linux Binaries"
                         (("Debian Linux" ,deb-path)
                          ("RedHat Linux" ,rpm-path)))
                        ("Windows Binaries" ,w32-path))))))))
