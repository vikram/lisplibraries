(in-package kmrcl)

(defpackage docbook
  (:use #:cl #:cl-who #:kmrcl)
  (:export
   #:docbook-file
   #:docbook-stream
   #:xml-file->sexp-file
   ))
(in-package docbook)

(defmacro docbook-stream (stream tree)
  `(progn
     (print-prologue ,stream)
     (write-char #\Newline ,stream)
     (let (cl-who::*indent* t)
       (cl-who:with-html-output (,stream) ,tree))))

(defun print-prologue (stream)
  (write-string "<?xml version='1.0' ?>   <!-- -*- DocBook -*- -->" stream)
  (write-char #\Newline stream)
  (write-string "<!DOCTYPE book PUBLIC \"-//OASIS//DTD DocBook XML V4.2//EN\"" stream)
  (write-char #\Newline stream)
  (write-string "     \"http://www.oasis-open.org/docbook/xml/4.2/docbookx.dtd\" [" stream)
  (write-char #\Newline stream)
  (write-string "<!ENTITY % myents SYSTEM \"entities.xml\">" stream)
  (write-char #\Newline stream)
  (write-string "%myents;" stream)
  (write-char #\Newline stream)
  (write-string "]>" stream)
  (write-char #\Newline stream))

(defmacro docbook-file (name tree)
  (let ((%name (gensym)))
    `(let ((,%name ,name))
      (with-open-file (stream ,%name :direction :output
                       :if-exists :supersede)
        (docbook-stream stream ,tree))
      (values))))

#+allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'pxml)
  (require 'uri))

(defun is-whitespace-string (s)
  (and (stringp s)
       (kmrcl:is-string-whitespace s)))

(defun atom-processor (a)
  (when a
    (typecase a
      (symbol
       (nth-value 0 (kmrcl:ensure-keyword a)))
      (string
       (kmrcl:collapse-whitespace a))
      (t
       a))))

(defun entity-callback (var token &optional public)
  (declare (ignore token public))
  (cond
   ((and (net.uri:uri-scheme var)
         (string= "http" (net.uri:uri-scheme var)))
    nil)
   (t
    (let ((path (net.uri:uri-path var)))
      (if (probe-file path)
          (ignore-errors (open path))
        (make-string-input-stream
         (let ((*print-circle* nil))
           (format nil "<!ENTITY ~A '~A'>" path path))))))))

#+allegro
(defun xml-file->sexp-file (file &key (preprocess nil))
  (let* ((path (etypecase file
                 (string (parse-namestring file))
                 (pathname file)))
         (new-path (make-pathname :defaults path
                                  :type "sexp"))
         raw-sexp)

    (if preprocess
        (multiple-value-bind (xml error status)
            (kmrcl:command-output (format nil
                                          "sh -c \"export XML_CATALOG_FILES='~A'; cd ~A; xsltproc --xinclude pprint.xsl ~A\""
                                          "catalog-debian.xml"
                                          (namestring (make-pathname :defaults (if (pathname-directory path)
                                                                                   path
                                                                                 *default-pathname-defaults*)
                                                                     :name nil :type nil))
                                          (namestring path)))
          (unless (and (zerop status) (or (null error) (zerop (length error))))
            (error "Unable to preprocess XML file ~A, status ~D.~%Error: ~A"
                   path status error))
          (setq raw-sexp (net.xml.parser:parse-xml
                          (apply #'concatenate 'string xml)
                          :content-only nil)))
      (with-open-file (input path :direction :input)
        (setq raw-sexp (net.xml.parser:parse-xml input :external-callback #'entity-callback))))

    (with-open-file (output new-path :direction :output
                     :if-exists :supersede)
      (let ((filtered (kmrcl:remove-from-tree-if #'is-whitespace-string
                                                 raw-sexp
                                                 #'atom-processor)))
        (write filtered :stream output :pretty t))))
  (values))


