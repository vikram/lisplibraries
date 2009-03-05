(in-package :trivial-html-parser)

(defparameter *this-file*
  (load-time-value
   (or #.*compile-file-pathname* *load-pathname*)))

(defparameter *this-directory*
  (make-pathname :directory (pathname-directory *this-file*)))

(defparameter sgml::*simple-catalog*
  (loop :for (name . filename)
     :in '(("-//W3O//DTD W3 HTML 3.0//EN" . "dtd/HTML-3.0")
           ("NETSCAPE-Bookmark-file-1" . "dtd/NETSCAPE-Bookmark-file-1")
           ("-//W3C//ENTITIES Special//EN//HTML" . "dtd/Entities-Special")
           ("-//W3C//ENTITIES Symbols//EN//HTML" . "dtd/Entities-Symbols")
           ("-//W3C//ENTITIES Latin1//EN//HTML" . "dtd/Entities-Latin1")
           ("-//W3C//DTD HTML 4.0 Frameset//EN" . "dtd/DTD-HTML-4.0-Frameset")
           ("-//W3C//DTD HTML 4.0//EN" . "dtd/DTD-HTML-4.0")
           ("-//W3C//DTD HTML 4.0 Transitional//EN" . "dtd/DTD-HTML-4.0-Transitional"))
     :collect (cons name (merge-pathnames filename *this-directory*))))

(defparameter *html-dtd* (sgml:parse-dtd '(:public "-//W3C//DTD HTML 4.0 Frameset//EN")))

(defun parse-html (inputstr)
  "given a string, produce a sgml:pt, which would be your toplevel parse tree node"
  (let ((dtd *html-dtd*))
    (let ((input (cxml:make-xstream
                  (flexi-streams:make-in-memory-input-stream
                   (flexi-streams:string-to-octets
                    inputstr :external-format (flexi-streams:make-external-format :utf-8))))))
      (setf (sgml::a-stream-scratch input)
            (make-array #.(* 2 4096) :element-type 'runes:rune))
      (sgml::setup-code-vector input :utf-8)
      (let ((r (sgml:sgml-parse dtd input)))
        (sgml::post-mortem-heuristic dtd r)))))

(defun unbreak-utf8 (arr &key (start 0))
  "given an utf-8 string, fix a common trouble with it:
   namely broken non-breaking-space sequences not being prefixed by 194"
  (when (> (length arr) start)
    (let* ((pos (position 160 arr :start start))
           (rest-fixed (when pos (unbreak-utf8 arr :start (1+ pos)))))
      (if pos
          (concatenate 'vector (subseq arr start pos) #(194 160) rest-fixed)
          (subseq arr start)))))

(defun cxml-pt-to-lhtml (pt)
  "given a sgml:pt, produce a lispified parse tree composed of lists of form:
   (tag property-list children)"
  (labels ((f (x)
             (cond
               ((null x) nil)
               ((stringp x) x)
               ((> (length x) 0)
                (let ((r (flexi-streams:octets-to-string x :external-format (flexi-streams:make-external-format :utf-8 :little-endian t))))
                  (unless r
                    (f (unbreak-utf8 x)))
                  r))
               (t (format t "impossible happened: ~S~%" x))))
           (iterate (pt)
             (let* ((attrs (if (listp (sgml:pt-attrs pt))
                               (loop :for (name val) :on (sgml:pt-attrs pt) :by #'cddr
                                  :collect (list name (f val)))
                               (f (sgml:pt-attrs pt)))))
               (if (eq (sgml:pt-name pt) :pcdata)
                   (f (sgml:pt-cdata pt))
                   (cons
                    (sgml:pt-name pt)
                    (cons
                     attrs
                     (loop :for n :in (sgml:pt-children pt)
                        :when n :do (if (arrayp n) (f n))
                        :nconc (if (arrayp n) 
                                   (list (f n))
                                   (list (iterate n))))))))))
    (iterate pt)))

(defun parse-html-to-lhtml (html)
  (cxml-pt-to-lhtml (parse-html html)))

#+nil
(format t "~s" (cxml-pt-to-lhtml
                 (parse-html "<br/>ėšį<br> <div>fdsaa <span>fdsadfafdsa</span> <booo> <div>fdsafdsafdsa </div>")))

(assert (equal
(cxml-pt-to-lhtml
 (parse-html
"
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
          \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\">
  <head>
<base href=\"http://licejus.pov.lt/calendar/daily.html\" />
"
))
'(:HTML NIL
  (:HEAD NIL (:BASE ((:HREF "http://licejus.pov.lt/calendar/daily.html"))))
  (:BODY NIL))))


(assert (equal
(cxml-pt-to-lhtml
 (parse-html
"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">
<html dir=\"ltr\">
<head>
<meta http-equiv=\"Content-Type\" content=\"text/html; charset=windows-1257\">
<meta http-equiv=\"Content-Style-Type\" content=\"text/css\">

<title>GameDev.LT - Žaidimų kūrimas Lietuvoje :: Index</title>
<!-- link rel=\"stylesheet\" href=\"templates/DustyGreen/DustyGreen.css\" type=\"text/css\" -->
<link rel=\"stylesheet\" href=\"templates/DustyGreen/ssmitems.css\" type=\"text/css\">
</head>
<script>
<!--

/*
Copyright © MaXimuS 2002, All Rights Reserved.
Site: http://maximus.ravecore.com
E-mail: maximusforever@hotmail.com
Script: Static Slide Menu
Version: 6.6 Build 34
*/

NS6=(document.getElementById&&!document.all)
IE=(document.all);IE4=(document.all&&!document.getElementById)
NS=(navigator.appName==\"Netscape\" && navigator.appVersion.charAt(0)==\"4\")
OP=(navigator.userAgent.indexOf('Opera')>-1)
-->
</script>
"
))
'(:HTML NIL
 (:HEAD NIL
  (:META
   ((:HTTP-EQUIV "Content-Type")
    (:CONTENT "text/html; charset=windows-1257")))
  (:META ((:HTTP-EQUIV "Content-Style-Type") (:CONTENT "text/css")))
  (:TITLE NIL "GameDev.LT - Žaidimų kūrimas Lietuvoje :: Index")
  (:LINK
   ((:REL "stylesheet") (:HREF "templates/DustyGreen/ssmitems.css")
    (:TYPE "text/css"))))
 (:BODY NIL))))
