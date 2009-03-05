;; -*- mode: common-lisp; package: net.aserve -*-
;;
;; macs.cl
;;
;; copyright (c) 1986-2000 Franz Inc, Berkeley, CA  - All rights reserved.
;; copyright (c) 2000-2004 Franz Inc, Oakland, CA - All rights reserved.
;;
;; This code is free software; you can redistribute it and/or
;; modify it under the terms of the version 2.1 of
;; the GNU Lesser General Public License as published by 
;; the Free Software Foundation, as clarified by the AllegroServe
;; prequel found in license-allegroserve.txt.
;;
;; This code is distributed in the hope that it will be useful,
;; but without any warranty; without even the implied warranty of
;; merchantability or fitness for a particular purpose.  See the GNU
;; Lesser General Public License for more details.
;;
;; Version 2.1 of the GNU Lesser General Public License is in the file 
;; license-lgpl.txt that was distributed with this file.
;; If it is not present, you can access it from
;; http://www.gnu.org/copyleft/lesser.txt (until superseded by a newer
;; version) or write to the Free Software Foundation, Inc., 59 Temple Place, 
;; Suite 330, Boston, MA  02111-1307  USA
;;

;;
;; $Id: macs.cl,v 1.10 2005/02/20 12:20:45 rudi Exp $

;; Description:
;;   useful internal macros

;;- This code in this file obeys the Lisp Coding Standard found in
;;- http://www.franz.com/~jkf/coding_standards.html
;;-



;; macros used by iserve


(in-package :net.aserve)

;; add features based on the capabilities of the host lisp
#+(and allegro (version>= 6 1))
(pushnew :io-timeout *features*) ; support i/o timeouts

;; Note for people using this code in non-Allegro lisps.
;;
;; The if* macro used in this code can be found at:
;;
;; http://www.franz.com/~jkf/ifstar.txt
;;


;; macros to speed up some common character operations
(defmacro find-it (ch buff start end)
  ;; return position of ch in buff from [start end}
  ;;
  (let ((pos (gensym)))
    `(do ((,pos ,start (1+ ,pos)))
	 ((>= ,pos ,end))
       (if* (eq (schar ,buff ,pos) ,ch)
	  then (return ,pos)))))

(defmacro find-it-rev (ch buff start end)
  ;; return position of ch in buff from [start end}
  ;; searching backwards
  ;;
  (let ((pos (gensym)))
    `(do ((,pos (1- ,end) (1- ,pos)))
	 ((< ,pos ,start))
       (if* (eq (schar ,buff ,pos) ,ch)
	  then (return ,pos)))))

(defmacro buffer-substr (buff start end)
  ;; return a string holding the chars in buff from [start end }
  ;;
  (let ((res (gensym))
	(i (gensym))
	(pos (gensym)))
    `(let ((,res (make-string (- ,end ,start))))
       (do ((,i 0 (1+ ,i))
	    (,pos ,start (1+ ,pos)))
	   ((>= ,pos ,end))
	 (setf (schar ,res ,i) (schar ,buff ,pos)))
       ,res)))

(defmacro buffer-match (buff start str)
  ;; return t if the buffer buff contains the same string as str
  (let ((pos (gensym))
	(i (gensym))
	(len (gensym)))
	   
    `(do ((,pos ,start (1+ ,pos))
	  (,i 0 (1+ ,i))
	  (,len (length ,str)))
	 ((>= ,i ,len) t)
       (if* (not (eq (schar ,buff ,pos) (schar ,str ,i)))
	  then (return nil)))))

(defmacro buffer-match-ci (buff start str)
  ;; return t if the buffer buff contains the same string as str
  ;; case insensitive version where str contains each char doubled
  (let ((pos (gensym))
	(i (gensym))
	(len (gensym))
	(xchar (gensym)))
	   
    `(do ((,pos ,start (1+ ,pos))
	  (,i 0 (+ 2 ,i))
	  (,len (length ,str)))
	 ((>= ,i ,len) t)
       (let ((,xchar (schar ,buff ,pos)))
	 (if* (not (or (eq ,xchar (schar ,str ,i))
		       (eq ,xchar (schar ,str (1+ ,i)))))
	    then (return nil))))))


(defmacro rational-read-sequence (&rest args)
  ;; acl's read-sequence was changed to conform to the
  ;; bogus ansi definition where the whole buffer was filled up.
  ;; even for socket stream. 
  ;; rational-read-sequence does read-sequence the right way.
#-(and allegro (version>= 6 0 pre-final 9)) 
`(read-sequence ,@args)
#+(and allegro (version>= 6 0 pre-final 9)) 
`(read-sequence ,@args :partial-fill t)
)




;;;; response macros


;----  unsigned byte 8 array macros:

(defmacro ausb8 (vec index)
  ; like aref but it declares the type
  `(aref (the (simple-array (unsigned-byte 8) 1) ,vec) ,index))

(defmacro copy-usb8 (from-vector from-start
		     to-vector   to-start
		     count)
  ;; copy count bytes from from-vector[start] to to-vector[start].
  ;; vectors are usb8
  (let ((from (gensym))
	(to   (gensym))
	(i (gensym)))


    `(do ((,from ,from-start (1+ ,from))
	  (,to   ,to-start   (1+ ,to))
	  (,i ,count (1- ,i)))
	 ((<= ,i 0))
       (setf (ausb8 ,to-vector ,to)
	 (ausb8 ,from-vector ,from)))))


(defmacro copy-usb8-up (from-vector from-start
			to-vector   to-start
			count)
  ;; copy count bytes from from-vector[start] to to-vector[start],
  ;; going from the top down.  this is designed to be used if we are
  ;; copying upward in place so we have to copy from the top down
  ;;
  ;; vectors are usb8
  (let ((from (gensym))
	(to   (gensym))
	(i    (gensym)))


    `(do* ((,i ,count (1- ,i))
	   (,from (+ ,from-start ,i -1) (1- ,from))
	   (,to   (+ ,to-start   ,i -1) (1- ,to)))
	 ((<= ,i 0))
       (setf (ausb8 ,to-vector ,to)
	 (ausb8 ,from-vector ,from)))))


;-------------




(defmacro dlogmess (&rest args)
  ;; for now we just disable completely the log messages.
  ;; in the future we'll turn on and off the log messages 
  ;; at runtime with a switch.
  (declare (ignore args))
  nil)


;---------------
; acl 6.1 and newer support timeouts on read/write for hiper streams
; Thus we can avoid using global timeouts in certain cases.
;

; with-timeout-local: use with-timeout if that all we've got
; else use read-write timeouts
; 
#-(and allegro (version>= 6 1))
(defmacro with-timeout-local ((time &rest actions) &body body)
  ;; same as with-timeout 
  `(acl-compat.mp:with-timeout (,time ,@actions) ,@body))   ; ok w-t


#+(and allegro (version>= 6 1))
(defmacro with-timeout-local ((time &rest actions) &body body)
  (declare (ignore time))
  (let ((g-blocktag (gensym)))
    `(block ,g-blocktag
       (handler-bind ((socket-error 
		       #'(lambda (c)
			   (if* (member (stream-error-identifier c) 
					'(:read-timeout :write-timeout)
					:test #'eq)
			      then ; must handle this
				   (return-from ,g-blocktag
				     (progn ,@actions))))))
	 ,@body))))
			 


