;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               regex.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Loaded for Michale Parker's REGEX
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2004-11-23 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal Bourguignon 2004 - 2004
;;;;    
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU General Public License
;;;;    as published by the Free Software Foundation; either version
;;;;    2 of the License, or (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be
;;;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;;;    PURPOSE.  See the GNU General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU General Public
;;;;    License along with this program; if not, write to the Free
;;;;    Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;;    Boston, MA 02111-1307 USA
;;;;****************************************************************************

(load "loaders:clocc")

(let ((sys-path
       (make-pathname
        :directory (append (pathname-directory (user-homedir-pathname)) 
                           '("src" "lisp" "michael-parker"))
        :defaults (user-homedir-pathname)))
      (src-path
       (make-pathname
        :directory (append (pathname-directory (user-homedir-pathname)) 
                           '("src" "lisp" "michael-parker" "regex"))
        :defaults (user-homedir-pathname))))
  (setf (logical-pathname-translations "REGEX")
        (list(list "REGEX:SYS;*.*" (make-pathname :name :wild
                                                  :defaults sys-path))
             (list "REGEX:SYS;*.*" (make-pathname :name :wild
                                                  :type :wild
                                                  :defaults sys-path))
             (list "REGEX:SYS;*.*.*" (make-pathname :name :wild
                                                    :type :wild
                                                    :version :wild
                                                    :defaults sys-path))
             (list "REGEX:SRC;*.*" (make-pathname :name :wild
                                                  :defaults src-path))
             (list "REGEX:SRC;*.*" (make-pathname :name :wild
                                                  :type :wild
                                                  :defaults src-path))
             (list "REGEX:SRC;*.*.*" (make-pathname :name :wild
                                                    :type :wild
                                                    :version :wild
                                                    :defaults src-path))))
  (push src-path MK::*CENTRAL-REGISTRY*))

(mk:oos "regex" :load)

;;;; regex.lisp                       --                     --          ;;;;

