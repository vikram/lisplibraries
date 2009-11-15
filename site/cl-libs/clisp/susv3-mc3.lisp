;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               susv3-mc3.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    An implementation of SUSV3-MC3 for clisp.
;;;;
;;;;    Implemented:
;;;;        mmap/munmap
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2004-11-29 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2004 - 2004
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


(cl:in-package "COMMON-LISP-USER")
(DECLAIM (DECLARATION ALSO-USE-PACKAGES)
         (ALSO-USE-PACKAGES "FFI" "LINUX"))
(defpackage "COM.INFORMATIMAGO.CLISP.SUSV3-MC3"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.CLISP.SUSV3")
  (:EXPORT 
   "PROT-NONE" "PROT-READ" "PROT-WRITE" "PROT-EXEC" 
   "MAP-SHARED" "MAP-PRIVATE" "MAP-FIXED" "MAP-FILE"
   "MAP-ANONYMOUS" "MAP-GROWSDOWN" "MAP-DENYWRITE" 
   "MAP-EXECUTABLE" "MAP-LOCKED" "MAP-NORESERVE" 
   "MAP-FAILED" 
   "MMAP" "MUNMAP")
  (:DOCUMENTATION "
    An implementation of SUSV3-MC3 for clisp.

    Implemented:
        mmap/munmap
    
    Copyright Pascal J. Bourguignon 2004 - 2004

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version
    2 of the License, or (at your option) any later version.
"))
(in-package "COM.INFORMATIMAGO.CLISP.SUSV3-MC3")

    
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; TODO: Actually, we should include the features only if it's proven to exist on the current system. At run-time.
  (pushnew :susv3 *features*)
  (pushnew :susv3-mc3 *features*))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +libc+ "/lib/libc.so.6"))



(defconstant PROT-NONE  0 "Page can not be accessed.")
(defconstant PROT-READ  1	"Page can be read.")
(defconstant PROT-WRITE 2 "Page can be written.")
(defconstant PROT-EXEC  4 "Page can be executed.")

(defconstant MAP-SHARED       #x01 "Share changes. ")
(defconstant MAP-PRIVATE      #x02 "Changes are private. ")
(defconstant MAP-FIXED        #x10 "Interpret addr exactly. ")
(defconstant MAP-FILE            0)
(defconstant MAP-ANONYMOUS    #x20 "Don't use a file. ")
(defconstant MAP-GROWSDOWN  #x0100 "Stack-like segment. ")
(defconstant MAP-DENYWRITE  #x0800 "ETXTBSY")
(defconstant MAP-EXECUTABLE #x1000 "Mark it as an executable. ")
(defconstant MAP-LOCKED     #x2000 "Lock the mapping. ")
(defconstant MAP-NORESERVE  #x4000 "Don't check for reservations. ")

(defconstant MAP-FAILED     #xffffffff) ; -1


(ffi:def-c-type pointer ffi:ulong)

(ffi:def-call-out mmap  (:name "mmap")
  (:arguments (start pointer) (size linux:|size_t|)
              (prot ffi:int) (flags ffi:int)
              (fd ffi:int) (offset linux:|off_t|))
  (:return-type pointer)
  (:library #.+libc+) (:language :stdc))


(ffi:def-call-out munmap  (:name "munmap")
  (:arguments  (start pointer) (size linux:|size_t|))
  (:return-type pointer)
  (:library #.+libc+) (:language :stdc))

;;;; susv3-mc3.lisp                   --                     --          ;;;;
