;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: ODD-STREAMS; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/odd-streams/specials.lisp,v 1.7 2007/12/31 01:08:45 edi Exp $

;;; Copyright (c) 2007, Dr. Edmund Weitz.  All rights reserved.

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

(in-package :odd-streams)

(defconstant +max-byte-size+ 128
  ;; why do we actually limit this?
  "The maximal size of an odd stream element in bits.")

(defconstant +recommended-bytes-per-buffer+ 1024
  "Default number of bytes per buffer.")

(defconstant +recommended-octets-per-buffer+ 8192
  "The library tries to make let the size of the stream buffers be a
 multiple of this value.  For that reason, the product of
 +MAX-BYTE-SIZE+ and +RECOMMENDED-BYTES-PER-BUFFER+ must also be a
 multiple of this value.")

(defconstant +min-buffer-size+ 64
  "The minimal size for a buffer in octets.")

