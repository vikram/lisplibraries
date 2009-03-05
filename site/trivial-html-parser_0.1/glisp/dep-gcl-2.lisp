;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: GLISP; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: Second part of GCL dependent stuff
;;;   Created: 1999-05-25 22:31
;;;    Author: Gilbert Baumann <gilbert@base-engineering.com>
;;;   License: MIT style (see below)
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 1999 by Gilbert Baumann

;;;  Permission is hereby granted, free of charge, to any person obtaining
;;;  a copy of this software and associated documentation files (the
;;;  "Software"), to deal in the Software without restriction, including
;;;  without limitation the rights to use, copy, modify, merge, publish,
;;;  distribute, sublicense, and/or sell copies of the Software, and to
;;;  permit persons to whom the Software is furnished to do so, subject to
;;;  the following conditions:
;;; 
;;;  The above copyright notice and this permission notice shall be
;;;  included in all copies or substantial portions of the Software.
;;; 
;;;  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. 
;;;  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;;  CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;;  TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;;  SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(in-package :GLISP)

(lisp::clines
 "#include <stdio.h>"
 "#include <unistd.h>"
 "#include <sys/stat.h>"
 "#include <sys/socket.h>"
 "#include <netinet/in.h>"
 "#include <stdlib.h>"
 "#include <fcntl.h>"
 "#include <resolv.h>"
 )

(lisp::defcfun "static object open_inet_socket_aux (object x, object y, char *hostname, int port)" 2
               "FILE *fp;"
               "object stream;"

               "struct hostent *hostinfo;"
               "struct sockaddr_in addr;"
               "int sock;"
               "vs_mark;"
  
               "hostinfo = gethostbyname (hostname);"

               "if (hostinfo == 0)"
               "{"
               "  return Cnil;"
               "}"

               "addr.sin_family = AF_INET;"
               "addr.sin_port = htons (port);"
               "addr.sin_addr = *(struct in_addr*) hostinfo->h_addr;"
               ""
               "sock = socket (PF_INET, SOCK_STREAM, 0);"
               "if (sock < 0)"
               "  return Cnil;"
               ""
               "if (connect (sock, (struct sockaddr *) &addr, sizeof (addr)) != 0)"
               "{"
               "  close (sock);"
               "  return Cnil;"
               "}"


               "fp = fdopen (sock, \"rb+\");"
               "stream = (object)  alloc_object(t_stream);"
               "stream->sm.sm_mode = (short)smm_io;"
               "stream->sm.sm_fp = fp;"
               "stream->sm.sm_object0 = x;"
               "stream->sm.sm_object1 = y;"
               "stream->sm.sm_int0 = stream->sm.sm_int1 = 0;"
               "vs_push(stream);"
               "setup_stream_buffer(stream);"
               "vs_reset;"
               "return stream;"    
               )

(lisp::defentry open-inet-socket-aux (lisp::object lisp::object lisp::string lisp::int)
                (lisp::object "open_inet_socket_aux"))

(lisp::defentry unix/system (lisp::string)
                (lisp::int "system"))

(defun open-inet-socket (hostname port)
  (values (or (open-inet-socket-aux '(unsigned-byte 8)
                                    (format nil "Network connection to ~A:~D" hostname port)
                                    hostname port)
              (error "Cannot connect to `~A' on port ~D."
                     hostname port))
          :byte))
