;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          os.lisp
;;;; Purpose:       Operating System utilities
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Jul 2003
;;;;
;;;; $Id$
;;;;
;;;; *************************************************************************

(in-package #:kmrcl)

(defun command-output (control-string &rest args)
  "Interpolate ARGS into CONTROL-STRING as if by FORMAT, and
synchronously execute the result using a Bourne-compatible shell,
returns (VALUES string-output error-output exit-status)"
  (let ((command (apply #'format nil control-string args)))
    #+sbcl
    (let* ((process (sb-ext:run-program
                    "/bin/sh"
                    (list "-c" command)
                    :input nil :output :stream :error :stream))
           (output (read-stream-to-string (sb-impl::process-output process)))
           (error (read-stream-to-string (sb-impl::process-error process))))
      (close (sb-impl::process-output process))
      (close (sb-impl::process-error process))
      (values
       output
       error
       (sb-impl::process-exit-code process)))


    #+(or cmu scl)
    (let* ((process (ext:run-program
                     "/bin/sh"
                     (list "-c" command)
                     :input nil :output :stream :error :stream))
           (output (read-stream-to-string (ext::process-output process)))
           (error (read-stream-to-string (ext::process-error process))))
      (close (ext::process-output process))
      (close (ext::process-error process))

      (values
       output
       error
       (ext::process-exit-code process)))

    #+allegro
    (multiple-value-bind (output error status)
        (excl.osi:command-output command :whole t)
      (values output error status))

    #+lispworks
    ;; BUG: Lispworks combines output and error streams
    (let ((output (make-string-output-stream)))
      (unwind-protect
          (let ((status
                 (system:call-system-showing-output
                  command
                  :prefix ""
                  :show-cmd nil
                  :output-stream output)))
            (values (get-output-stream-string output) nil status))
        (close output)))

    #+clisp
    ;; BUG: CLisp doesn't allow output to user-specified stream
    (values
     nil
     nil
     (ext:run-shell-command  command :output :terminal :wait t))

    #+openmcl
    (let* ((process (ccl:run-program
                     "/bin/sh"
                     (list "-c" command)
                     :input nil :output :stream :error :stream
                     :wait t))
           (output (read-stream-to-string (ccl::external-process-output-stream process)))
           (error (read-stream-to-string (ccl::external-process-error-stream process))))
      (close (ccl::external-process-output-stream process))
      (close (ccl::external-process-error-stream process))
      (values output
              error
              (nth-value 1 (ccl::external-process-status process))))

    #-(or openmcl clisp lispworks allegro scl cmu sbcl)
    (error "COMMAND-OUTPUT not implemented for this Lisp")

    ))

(defun run-shell-command (control-string &rest args)
  "Interpolate ARGS into CONTROL-STRING as if by FORMAT, and
synchronously execute the result using a Bourne-compatible shell,
returns (VALUES output-string pid)"
  (let ((command (apply #'format nil control-string args)))
    #+sbcl
    (sb-impl::process-exit-code
     (sb-ext:run-program
      "/bin/sh"
      (list  "-c" command)
      :input nil :output nil))

    #+(or cmu scl)
    (ext:process-exit-code
     (ext:run-program
      "/bin/sh"
      (list  "-c" command)
      :input nil :output nil))


    #+allegro
    (excl:run-shell-command command :input nil :output nil
                            :wait t)

    #+lispworks
    (system:call-system-showing-output
     command
     :shell-type "/bin/sh"
     :show-cmd nil
     :prefix ""
     :output-stream nil)

    #+clisp             ;XXX not exactly *verbose-out*, I know
    (ext:run-shell-command  command :output :terminal :wait t)

    #+openmcl
    (nth-value 1
               (ccl:external-process-status
                (ccl:run-program "/bin/sh" (list "-c" command)
                                 :input nil :output nil
                                 :wait t)))

    #-(or openmcl clisp lispworks allegro scl cmu sbcl)
    (error "RUN-SHELL-PROGRAM not implemented for this Lisp")

    ))

(defun delete-directory-and-files (dir &key (if-does-not-exist :error) (quiet t) force)
  #+allegro (excl:delete-directory-and-files dir :if-does-not-exist if-does-not-exist
                                             :quiet quiet :force force)
  #-(or allegro) (declare (ignore force))
  #-(or allegro) (cond
                   ((probe-directory dir)
                    (let ((cmd (format nil "rm -rf ~A" (namestring dir))))
                      (unless quiet
                        (format *trace-output* ";; ~A" cmd))
                      (command-output cmd)))
                   ((eq if-does-not-exist :error)
                    (error "Directory ~A does not exist [delete-directory-and-files]." dir))))

(defun file-size (file)
  (when (probe-file file)
    #+allegro (let ((stat (excl.osi:stat (namestring file))))
                (excl.osi:stat-size stat))
    #-allegro
    (with-open-file (in file :direction :input)
      (file-length in))))

(defun getpid ()
  "Return the PID of the lisp process."
  #+allegro (excl::getpid)
  #+(and lispworks win32) (win32:get-current-process-id)
  #+(and lispworks (not win32)) (system::getpid)
  #+sbcl (sb-posix:getpid)
  #+cmu (unix:unix-getpid)
  #+openmcl (ccl::getpid)
  #+(and clisp unix) (system::process-id)
  #+(and clisp win32) (cond ((find-package :win32)
                             (funcall (find-symbol "GetCurrentProcessId"
                                                   :win32)))
                            (t
                             (system::getenv "PID")))
  )


