;;; Authored by Frank Duncan (frank@nekthuth.com), Copyright 2008
;;;
;;; This software is licensed under the Library General Public License
;;; as extended to be more applicable to Lisp.  See for more information:
;;;
;;; http://opensource.franz.com/preamble.html
;;;
;;; Unless required by applicable law or agreed to in writing, software distributed
;;; under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
;;; CONDITIONS OF ANY KIND, either express or implied. See the License for the
;;; specific language governing permissions and limitations under the License.
;;;
;;; Three types of plugins
;;;   * Receiver plugin - receives commands from vim and does some thing with them, may not be needed
;;;   * Sender plugin - sends commands to vim every time something is done in the repl (could update to be on a command thread)
;;;   * Command plugin - Executes something from vim and returns a response

(eval-when (:compile-toplevel :load-toplevel :execute)
 (require 'sb-bsd-sockets) )

(in-package #:nekthuth)

; To be used for debugging when you actually want things to go to the REPL
(defvar *real-stdout* *standard-output*)

(defvar *receivers* nil)
(defvar *commands* nil)
(defvar *senders* nil)

; This holds all the stateful (per thread) data senders may need
; We lexically override this in each start-in-vim based thread we boot
; Since the set of all senders is going to be small, we'll just make it an assoc list
(defvar *sender-data*)

; These are threadsafe only because we use them only in send-all-senders, which is in a mutex
; Let's please not use them anywhere else
(defun get-sender-data (sender-char)
 (cdr (assoc sender-char *sender-data*)))
(defun save-sender-data (sender-char data)
 (ifit (assoc sender-char *sender-data*)
       (setf (cdr it) data)
       (setf *sender-data* (acons sender-char data *sender-data*))))

(defun register-receiver (cmd-char func)
 (setf *receivers* (acons cmd-char func *receivers*)))
(defun register-command (cmd-char func)
 (setf *commands* (acons cmd-char func *commands*)))
(defun register-sender (cmd-char func)
 (setf *senders* (acons cmd-char func *senders*)))

(whenit (probe-file (merge-pathnames #p".nekthuthrc.lisp" (user-homedir-pathname)))
        (load it))

(defvar *remote-port* 8532)

(defstruct mailbox
           (q nil)
           (waitq (sb-thread:make-waitqueue))
           (mutex (sb-thread:make-mutex)))

(defun send (mailbox letter)
 (sb-thread:with-mutex ((mailbox-mutex mailbox))
  (setf (mailbox-q mailbox) (nconc (mailbox-q mailbox) (list letter)))
  (sb-thread:condition-broadcast (mailbox-waitq mailbox))))

(defun receive (mailbox)
 (sb-thread:with-mutex ((mailbox-mutex mailbox))
  (if
   (mailbox-q mailbox)
   (pop (mailbox-q mailbox))
   (progn
    (sb-thread:condition-wait (mailbox-waitq mailbox) (mailbox-mutex mailbox))
    (pop (mailbox-q mailbox))))))

(defvar *output-lock* (sb-thread:make-mutex))

;; Do not ever do anything more complicated than writing to output from this func
;; or we could run into lock nastiness (with sender-mutex, for instance)
(defun send-to-vim (cmd-char msg &optional (output *standard-output*))
 (let
  ((msg-str (format nil "~A" msg)))
  (sb-thread:with-mutex (*output-lock*)
   (format output "~A~6,'0d~A~%" cmd-char (1+ (length msg-str)) msg-str)
   (force-output output))))

(let
 ((sender-mutex (sb-thread:make-mutex)))
 (defun send-all-senders (&optional (output *standard-output*))
  (sb-thread:with-mutex (sender-mutex)
   (mapcar (lambda (sender)
            (let
             ((sender-retn (multiple-value-list (apply (cdr sender) (get-sender-data (car sender))))))
             (when (car sender-retn) (send-to-vim (car sender) (car sender-retn) output))
             (when (cdr sender-retn) (save-sender-data (car sender) (cdr sender-retn)))))
           *senders*))))

(defun make-repl-thread (incoming true-out debug-mailbox)
 (sb-thread:make-thread
  (lambda ()
   (let*
    ((output (make-string-output-stream))
     (*standard-output* output)
     (*error-output* output)
     (*sender-data* nil)) ; This effectively makes senders get their data all kinds of weirdly
    (loop
     (let*
      ((next (receive incoming))
       (*debugger-hook* (build-debugger-hook true-out debug-mailbox))
       (retn (with-simple-restart (nekthuth-abort "Abort Nekthuth request") (multiple-value-list (eval next)))))
      (send-to-vim #\R (format nil "~A~%~{~S~%~}" (get-output-stream-string output) retn) true-out)
      (send-all-senders true-out)))))
  :name "Repl thread"))

(defun execute-command (cmd func &rest msg-from-vim)
 (with-output-to-string (strm)
  (send-to-vim
   cmd
   (let
    ((*standard-output* strm))
    (lett
     (((retn error) (ignore-errors (apply func msg-from-vim))))
     (if error
      (format nil "Error occured: ~A" error)
      retn))))))

(defun execute-receiver (func &rest msg-from-vim)
 (with-output-to-string (strm)
  (let
   ((*standard-output* strm)
    (*error-output* strm))
   (apply func msg-from-vim))))

(defun decode (cmd)
 (condit
  ((assoc cmd *commands*) (compose #'execute-command (car it) (cdr it)))
  ((assoc cmd *receivers*) (compose #'execute-receiver (cdr it)))
  (t (lambda (msg-from-vim) (send-to-vim #\E (format nil "Command not found: ~A" cmd))))))

(defun read-forms-from-string (string)
 (with-input-from-string (str string)
  (loop as obj = (read str nil 'end-of-file)
           while (not (eql obj 'end-of-file))
           collect obj)))

(defun read-from-vim ()
 (let
  ((cmd (read-char *standard-input*))
   (msg-length-str (make-string 6)))
  (read-sequence msg-length-str *standard-input*)
  (let*
   ((msg-length (parse-integer msg-length-str))
    (msg (make-string msg-length)))
   (read-sequence msg *standard-input*)
   (apply (decode cmd) (when (< 0 msg-length) (read-forms-from-string msg)))
   (send-all-senders))))

(defun start-in-vim ()
 (let*
  ((*sender-data* nil)
   (repl-mailbox (make-mailbox))
   (debug-mailbox (make-mailbox))
   (repl (make-repl-thread repl-mailbox *standard-output* debug-mailbox))
   (true-out *standard-output*)
   (*debugger-hook* (build-simple-debugger-hook *standard-output*))
   (*receivers* *receivers*)) ; We dynamically scope *receivers* to itself so the following receivers will be threadlocal
  (register-receiver #\R (compose #'send repl-mailbox))
  (register-receiver #\D (compose #'send debug-mailbox))
  (register-receiver #\I (lambda ()
                          (sb-thread:interrupt-thread repl
                           (lambda ()
                            (with-simple-restart (soldier-on "Don't cancel")
                             (let
                              ((*debugger-hook* (build-interrupt-debugger-hook true-out debug-mailbox)))
                               (error "Interrupted execution")))))))
  (register-receiver #\Q (lambda () (sb-thread:terminate-thread repl) (sb-ext:quit)))
  (format t "~%AGO~%")
  (format t "~%AVERSION~A~%" nekthuth.system:+nekthuth-version+)
  (send-all-senders)
  (loop
   (with-simple-restart (nekthuth-abort "Abort Nekthuth request") (read-from-vim)))))

(defvar *remote-thread* nil)

(defun start-remote (&optional (port *remote-port*))
 (setf *remote-thread*
       (sb-thread:make-thread (lambda ()
        (let ((socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
          (sb-bsd-sockets:socket-bind socket #(127 0 0 1) port)
          (sb-bsd-sockets:socket-listen socket 5)
         (unwind-protect
          (loop
           (let
            ((new-fd (sb-bsd-sockets:socket-accept socket)))
            (let
             ((strm (sb-bsd-sockets:socket-make-stream new-fd :input t :output t)))
             (sb-thread:make-thread (lambda ()
                                     (let
                                      ((*standard-output* strm)
                                       (*standard-input* strm))
                                      (start-in-vim)))
              :name "Inner remote thread"))))
        (sb-bsd-sockets:socket-close socket))))
      :name "Main Remote Thread")))

(defun stop-remote ()
 (when (and *remote-thread* (sb-thread:thread-alive-p *remote-thread*))
       (sb-thread:terminate-thread *remote-thread*)))
