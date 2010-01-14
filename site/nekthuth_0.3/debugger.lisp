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
;;;
;;; The debuggers basically fall through from one of the two real debuggers (interrupt
;;; or regular) into the simple (just print out what happeend and move on) to
;;; bombing (just get me the hell out so we can get back up and running)
(in-package :nekthuth)

(defvar *eval-in-lexenv* (sb-di:frame-debug-fun (sb-di:top-frame)))
(defvar *max-frames* 10) ; For now

(defun find-interrupted-frame (frame &optional (n 0))
 (cond
  ((not frame) nil)
  ((sb-di::compiled-frame-escaped frame) frame)
  (t (find-interrupted-frame (sb-di:frame-down frame) (1+ n)))))

(defun print-var (frame var)
 (format nil "~A"
   (cond
    ((sb-di:debug-var-p var)
     (if (eql :valid (sb-di:debug-var-validity var (sb-di:frame-code-location frame)))
         (format nil "~S" (sb-di:debug-var-value var frame))
         (format nil "~A"  var)))
    ((eql var :deleted) "")
    ((eql (car var) :keyword) (format nil "~S ~S" (cadr var) (sb-di:debug-var-value (caddr var) frame)))
    ((eql (car var) :rest) (format nil "~{~S~^ ~}" (sb-di:debug-var-value (cadr var) frame)))
    ((eql (car var) :optional) (format nil "~S" (sb-di:debug-var-value (cadr var) frame))))))

(defun print-frame (frame)
 (let*
  ((fun (sb-di:frame-debug-fun frame))
   (name (sb-di:debug-fun-name fun))
   (vars (sb-di:debug-fun-lambda-list fun)))
  (format nil "(~A~{ ~A~})"
            (if (consp name) (cadr name) name)
            (mapcar (compose #'print-var frame) vars)
            )))

(defun print-frames (frame &optional (n 0))
 (when (and frame (< n *max-frames*))
  (cons
   (print-frame frame)
   (when
    (not (eql (sb-di:frame-debug-fun frame) *eval-in-lexenv*))
    (print-frames (sb-di:frame-down frame) (1+ n))))))

; This debugger should only come into play when we can't even send information to
; vim without running into errors.  At that point, we'll just kill our thread
; and let the master process restart it
;
; I know, I know, we should be using the condition/restart system properly,
; I'm just a simple minded java developer and I have yet to work out
; just how I want it all to work.
(defun bomb-debugger (cnd tmp)
 (whenit
  (find-restart 'sb-thread:terminate-thread)
  (invoke-restart it)))

(defun build-simple-debugger-hook (output)
 (lambda (cnd tmp)
  (let
   ((*debugger-hook* #'bomb-debugger))
   (send-to-vim #\E
                (format nil
                        "Simple Nekthuth Debugger~%~A~%~{~{~A: [~A] ~A~%~}~}----- Backtrace (at most ~A deep) -----~%~{~A~%~}"
                        cnd
                        (mapcar-with-idx (lambda (rst idx)
                                            (list (1+ idx) (restart-name rst) rst))
                           (remove-if (lambda (rst) (eql 'sb-thread:terminate-thread (restart-name rst)))
                            (compute-restarts)))
                        *max-frames*
                        (print-frames (or (find-interrupted-frame (sb-di:top-frame)) (sb-di:top-frame))))
                output)
   (ifit (find-restart 'nekthuth-abort)
         (invoke-restart it)
         (invoke-restart 'sb-thread:terminate-thread)))))

(defun build-interrupt-debugger-hook (output debug-mailbox)
 (lambda (cnd tmp)
  (declare (ignore tmp))
  (let*
   ((restarts (remove-if (lambda (rst) (eql 'sb-thread:terminate-thread (restart-name rst))) (compute-restarts)))
    (*debugger-hook* (build-simple-debugger-hook output)))
   (send-to-vim #\D
                (format nil "~A~%Interrupt Nekthuth Debugger~%~A~%~{~{~A: [~A] ~A~%~}~}"
                 (length restarts)
                 cnd
                 (mapcar-with-idx (lambda (rst idx)
                                   (list (1+ idx) (restart-name rst) rst))
                  restarts))
                output)
   (let
    ((restart-num (receive debug-mailbox)))
    (invoke-restart (if (eql 'error restart-num) 'nekthuth-abort (nth (1- restart-num) restarts)))))))

(defun build-debugger-hook (output debug-mailbox)
 (lambda (cnd tmp)
  (declare (ignore tmp))
  (let*
   ((restarts (remove-if (lambda (rst) (eql 'sb-thread:terminate-thread (restart-name rst))) (compute-restarts)))
    (*debugger-hook* (build-simple-debugger-hook output)))
   (send-to-vim #\D
                (format nil "~A~%Lame Nekthuth Debugger~%~A~%~{~{~A: [~A] ~A~%~}~}----- Backtrace (at most ~A deep) -----~%~{~A~%~}"
                 (length restarts)
                 cnd
                 (mapcar-with-idx (lambda (rst idx)
                                   (list (1+ idx) (restart-name rst) rst))
                  restarts)
                 *max-frames*
                 (print-frames (or (find-interrupted-frame (sb-di:top-frame)) (sb-di:top-frame))))
                output)
   (let
    ((restart-num (receive debug-mailbox)))
    (invoke-restart (if (eql 'error restart-num) 'nekthuth-abort (nth (1- restart-num) restarts)))))))
