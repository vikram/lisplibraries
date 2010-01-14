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

;; #\S == Syntax additions
(in-package :nekthuth)

(defun get-funcs ()
 (remove-if-not
  (lambda (symb)
   (and
    (not (eql (find-package 'common-lisp) (symbol-package symb)))
    (fboundp symb)))
  (get-all-symbols)))

(defun get-external-funcs ()
 (let
  ((retn nil))
  (mapit
   (do-external-symbols (symb it) (setf retn (cons symb retn)))
   (remove (find-package 'common-lisp) (list-all-packages)))
  retn))

(defun get-function-additions (previous-functions previous-external-functions)
 (let*
  ((current-functions (get-funcs))
   (current-external-functions (get-external-funcs))
   (retn-func (and (/= (length current-functions) (length previous-functions))
                   (remove-if (lambda (item) (find item previous-functions)) current-functions)))
   (retn-ext-func (and (/= (length current-external-functions) (length previous-external-functions))
                       (remove-if (lambda (item) (find item previous-external-functions)) current-external-functions))))
  (values retn-func retn-ext-func current-functions current-external-functions)))

(defun syntax-additions (&optional previous-functions previous-external-functions)
 (let
  ((*print-pretty* nil))
  (multiple-value-bind (function-additions external-function-additions new-previous-functions new-previous-external-functions)
                       (get-function-additions previous-functions previous-external-functions)
                       (when (or function-additions external-function-additions)
                        (values (format nil "[~{~('~A'~)~^, ~}~A~{~('~A'~)~^, ~}]" function-additions
                                                                                   (if (and function-additions external-function-additions) "," "")
                                                                                   (mapit
                                                                                    (format nil "~A:~A" (package-name (symbol-package it)) it)
                                                                                    external-function-additions))
                                new-previous-functions
                                new-previous-external-functions)))))

(register-sender #\S #'syntax-additions)
