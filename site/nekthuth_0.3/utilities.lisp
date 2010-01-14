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
;;;
;;; These are generic utilities that I use in most all of my programs.  It seemed
;;; more elegant to just include them in their current form here, but who knows
;;; if that's a good idea
(in-package :nekthuth)

(defmacro whenit (test-form &body body)
 `(ifit ,test-form
   (progn ,@body)))

(defmacro condit (&rest clauses)
 (if (not clauses)
     nil
     (let ((clause (car clauses))
           (sym (gensym)))
      `(let ((,sym ,(car clause)))
        (if ,sym
            (let ((it ,sym)) ,@(cdr clause))
            (condit ,@(cdr clauses)))))))

(defmacro ifit (test-form then-form &optional else-form)
 `(let
   ((it ,test-form))
   (if it ,then-form ,else-form)))

(defun compose (func &rest args)
 (lambda (&rest other-args)
  (apply func (append args other-args))))

(defmacro mapcar-with-idx (func &rest lsts)
 (let
  ((first-lst (gensym)))
 `(let
   ((,first-lst ,(car lsts)))
   (mapcar ,func ,first-lst ,@(cdr lsts) (mapint #'identity (length ,first-lst))))))

(defun mapint (fn n &optional (current 0))
 (cond
  ((>= current n) nil)
  ((cons (funcall fn current) (mapint fn n (+ current 1))))))

(defmacro mapit (fn-body lst)
 `(mapcar (lambda (it) ,fn-body) ,lst))

(defmacro lett (spec &rest body)
 (let
  ((pairs (apply #'append (mapit
                           (when (listp (car it))
                                 (mapit (cons it (gensym)) (car it)))
                           spec))))
  (labels
   ((build-multi-val-bind (spec body)
     (cond
      ((not spec) body)
      ((listp (caar spec))
       `(multiple-value-bind ,(mapit (cdr (assoc it pairs)) (caar spec)) ,(cadar spec)
         ,(build-multi-val-bind (cdr spec) body)))
      (t (build-multi-val-bind (cdr spec) body)))))
   (build-multi-val-bind spec
   `(let ,(apply #'append (mapit (cond
                     ((listp (car it))
                      (mapit
                       (list it (cdr (assoc it pairs)))
                       (car it)))
                     (t (list it)))
              spec))
     ,@body)))))

(defun get-all-symbols ()
 (let
  ((symbols nil)
   (pkgs (get-dep-pkgs (package-use-list *package*))))
  (do-symbols (symb *package*) (setf symbols (cons symb symbols)))
  (dolist (pkg pkgs) (do-external-symbols (symb pkg) (setf symbols (cons symb symbols))))
  symbols))

(defun get-dep-pkgs (pkgs &optional retn)
 (if (not pkgs)
     retn
     (let
      ((pkg (car pkgs)))
      (if (find pkg retn)
          (get-dep-pkgs (cdr pkgs) retn)
          (get-dep-pkgs (append (cdr pkgs)
                                (package-use-list pkg))
                        (cons pkg retn))))))
