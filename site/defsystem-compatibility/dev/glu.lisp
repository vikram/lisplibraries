(in-package #:defsystem-compatibility)

(defclass glu-system ()
  ((name :initarg :name :accessor glu-name)))

(defmethod registered-systems* ((system-definer (eql :glu)))
  user:*glu-defined-systems*)

(defmethod loaded-systems* ((system-definer (eql :glu)))
  user:*glu-loaded-systems*)

(defmethod system-source-file* ((system-definer (eql :glu)) system-name)
  (user:glu-system-source-file (ensure-system-name system-name)))

(defmethod associated-test-system* ((system-definer (eql :glu)) system-name)
  (user:glu-system-test-system system-name))

(defmethod top-level-system-p* ((system-definer (eql :glu)) system-name)
  (user:glu-system-top-level system-name))

(defmethod system-name-for-display* ((system-definer (eql :glu)) system-name)
  (user::glu-system-name system-name))

(defmethod ensure-system-name* ((system-definer (eql :glu)) system-name)
  (user::canonicalize-glu-system-name system-name))

(defmethod filename-looks-like-system-file-p* ((system-definer (eql :glu)) filename)
  (string-equal "system" (pathname-type filename))) 
                
(defmethod system-dependencies* ((system-definer (eql :glu)) system-name)
  (user:glu-system-all-subsystems system-name)) 
                
;;?? Gary King 2005-12-02: a very ugly hack
(defmethod find-system-for-mapping* (system-definer system-name)
  (find-system* system-definer system-name))

(defmethod find-system-for-mapping* ((system-definer (eql :glu)) system-name)
  (let ((system (find-system* system-definer system-name)))
    (when system
      (make-instance 'glu-system :name system))))

(defmethod system-sub-systems* ((system-definer (eql :glu)) system-name)
  (collect-system-dependencies system-name))


#+GLU-GENERIC-LOAD-UTILS
(defmethod map-system-files ((system glu-system) function &rest args 
                             &key system-closure? include-pathname?
                             &allow-other-keys)
  (when system-closure?
    (dolist (system (collect-system-dependencies (glu-name system)))
      (apply #'map-system-files system function args)))
  
  (user:map-glu-system-files 
   (nconc (list (glu-name system))
          #+Ignore
          (when include-tests?
            (list (associated-test-system system-name))))
   function 
   :system-closure? nil
   :include-pathname? include-pathname?
   :include-associates? (get-setting :project-manager :show-associates?)))


#+(and Ignore GLU-GENERIC-LOAD-UTILS)
(defun glu-system-top-level-system (system-name sorted-systems)
  (argmin (user:glu-system-top-level-systems system-name)
          (lambda (x)
            (position x sorted-systems))))

#+(and Ignore GLU-GENERIC-LOAD-UTILS)
;;?? also found in grapher-examples
(defun make-systems-graph (&optional (systems (registered-systems)))
  "Creates a GRAPH-CONTAINER that is the dependency graph for the given
  SYSTEMS."
  (let* ((canon-systems (mapcar #'ensure-system-name systems))
         (graph (make-instance 'graph-container
                  :initial-contents canon-systems
                  :default-edge-type :directed))
         (done nil))
    (labels ((show-parents (system)
               (unless (member system done)
                 (push system done)
                 (dolist (parent (get system :glu-system-depends-on))
                   (let ((pn (ensure-system-name parent)))
                     (add-edge-between-vertexes graph pn system)
                     (show-parents pn))))))
      (mapc #'show-parents canon-systems)
      (values graph))))

;;; ---------------------------------------------------------------------------

#+(and Ignore GLU-GENERIC-LOAD-UTILS)
(defun sorted-glu-systems ()
  (mapcar #'element (topological-sort (make-systems-graph))))
