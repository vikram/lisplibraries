(in-package :cl-user)

;;; given a system definition, create a "Makefile" that copies all the
;;; source files in it into a given directory

(defun print-source-file-name (component stream)
  (case (make::component-type component)
    ((:file)
     (format stream "~A " 
             (enough-namestring
              (translate-logical-pathname
               (pathname
                (make::component-full-pathname component :source))))))
    ((:module :defsystem)
     (loop for c in (make::component-components component)
           do (print-source-file-name c stream)))
    (t (format t "Doing nothing for ~A ~%" component))))

(defun make-makefile (system output-file)
  "List files in SYSTEM, in the order that they need to be processed"
  (with-open-file (o output-file :direction :output
                     :if-exists :rename)
    (format o "# Automatically generated, edit make-makefile.lisp instead

FILES=")
    (let* ((system-definition (make::find-system system))
           (*default-pathname-defaults*
            (translate-logical-pathname (make::component-source-root-dir system-definition))))
      (print-source-file-name system-definition o))
    (format o "

all:
	@echo That did nothing: now look at the defsystem

release: debian/changelog
	head -1 debian/changelog |cut -d' ' -f2 | tr -d '()' > release


install:
	install -d $(DESTDIR)/usr/share/common-lisp/repositories/~A
	tar cf - $(FILES) ~A.system | (cd $(DESTDIR)/usr/share/common-lisp/repositories/~A && tar xvpf -)


clean:
	-rm a.out *~~ diffs *.*f ~%"
            (string-downcase (symbol-name system))
            (string-downcase (symbol-name system))
            (string-downcase (symbol-name system)))))



