(in-package :araneida)

#+sbcl
(defmacro with-ignored-signals (signals &body body)
  (let ((sighandlers
         (mapcar (lambda (sig) (list sig :ignore)) signals)))
    `(sb-sys:with-enabled-interrupts ,sighandlers ,@body)))

#+cmu
(defmacro with-ignored-signals (signals &body body)
  (let ((sighandlers
         (mapcar (lambda (sig) (list sig :ignore)) signals)))
    `(system:with-enabled-interrupts ,sighandlers ,@body)))

; Cribbed from Paul Graham
(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
                     `(,s (gensym)))
                 syms)
    ,@body))

; Cribbed from PCL by Seibel
(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (loop for n in names collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
      `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
        ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
           ,@body)))))
