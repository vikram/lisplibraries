;;; (C) By Jeremy Smith February 2006
;;; level9@decompiler.org

;;; BSD license

;;; *** Start of pol.lisp code ***

;;; Call a function in this file with (py::[function]) or
;;; (pythononlisp::[function])

(in-package :pythononlisp)

                         ; TODO: research how to load
					; packages just once, and/or
                         ; only execute code once per
                         ; session


(defparameter *path-to-lisp-executable* "/usr/bin/cmucl")

;; Note: If your python library file lives in a place not already
;; named here, this is the form you need to adjust. Scanty
;; documentation on using DEFINE-FOREIGN-LIBRARY are at
;; http://common-lisp.net/project/cffi/manual/cffi-manual.html
;;
;; More extensive documentatin is in the comments within the cffi file
;; libraries.lisp

(cffi:define-foreign-library python-library
  (:darwin (:framework "Python"))
  (:unix (:or "libpython2.5.so.1.0" "libpython2.4.so.1.0" "libpython2.3.so.1.0"))
  (:windows (:or "python25.dll" "python24.dll" "python23.dll") )
  (t (:default "libpython")))

(defmacro str-format (&rest body)
  "Like FORMAT but always returns the string"
  `(with-output-to-string (out) (format out ,@body)))

(defun atof (decimal-string)
  "Takes a numeric string and returns the number itself. Uses READ, so can handle integers, decimals, etc.."
  (with-input-from-string (s (concatenate 'string decimal-string))
    (read s)))

;; This command must be loaded before any of the following API calls are referenced, and later used by defpyfun

  (cffi:use-foreign-library python-library)

;; These must come before ANY use of them!

;; Py_InitModule4: Initializes a Python module. 
;;
;; The first 2 arguments are all that's required - the last 3 can be
;; NIL. Py_InitModule is a C macro which calls this with NILs.  methods
;; is an array of PyMethodDefs
(cffi:defcfun ("Py_InitModule4" Py_InitModule4) :void
  (name :string)
  (methods :pointer)
  (doc :string)
  (self :pointer)
  (apiver :int))

;; Py_SetProgramName: Sets the value of argv[0]. 
;;
;; If called, must be called before Py_Initialize. Default value is
;; 'python'. This value can be used to give Python hints about where
;; to find its libraries. Details at
;; http://docs.python.org/api/embedding.html#l2h-30
(cffi:defcfun ("Py_SetProgramName" Py_SetProgramName) :void
  (name :string))

;; Py_Initialize: initializes the Python interpreter 
(cffi:defcfun ("Py_Initialize" Py_Initialize) :void)

;; PyRun_SimpleString: Lets us 'eval' Python strings. Where all the work takes place
(cffi:defcfun ("PyRun_SimpleString" PyRun_SimpleString) :void
  (code :string))

;; Py_BuildValue: Build return values from pointers. 
;;
;; TODO: It *should* take multiple arguments, a la sprintf, in the same
;; manner as PyArg_ParseTuple, where the number and types of arguments
;; is expressed in eg, Py_BuildValue ("ss",var1,var2). 
;;
;; Described: http://www.python.org/doc/1.5.2p2/ext/buildValue.html
(cffi:defcfun ("Py_BuildValue" Py_BuildValue) :pointer 
  (types :string)
  (value :string))

;; PyArg_ParseTuple: 
;;
;; PyArg_ParseTuple takes n+2 arguments: The argument tuple (passed in
;; to every callback), the types expressed as a c-string with one
;; character per type, and then, for each character, a variable to hold
;; the value (hence the pointer dereferencing with mem-ref to get the
;; value). These values do not need to be freed up, because Python does
;; all that automatically
;;
;; This wrapper does not include 'n+2' arguments, just 3, but this
;; wrapper is used in LispEval and SetString because they just use one
;; argument. See below in mbox-add-field for the use of this function
;; with more than 3 arguments.
;;
;; Described: http://www.python.org/doc/1.5.2p2/ext/parseTuple.html
(cffi:defcfun ("PyArg_ParseTuple" PyArg_ParseTuple) :pointer
  (args :pointer)
  (types :string)
  (value :pointer))

;;; *** End of Python wrapper definitions ***



;;; *** Python wrapper definitions ***

;; The following code defines Lisp wrappers for C calls into Python's
;; C API. Documentation on the Python C/API is available at
;; http://docs.python.org/api/api.html and describes the correct use
;; of the wrapped functions.


                         ; TODO: 'long and 'string seem
                         ; to be locked in the py::
                         ; namespace - I don't know how
                         ; to fix this yet.
(defun generate-parsetuple-format (input)
  (let ((format-arg ""))
    (dolist (var input)
                         ;py::string and py::long actually
      (when (eq (second var) 'string)
     (setf format-arg (str-format "~as" format-arg)))
      (when (eq (second var) 'long)
     (setf format-arg (str-format "~al" format-arg))))
    format-arg))

                         ; FIXME: Because this defun is
                         ; wrapped by a defmacro with
                         ; an identical argument
                         ; signature, we assume it is a
                         ; defun for a good
                         ; reason. However, having a
                         ; macro depend on a function
                         ; throws errors if you compile
                         ; before loading, because
                         ; functions aren't yet defined
                         ; at macroexpansion time.

;; Special thanks to Pascal Bourguignon for basically writing this
;; macro! I did add a few things so it's not exactly the same.  
(defun gen-pycallback 
    (function-name return-type arguments &optional in-body)
  "Generates the macro code to take a function name, return type,
and list of args, then creates the ParseTuple call that (at
runtime) parses the args from the Python args binding, and later
on makes Lisp bindings.

It creates the BuildValue call which returns the return value, in
Lisp, or returns a NIL return value (an empty string). Finally, it
wraps it all within with-foreign-pointer calls which bind the local
Lisp variables with the given name, to the values created within
ParseTuple."
  (loop
     :with body =
     `(progn
	,(if arguments
	     `(cffi:foreign-funcall "PyArg_ParseTuple"
				    :pointer args
				    :string
				    ,(generate-parsetuple-format arguments)
				    ,@(loop 
					 :for (<name> <type>) :in arguments
					 :nconc `(:pointer ,<name>)))
	     (list))
	(if ,in-body
	    ,in-body
	    ,(if return-type
		 `(Py_BuildValue ,(generate-parsetuple-format (list (list 'dummy return-type))) 
				 (,function-name 
				  ,@(loop 
				       :for (<name> <type>) :in arguments
				       :collect `(pygetarg ,<name> (quote ,<type>)))))
		 (progn
		   `(progn
		      (,function-name ,@(loop 
					   :for (<name> <type>) :in arguments
					   :collect `(pygetarg ,<name> (quote ,<type>))))
		      (Py_BuildValue "" ""))))))
     :for (<name> <type>) :in (reverse arguments)
     :do (setf body `(cffi:with-foreign-pointer (,<name> 255) ,body))
     :finally (return body)))

                         ; FIXME: Because this defun is
                         ; wrapped by a defmacro with
                         ; an identical argument
                         ; signature, we assume it is a
                         ; defun for a good
                         ; reason. However, having a
                         ; macro depend on a function
                         ; throws errors if you compile
                         ; before loading, because
                         ; functions aren't yet defined
                         ; at macroexpansion time.
(defun gen-depyfun (name return-type args body)
  "A 'pyfun' macro, which really just takes the arguments value and
shoves the first (the variable names) into the lambda binding list of
a standard defun. See defpyfun, below."
  (declare (ignorable return-type))
  `(defun ,name
    ,(let ((arglist))
       (dolist (arg args)
         (push (first arg) arglist))
       (nreverse arglist))
    (progn
      ,@body)))

(defun pygetarg (variable type)
  "Helper function for handling python arguments, used by gen-pycallback. Should never be used directly from code - use defpyfun with its arg format"
  (case type
    (long (cffi:mem-ref variable :long))
    (string (cffi:mem-ref variable :string))))

(defmacro defpycallbackentry (function-name return-type input)
  "Define a callback entry, which is the cffi:def-callback stuff"
  (gen-pycallback function-name return-type input))

(defmacro defpycallbackfunction (function-name return-type input body)
  "Define a callback function, which is the defun stuff, but with stock Lisp arguments"
  (gen-depyfun function-name return-type input body))

(defmacro defpyfun (name return-type args &rest body) 
  "Define both a callback entry, and a callback function, and
translate the args (eg, '((key string) (value string))') into
Python-calling code. Definitely a great help.

A pycallback function has to have a defun with a name matching the
name argument and an arg list the same size as the arg list
specified. A callback entry has to have variable type definitions, and
a reference in memory."
  `(progn
    (cffi:defcallback ,name :pointer ((self :pointer) (args :pointer))
      (defpycallbackentry ,name ,return-type ,args))
    (defpycallbackfunction ,name ,return-type ,args ,body)))

;;; *** Start of pol default module functions ***

;; This holds data between Lisp and Python. Not thread-safe? Perhaps a
;; better way of passing values is to run LispEval from within Python
;; with the return value, to whatever closure
(defparameter *callbackstr* "")

;; Holds any text printed out during the session via dumpstd, returned
;; by pythonlisp
(defparameter *session-stdout* NIL)

(defpyfun LispEval string ((code string))
     (str-format "~A" (eval (read-from-string code))))

(defpyfun GetString string NIL
     *callbackstr*)

(defpyfun SetString NIL ((text string))
     (setf *callbackstr* text))

(defpyfun dumpstd NIL ((text string))
     (when *session-stdout*
          (setf *session-stdout* (str-format "~A~A" *session-stdout* text 'string)))
     (format t text))

(defvar *python-initialised* NIL)

; FIXME: URGENT: add exception handling so this throws errors on
; failure
;
; FIXME: change the hardcode string referring to the lisp executable
; here to the user-configurable parameter *path-to-lisp-executable*
;;
;; to connect cffi to python: (cffi:use-foreign-library python-library)
;; to check setting of ProgramName: (foreign-string-to-lisp (cffi:foreign-funcall "Py_GetProgramName" :pointer))
;; to check if Python is inititialized: (eq 1 (cffi:foreign-funcall "Py_IsInitialized" :int))
;;
(defun init-python (&optional (program-name "cmucl") (secure NIL))
  "Do all initialization of Python before running any code.

The argument 'program-name' should specify the path to the Lisp
executable, but I'm not sure where in Python the value is used"
  (cffi:use-foreign-library python-library)
  (Py_SetProgramName program-name)
  (Py_Initialize)
  (if secure
      (add-python-module (list "pol" 
                      (list "SetString" 1 'SetString) 
                      (list "GetString" 4 'GetString) 
                      (list "dumpstd" 1 'dumpstd)))
      (add-python-module (list "pol" 
                      (list "SetString" 1 'SetString) 
                      (list "GetString" 4 'GetString) 
                      (list "LispEval" 1 'LispEval) 
                      (list "dumpstd" 1 'dumpstd))))
  (setf *python-initialised* T)
  (pythonlisp "print \"If you can see this, Python is loaded and working\""))

;;; *** Start of module-creation functions ***

;; This is to define an entry in the Python module array.
(cffi:defcstruct PyMethodDef
  (ml_name :string)
  (ml_meth :pointer)
  (ml_flags :int)
  (ml_doc :pointer))


(defmacro defpycallstruct (count name flags callback &optional NULL)
  " Create exactly one callback array entry from the data given -
rather tied to add-python-module due to its assumption of 'methodptr',
needs rewriting "
  `(progn
    (setf 
     (cffi:foreign-slot-value (cffi:mem-aref methodptr 'PyMethodDef ,count) 'PyMethodDef 'ml_name) 
     ,name)
    (setf 
     (cffi:foreign-slot-value (cffi:mem-aref methodptr 'PyMethodDef ,count) 'PyMethodDef 'ml_flags) 
     ,flags)
    (setf 
     (cffi:foreign-slot-value (cffi:mem-aref methodptr 'PyMethodDef ,count) 'PyMethodDef 'ml_meth) 
     (if ,NULL 
      (cffi:null-pointer) 
      (cffi:get-callback ,callback)))
    (setf 
     (cffi:foreign-slot-value (cffi:mem-aref methodptr 'PyMethodDef ,count) 'PyMethodDef 'ml_doc) 
     (cffi:null-pointer))))

(defun add-python-module (methods)
  "Takes a list of function names, and generates a null-terminated
array, creating 4 lines for each function entry in the list with an
increasing array offset.

The module created is not destroyed or gabage collected until Lisp
exits.  This is to prevent Python from being able to call a module
after its memory has been freed up."
  (let ((methodptr (cffi:foreign-alloc 'PyMethodDef 
                         ; length+1 for the NULL item
                           :count (+ 1 (length (rest methods))))))

    (dotimes (i (length (rest methods)))
      (defpycallstruct i 
                    (first (elt (rest methods) i)) 
                    (second (elt (rest methods) i)) 
                 (third (elt (rest methods) i))))

    ; Add NULL terminator, so Python doesn't reading past the end
    (defpycallstruct (length (rest methods)) 
                  (cffi:null-pointer) 
                     0 
                     (cffi:null-pointer) 
                  T)
    
    ;; 1011 is Python 2.2 which I use - don't panic if it gives a warning about the wrong version
    (Py_InitModule4 (first methods) 
              methodptr 
              (cffi:null-pointer) 
              (cffi:null-pointer) 
              1011)

    (pyrun_simplestring (str-format "import ~a" (first methods)))))

;;; *** End of module-creation functions ***

;;; *** Start of PyRun_SimpleString wrapper ***

(defmacro py (code)
  "A shorthand: (py ...) = (pythonlisp ...)"
  `(pythonlisp ,code))

(defun pythonlisp (code &optional (redirect-stdout 'CONSOLE) methods)
  "Executes the string CODE as Python code.

The second argument is the module you want to create for this bit of
Python code (or not) but will stay in memory for any additional
sessions.

The third argument (default switched on) states that you want to see
any Python output in the Lisp REPL and/or returned from pythonlisp as
a string. It adds overhead, so turn it off for extensive Python calls
where you don't need to see what's going on."
  ; Initialize Python if necessary
  (unless *python-initialised*
    (init-python))
  
  (when redirect-stdout
    (when (eq redirect-stdout 'CONSOLE)
      (setf *session-stdout* NIL))
    (when (eq redirect-stdout 'STRING)
      (setf *session-stdout* "")))
  
  ;; Adds them permanently. I think the 'methods' parameter to
  ;; pythonlisp could be removed completely, and I'd rather ask people
  ;; to specifically define a module before calling it.
  (when methods
    (add-python-module methods))
  
  ;; I don't know if this has to be imported every time, or just
  ;; once per session. Non-Clisp Lisps don't catch any Python
  ;; stdout without this. I think it can be improved by removing
  ;; the 'class Sout' and the imports.
  ;;
  ;; At least the 'sys.std*' lines have to be called every time
  ;;
  ;; This inline code has to be aligned far-left, so Python can
  ;; get the right indents
  (let ((header
"class Sout:
     def write(self, stdstring):
          pol.dumpstd(stdstring)
import pol
import sys
sys.stdout = Sout()
sys.stderr = Sout()
#sys.sydin = None
"))
    (let ((code-to-execute (if methods (str-format "import ~a~%~a" (first methods) code) code)))
      ;; Redirecting stdout is necessary if you want to get Python's printed output, but creates additional overhead
      (if redirect-stdout
       (pyrun_simplestring (str-format "~a~a~%" header code-to-execute))
       (pyrun_simplestring code-to-execute)))
    *session-stdout*))

;;; *** End of PyRun_SimpleString wrapper ***

;;; *** Start of some simple example 'toys' ***


;; EXAMPLE: Emulating the Python REPL (just for fun)

;; Use ~% for linebreaks (terrible use of str-format for this)
;;
;; Kind of 'intelligent' like the Python REPL (prompt=">>>"): after a
;; line ending in ':', it prints new lines starting with the prompt
;; "...". You indent each line appropriately until you have finished
;; the block, and then you insert an empty line to execute the code,
;; all in one go. Seems to work! Even the Python REPL doesn't evaluate
;; until the indented block is at an end (checks for a blank line).
;;
;; Doesn't seem to evaluate expressions on their own (eg, just running
;; the Python code "20" will print nothing, but "print 20" will), you
;; have to use print, perhaps this could be added to the execution
;; string
;;
(defun py-repl ()
  "Open a python REPL"
  (format t "Welcome to the Python-on-lisp REPL emulator - enter on a blank line to quit~%>>> ")
  (loop for line = (read-line) while (not (equal line "")) do
     ;;Doesn't work if the line ends in a ": " or similiar
     (if (eq (elt (reverse line) 0) #\:)
         (let ((indented-block line))
           (format t "... ")
                    ;;;Can't be bothered to see if the line is validly indented or not!
           (loop for line = (read-line) while (not (equal line "")) do
              (setf indented-block (str-format "~a~%~a" indented-block line))
              (format t "... "))
           (py (str-format "~a~%" indented-block)))
         (py (str-format line)))
     (format t ">>> ")))


;; EXAMPLE: Getting a web page with python

;; Returns the web page text (though it is more efficient to use
;; SetString) or a list of errors if something went wrong
(defun get-web-page (url)
     (pythonlisp
          (str-format
"from httplib import HTTP
import urllib
import string
f = urllib.urlopen(\"~a\")
print f.read()
#More efficient than print f.read()
#pol.SetString(f.read()
          " url)'STRING))

;; EXAMPLE: Processing mbox files with python

(defparameter *mbox-message* NIL)
(defparameter *mbox-messages* NIL)
(defparameter *mbox-ids* (make-hash-table :test #'equal))

;     Gets an mbox file into a list of (sort of alists)
;     lists. Demonstrates callbacks and creating a custom callback
;     module, and the ease of Python to do certain things.
;
;     Just a note of interest, but mbox_add_field could take a
;     numeric value to specify which map the value is to go into,
;     the value being incremented at the start of get-mbox-messages
;     and this could be threadsafe, in that you could call
;     get-mbox-messages from several threads. It wouldn't be totally
;     threadsafe, but it's a start.
;
;     Even better might just be to create a lambda expression with
;     its own message list and field hashmap, and pass the closure
;     as a new CFFI callback. That might be possible using gensym.
;
(defun get-mbox-messages (filename &optional fresh)
  "Opens an mbox file"
  (setf *mbox-message* (list))
  (when fresh
    (setf *mbox-messages* (list)))
  (pythonlisp 
   (str-format
"import mailbox,rfc822,sys,os,string,re
mb = mailbox.UnixMailbox(file(\"~a\",'rb'))
msg = mb.next()
while msg is not None:
     for i in msg.keys():
          polmbox.mbox_add_field(i,msg.getheader(i))
     polmbox.mbox_add_field('Contents',msg.fp.read())
     polmbox.mbox_add_email()
     msg = mb.next()" filename)
          ;Here is the argument to pythonlisp: a custom callback table, with a module name (which Python sees), a table of:
               ;;strings (the name Python will see)
               ;;numbers (function return type, which I should define an ENUM for, use '1' for now) and
               ;;callbacks (as a symbol to the CFFI callback lookup function)
          ;No stdout, as we're using solely callbacks for data
          NIL
          ;Create this module, polmbox
          (list "polmbox" (list "mbox_add_header" 1 'mbox-add-header) (list "mbox_add_field" 1 'mbox-add-field) (list "mbox_add_email" 4 'mbox-add-email)))
     ;After execution, return this variable which will now contain all the mbox messages
     *mbox-messages*)

;;These 2 functions are totally experimental, and for my use only
;;(defun write-mbox-message (message stream)
;;     ;This is required to separate messages in an MBOX file - but Python does not handle it
;;     (format stream "From - Sat Apr 01 20:38:32 1994~%")
;;     (format stream "From: ~A~%" (second (assoc "from" message :test #'equal)))
;;     (dolist (header message)
;;          (when (not (equal (first header) "Contents"))
;;               (format stream "~a: ~a~%" (first header) (second header))))
;;     (format stream "~A~%" (second (assoc "Contents" message :test #'equal))))

;;(defun write-mbox-messages (messages filename)
;;     (with-open-file (file filename :direction :output :if-exists :supersede :if-does-not-exist :create)
;;          (dolist (message messages)
;;               (write-mbox-message message file))))

(defpyfun mbox-add-field NIL ((key string) (value string))
     ;Detects messages with duplicate IDs
;     (when (equal key "message-id")
;          (when (gethash value *mbox-ids*)
;               (break))
;          (setf (gethash value *mbox-ids*) T))
     (push (list key value)*mbox-message*))

(defpyfun mbox-add-header NIL ((value string))
     (push value *mbox-message*))

;; Here, the return type and args are both NIL, this works fine.
(defpyfun mbox-add-email NIL NIL
       (push *mbox-message* *mbox-messages*)
       (setf *mbox-message* (list)))

;; An example of what you can do with this data
(defun find-mbox-author (messages author)
  "Returns items in MESSAGES with given AUTHOR"
  (let ((retval))
    (dolist (msg messages)
      (when (equal author (second (assoc "from" msg :test #'equal)))
     (push msg retval)))
    retval))


;;; EXAMPLE: using a user-defined module
;;
;; Warning: Don't call any defun function the same name as a
;; defpyfun. That's because it's the same name, so the latter will
;; overwrite the former. Just treat a pyfun like any other.
;;
(defun testmynum ()
  "Calls test.testnum(50)"
  (pythonlisp
   "import test
test.testnum(50)
"
   NIL
   (list "test" (list "testnum" 1 'testnum))))

(defpyfun testnum NIL ((number long))
       (print number))

;;; *** End of some simple example 'toys' ***

;;; *** End of pol.lisp code ***


