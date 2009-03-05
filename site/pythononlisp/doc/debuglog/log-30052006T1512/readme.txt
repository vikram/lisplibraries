These two text files record the output of installing pythononlisp
using asdf, and then trying to run (py::py "print \""Hello world\"").

As they indicate, pythononlisp now installs successfully on OS X with
only style warnings. However, it still does not function
correctly. Somewhere within the function PYTHONLISP, a function
expects an argument to be a pointer to a foreign string but is instead
passed what is already a valid lisp string.

This log was generated on:
- OS X 10.4.6
- SBCL ppc 0.9.7
- SLIME cvs version of 2006-3-20
- cffi version 0.9.1

A.Gallagher,
recorded on 30052006T1520
