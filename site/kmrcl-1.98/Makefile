.PHONY: all clean test test-acl test-sbcl

test-file:=`pwd`/run-tests.lisp
all:

clean:
	@find . -type f -name "*.fasl*" -or -name "*.ufsl" -or -name "*.x86f" \
	  -or -name "*.fas" -or -name "*.pfsl" -or -name "*.dfsl" \
	  -or -name "*~" -or -name ".#*" -or -name "#*#" | xargs rm -f

test: test-alisp

test-alisp:
	alisp8 -q -L $(test-file)

test-mlisp:
	mlisp -q -L $(test-file)

test-sbcl:
	sbcl --noinform --disable-debugger --userinit $(test-file)

test-cmucl:
	lisp -init $(test-file)

test-lw:
	lw-console -init $(test-file)

test-scl: 
	scl -init $(test-file)

test-clisp: 
	clisp -norc -q -i $(test-file)
