#!/bin/sh

CLISP=/usr/bin/clisp

rm -f html-extract
$CLISP -x '(unless (probe-file "defsystem.fas") (compile-file "defsystem.lisp"))'
$CLISP -i defsystem.fas -x '(progn (pushnew :build *features*) (mk:compile-system :html-extract :force t))'
echo "#!$CLISP -Eterminal ISO-8859-1 -Efile ISO-8859-1" > html-extract
cat packages.fas specials.fas primitives.fas util.fas html-extract.fas >> html-extract
chmod 755 html-extract
