#!/bin/sh

rm -rf ucw-boxset ucw-boxset.tar ucw-boxset.tar.gz

darcs get http://common-lisp.net/project/ucw/repos/ucw-boxset/

cd ucw-boxset
sh ./get-all.sh
cd ..

tar -zcf ucw-boxset.tar.gz ucw-boxset
scp ucw-boxset.tar.gz mbaringer@common-lisp.net:/project/ucw/public_html/
