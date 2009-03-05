#!/bin/bash

HTTP_GET=`which wget`
CVS=`which cvs`
DARCS=`which darcs`
CL_LAUNCH=`which cl`

echo = "Getting UCW and all related libraries (this may take a while)" =

function darcs_get {
    echo = Getting $1 from $2 =
    ${DARCS} get --no-pristine-tree --partial $2 $3
}

function http_get {
    echo = Getting $1 from $2 =
    TMPFILE=`tempfile`
    ${HTTP_GET} "$2" -O ${TMPFILE}
    tar -zxf ${TMPFILE}
    rm ${TMPFILE}
}

function cvs_get {
    echo = Getting $1 from $2 =
    ${CVS} -d$3 $4
}

# Grab all the code
darcs_get ucw_dev http://common-lisp.net/project/ucw/repos/ucw_dev/
darcs_get ucw_ajax http://common-lisp.net/project/ucw/repos/ucw_ajax/

cd ucw_ajax
darcs optimize --sibling ../ucw_dev --relink
cd ..

mkdir dependencies
cd dependencies

darcs_get trivial-garbage http://common-lisp.net/~loliveira/darcs/trivial-garbage/
darcs_get local-time http://common-lisp.net/project/local-time/darcs/local-time/
darcs_get bordeaux-threads http://common-lisp.net/project/bordeaux-threads/darcs/bordeaux-threads/
darcs_get arnesi http://common-lisp.net/project/bese/repos/arnesi_dev/
darcs_get yaclml http://common-lisp.net/project/bese/repos/yaclml/
darcs_get parenscript http://common-lisp.net/project/ucw/repos/parenscript/
darcs_get iterate http://common-lisp.net/project/iterate/darcs/iterate
darcs_get rfc2388 http://common-lisp.net/project/ucw/repos/rfc2388/ 
darcs_get rfc2109 http://common-lisp.net/project/rfc2109/rfc2109/
darcs_get cl-l10n http://common-lisp.net/project/cl-l10n/repos/cl-l10n/
http_get slime "http://common-lisp.net/cgi-bin/viewcvs.cgi/root.tar.gz?root=slime&view=tar"
mv slime slime.delme
mv slime.delme/slime .
rm -r slime.delme
http_get cl-ppcre http://weitz.de/files/cl-ppcre.tar.gz
http_get cl-fad http://weitz.de/files/cl-fad.tar.gz
http_get split-sequence http://common-lisp.net/project/ucw/ucw-boxset/split-sequence.tar.gz
darcs_get trivial-sockets http://common-lisp.net/project/bese/repos/trivial-sockets_until-i-can-merge-with-the-mainline
http_get puri http://files.b9.com/puri/puri-latest.tar.gz
darcs_get detachtty http://common-lisp.net/project/bese/repos/detachtty/
http_get net-telent-date http://common-lisp.net/project/ucw/ucw-boxset/net-telent-date.tar.gz
http_get parse-number http://common-lisp.net/project/asdf-packaging/parse-number-latest.tar.gz

mkdir asdf
${HTTP_GET} "http://cclan.cvs.sourceforge.net/*checkout*/cclan/asdf/asdf.lisp?revision=1.98" -O asdf/asdf.lisp

echo "Done."
