#!/bin/sh
cd $(dirname $0)/..
home=$(pwd)
name=$(basename $home)
dir=${name}-$(date --iso)

TMPDIR=`mktemp -d /tmp/dist.XXXXXXXXXX`
cleanup() {
    cd
    rm -rf $TMPDIR
}
trap cleanup exit

sbcl --load build/atdoc.lisp --eval '(quit)'

cd $TMPDIR
git clone $home $dir
rm -rf $dir/.git
rsync -a $home/doc $dir/

make -C $dir
make -C $dir/tutorial

tgz=$TMPDIR/${dir}.tgz
tar czf $tgz $dir
gpg -b -a $tgz

mv $tgz $home/build/
