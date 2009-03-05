#!/bin/sh

# darcs does not support symlinks yet, therefore we have this file to set them up.
# they are only needed for backends that does not support directory-publishing
# (e.g. apache with mod_lisp)

EXAMPLES_HOME="`dirname $0`"

if [ ! -e "$EXAMPLES_HOME" ]; then
    echo Hm, cofused, exiting...
    exit 1
fi

cd wwwroot
ln -sf ../../wwwroot/ucw
ln -sf ../../wwwroot/dojo

ln -sf ../../wwwroot/ucw l10n/
ln -sf ../../wwwroot/dojo l10n/

ln -sf ../../wwwroot/ucw shared-counter/
ln -sf ../../wwwroot/dojo shared-counter/

