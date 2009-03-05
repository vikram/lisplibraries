#!/bin/bash
SBCL=sbcl
CMUCL=cmucl

# The test program has a unique string that it is supposed to produce and look for
# to make sure that we are not dealing with a stale server.
# Not that that's ever happened to me...twice....
randomtext=${RANDOM}${RANDOM}${RANDOM}${RANDOM} # I make it long-ish so as to make it improbable to find naturally

# In order to add a new lisp to the test suite, you need to alter 3 areas in test-server.lisp
#    - Add any backtrace information you want to *invoke-debugger-hook*
#    - Alter GET-RANDOM-TEXT to properly get the random text from the command line
#    - Below :testend add a quit function, or whatever will make your lisp properly terminate
#
# Then add the proper call to the case statement below.
#
# I believe the test suite will work fine for both threaded and non-threaded servers,
# but YMMV.

case "$1" in
    sbcl)
	echo "Running test suite for SBCL."
	$SBCL --noprint --load "test-server.lisp" --end-toplevel-options "$randomtext" > /dev/null &
	sleep 15
	;;
    sbcl-serve-event)
	# force use of serve-event, not threads
	echo "Running test suite for SBCL (force serve-event)."
	$SBCL --noprint --load "test-server.lisp" --end-toplevel-options "$randomtext" "serve-event" > /dev/null &
	sleep 15
	;;
    cmucl)
	echo "Running test suite for CMUCL."
	$CMUCL -quiet -load "test-server.lisp" "$randomtext" > /dev/null &
	sleep 10
	;;
    *)
	echo "Usage: sh test.sh [lisp name]"
	echo "I currently know about: sbcl sbcl-serve-event cmucl"
	exit 1
esac
perl test-server-walker.pl $randomtext

