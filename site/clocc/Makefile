# $Id: Makefile,v 1.11 2000/07/12 18:21:03 sds Exp $
# $Source: /cvsroot/clocc/clocc/Makefile,v $

TOP := $(shell pwd)
LISPEXT := lisp
SOURCES := clocc
include $(TOP)/clocc.mk

TOP_DEP = clocc.$(FASLEXT) src/defsystem-3.x/defsystem.$(FASLEXT)

ifneq ($(DO_DUMP),)
ifneq ($(DUMPEXT),)
clocc-top: clocc-top$(DUMPEXT)
endif

clocc-top$(DUMPEXT): $(TOP_DEP)
	$(RUNLISP) $(patsubst %,-i %,$^) -d clocc-top

else

clocc-top: clocc-top.$(FASLEXT)

clocc-top.$(FASLEXT): $(TOP_DEP)
	$(RUNLISP) -cat $^ > $@

endif

recursive-clean: force
	for x in `find . -type d ! -name CVS`; do \
		if [ -r $${x}/Makefile ]; then $(MAKE) -C $${x} clean; \
		else TOP=$(TOP) $(MAKE) -C $${x} -f $(TOP)/clocc.mk clean; \
		fi ; \
	done

cvs.log: force
	cvs log > $@ 2>/dev/null

cvs-stat: cvs.log
	@fgrep "author:" cvs.log | sed 's/^.*author: \([^;]*\);.*$$/\1/' | \
		sort | uniq -c | sort | sed 's/^/    /';
	@fgrep "author:" cvs.log | wc -l;
	$(RUNLISP) -i clocc -i src/cllib/base -i src/cllib/cvs \
		-x '(funcall (intern "CVS-STAT-LOG" :cllib) "cvs.log")'
