
include ../Makefile.mk

doc:

install:
	ln -sf `pwd`/pxmlutils.asd $(ASDF_CENTRAL_REGISTRY)

dist:
	rm -rf "{arch}"
	rm -rf $(shell find . -type d -name ".arch-ids")
