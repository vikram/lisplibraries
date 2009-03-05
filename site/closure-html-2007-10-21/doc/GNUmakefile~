USER=dlichteblau

all: index.html

index.html: index.xml index.xsl
	xsltproc index.xsl $< >index.html

.PHONY: push
push: all
	scp index.html index.css $(USER)@common-lisp.net:/project/closure/public_html/closure-html/
