LISP=openmcl -b

all: intro wiki source rest

intro: html/intro/index.html pdf/intro.pdf

html/intro/index.html: intro.tex .latex2html-init
	mkdir -p html/intro
	latex2html -dir html/intro intro.tex
	cp intro.css html/intro/style.css

pdf/intro.pdf: intro.tex
	mkdir -p pdf/
	cp intro.tex pdf/
	(cd pdf && pdflatex intro.tex && pdflatex intro.tex && cd ..)

wiki:
	mkdir -p html/wiki/ pdf/
	cat wiki-make.lisp | $(LISP)
	cp wiki.css html/wiki/style.css
	(cd pdf && pdflatex wiki.tex && pdflatex wiki.tex && cd ..)

source: 
	mkdir -p html/source/ pdf/
	cat source-make.lisp | $(LISP)
	cp source.css html/source/style.css
	(cd pdf && pdflatex source.tex && pdflatex source.tex && cd ..)

rest: 
	mkdir -p html/rest/
	cp rest.html html/rest/rest.html
