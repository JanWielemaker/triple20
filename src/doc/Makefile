# Generated automatically from Makefile.in by configure.
################################################################
# SWI-Prolog `RDF' package
# Author:    Jan Wielemaker. jan@swi.psy.uva.nl
# Copyright: LGPL (see COPYING or www.gnu.org
################################################################

.SUFFIXES: .tex .dvi .doc .pl

SHELL=/bin/sh
PLHOME=/staff/jan/src/pl
PCEHOME=$(PLHOME)/packages/xpce
RUNTEX=$(PLHOME)/man/runtex

DOCTOTEX=$(PCEHOME)/bin/doc2tex
PLTOTEX=$(PCEHOME)/bin/pl2tex
LATEX=latex
DOC=triple20
TEX=$(DOC).tex
DVI=$(DOC).dvi
PDF=$(DOC).pdf
HTML=$(DOC).html

################################################################
# Documentation
################################################################

doc:		$(PDF) $(HTML)
pdf:		$(PDF)
html:		$(TEX) $(HTML)

$(HTML)::
		latex2html $(DOC)
		mv html/index.html $@
		cp -a html/*.gif .

$(PDF):		$(TEX)
		$(RUNTEX) --pdf $(DOC)

$(TEX):		$(DOCTOTEX)

.doc.tex:
		$(DOCTOTEX) $*.doc > $*.tex
.pl.tex:
		$(PLTOTEX) $*.pl > $*.tex

################################################################
# Clean
################################################################

clean:
		rm -f *~ *% config.log

distclean:	clean
		rm -f $(TARGETS) config.cache config.status Makefile
		rm -f $(TEX)
		runtex --clean $(DOC)
