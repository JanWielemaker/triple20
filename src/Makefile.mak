INCLUDE=$(INCLUDE);$(HOME)\include
LIB=$(LIB);$(HOME)\lib

DEFS=-DO_DEBUG -DO_REENTRANT

all:	rdf_db.dll

rdf_db.dll:	Makefile rdf_db.c rdf_db.h
		plld $(DEFS) -shared -o $@ rdf_db.c pthreadVC.lib
