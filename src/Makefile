COFLAGS=-g -O3
#COFLAGS=-g
CWFLAGS=-Wall -DO_DEBUG

CFLAGS=$(COFLAGS) $(CWFLAGS)

all:	rdf_db.so

clean::
	rm -f rdf_db.so *~

rdf_db.so:	Makefile rdf_db.c rdf_db.h
		plld -Wall -pthread $(CFLAGS) -shared -o $@ rdf_db.c
