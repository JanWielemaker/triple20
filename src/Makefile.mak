HOME=C:\Documents and Settings\jan
INCLUDE=$(HOME)\include;$(INCLUDE)
LIB=$(HOME)\lib;$(LIB)
LIBS=user32.lib gdi32.lib pthreadVC.lib

pcecall.dll:	pcecall.c Makefile.mak
		plld -shared -o pcecall pcecall.c $(LIBS)
