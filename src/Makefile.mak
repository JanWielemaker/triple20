HOME=C:\Documents and Settings\jan
PLHOME=$(HOME)\My Documents\src\pl
INCLUDE=$(HOME)\include;$(PLHOME);$(INCLUDE)
LIB=$(HOME)\lib;$(PLHOME)\packages\xpce\src;$(LIB)

LIBS=pl2xpce.lib user32.lib gdi32.lib pthreadVC.lib

pcecall.dll:	pcecall.c Makefile.mak
		plld -shared -o pcecall pcecall.c $(LIBS)
