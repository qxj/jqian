### Makefile.tpl --- 
## Time-stamp: <2009-04-21 22:48:52 lancer>
## Author: (>>>AUTHOR<<<)
## Version: $Id: (>>>FILE<<<),v 0.0 (>>>VC_DATE<<<) (>>>LOGIN_NAME<<<) Exp $
## Keywords: (>>>1<<<)
## X-URL: (>>>2<<<)

.SUFFIXES: .c
.PHONY: clean all

INCS=-I.
LIBS=-L.

CC=gcc
CXX=g++
CFLAGS=-O -DUNIX -DDEBUG -DTRACE_FILE='"./trace"' -D__PATH__='"$(PWD)/"'

EDF=

all:

(>>>POINT<<<)

%: %.c
	$(CC) -o $@ $< $(CFLAGS) $(INCS) $(LIBS)

%.o: %.c
	$(CC) -c $< $(CFLAGS) $(INCS) $(LIBS)

%: %.cc
	$(CXX) -o $@ $< $(CFLAGS) $(INCS) $(LIBS)

%.o: %.cc
	$(CXX) -c $< $(CFLAGS) $(INCS) $(LIBS)

clean:
	-rm *.o

### (>>>FILE<<<) ends here
