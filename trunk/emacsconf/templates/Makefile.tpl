### (>>>FILE<<<) ---
## Time-stamp: <Julian Qian 2011-01-21 11:50:33>
## Author: (>>>AUTHOR<<<)
## Version: $Id: (>>>FILE<<<),v 0.0 (>>>VC_DATE<<<) (>>>LOGIN_NAME<<<) Exp $
## Keywords: (>>>1<<<)
## X-URL: (>>>2<<<)

.SUFFIXES: .c .cpp
.PHONY: check-syntax clean all

CC=gcc
CXX=g++
CFLAGS=-g -O0 -DUNIX -DDEBUG -std=c99
CXXFLAGS=-g -O0 -DUNIX -DDEBUG -std=c99

EDF=

INCS=-I.
LIBS=-L.
SRCS=(>>>POINT<<<)

# flymake
check-syntax:
	$(CXX) $(CXXFLAGS) -Wall -Wextra -pedantic -fsyntax-only $(SRCS)

all:


%: %.c
	$(CC) -o $@ $^ $(CFLAGS) $(INCS) $(LIBS)

%.o: %.c
	$(CC) -o $@ -c $^ $(CFLAGS) $(INCS) $(LIBS)

%: %.cpp
	$(CXX) -o $@ $^ $(CXXFLAGS) $(INCS) $(LIBS)

%.o: %.cpp
	$(CXX) -o $@ -c $^ $(CXXFLAGS) $(INCS) $(LIBS)

clean:
	-rm *.o

### (>>>FILE<<<) ends here