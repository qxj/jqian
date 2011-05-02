### (>>>FILE<<<) ---
## Time-stamp: <Julian Qian 2011-05-02 14:55:54>
## Author: (>>>AUTHOR<<<)
## Version: $Id: (>>>FILE<<<),v 0.0 (>>>VC_DATE<<<) (>>>LOGIN_NAME<<<) Exp $
## Keywords: (>>>1<<<)
## X-URL: (>>>2<<<)

.SUFFIXES: .c .cpp
.PHONY: check-syntax clean all

CC=gcc
CXX=g++
CFLAGS=-g -O0 -std=c99 -Wall -Wextra
CXXFLAGS=-g -O0 -Wall -Wextra

EDF=

INCS=-I.
LIBS=-L.
SRCS=(>>>POINT<<<)

all:

# flymake
check-syntax:
	$(CXX) $(CXXFLAGS) -Wall -Wextra -pedantic -fsyntax-only $(SRCS)


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