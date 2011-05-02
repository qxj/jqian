### (>>>FILE<<<) ---
## Time-stamp: <Julian Qian 2011-05-02 14:54:18>
## Author: (>>>AUTHOR<<<)
## Version: $Id: (>>>FILE<<<),v 0.0 (>>>VC_DATE<<<) (>>>LOGIN_NAME<<<) Exp $
## Keywords: (>>>1<<<)
## X-URL: (>>>2<<<)

.SUFFIXES: .c .cpp

CC=g++

TEST = test

ALL: ${TEST}

TEST_O = test.o

${TEST}: ${TEST_O}
	$(CC) -o ${TEST} ${TEST_O}

%.o:%.cpp %.h
	$(CC) -Wall -g -O0 -o $@ -c $<

clean:
	-rm -f *.o
	-rm -f ${TEST}
