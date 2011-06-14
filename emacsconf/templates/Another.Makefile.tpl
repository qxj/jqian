### (>>>FILE<<<) ---
## Time-stamp: <Julian Qian 2011-06-13 12:57:14>
## Author: (>>>AUTHOR<<<)
## Version: $Id: (>>>FILE<<<),v 0.0 (>>>VC_DATE<<<) (>>>LOGIN_NAME<<<) Exp $
## Keywords: (>>>1<<<)
## X-URL: (>>>2<<<)

.SUFFIXES: .h .cpp

CC=g++

TEST = test
TEST_O = test.o libone.o libtwo.o

ALL: ${TEST}

${TEST}: ${TEST_O}
	$(CC) -o ${TEST} ${TEST_O}

%.o:%.cpp %.h
	$(CC) -Wall -g -O0 -o $@ -c $<

# for solo cpp
%.o:%.cpp
	$(CC) -Wall -g -O0 -o $@ -c $<

clean:
	-rm -f *.o
	-rm -f ${TEST}
