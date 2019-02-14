CFLAGS=-Wall -g --std=c++0x 

all: tests sha256

sha256: sha256.cpp anser.h
	g++ $(CFLAGS) -D__MAIN__ -g -o sha256 sha256.cpp

tests: tests.cpp sha256.cpp anser.h
	g++ $(CFLAGS) -o tests tests.cpp sha256.cpp

clean: 
	rm -rf $? *~ *.dSYM *.o tests sha256
