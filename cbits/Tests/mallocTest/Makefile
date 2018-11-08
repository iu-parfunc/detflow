# Note! This test requires aslr disabled to work!

all:
	gcc -o mallocTest mallocTest.c

clean:
	rm mallocTest

test:
	make all
	LD_PRELOAD=../../libdet.so ./mallocTest
