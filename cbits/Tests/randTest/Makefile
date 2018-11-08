all:
	gcc -o randTest randTest.c
clean:
	rm randTest
test:
	make all
	LD_PRELOAD=../../libdet.so ./randTest
