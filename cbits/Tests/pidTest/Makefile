all:
	gcc -o pid pid.c

clean:
	rm pid

test:
	make all
	LD_PRELOAD=../../libdet.so ./pid
