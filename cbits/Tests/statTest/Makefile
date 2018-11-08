all:
	gcc -o stat stat.c
	gcc -o lstat lstat.c
	gcc -o fstat fstat.c

clean:
	rm stat lstat fstat

test:
	make all
	LD_PRELOAD=../../libdet.so ./stat
	LD_PRELOAD=../../libdet.so ./lstat
	LD_PRELOAD=../../libdet.so ./fstat
