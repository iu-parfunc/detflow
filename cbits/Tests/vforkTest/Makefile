all:
	gcc -o vfork vfork.c
clean:
	rm vfork
test:
	make all
	LD_PRELOAD=../../libdet.so ./vfork
