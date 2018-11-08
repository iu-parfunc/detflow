all:
	gcc -o fork fork.c
	gcc -o forkWait forkWait.c
	gcc -o forkWaitid forkWaitid.c
	gcc -o forkWaitpid forkWaitpid.c

clean:
	rm fork forkWait forkWaitid forkWaitpid

test:
	make all
	LD_PRELOAD=../../libdet.so ./fork
	LD_PRELOAD=../../libdet.so ./forkWait
	LD_PRELOAD=../../libdet.so ./forkWaitpid
	LD_PRELOAD=../../libdet.so ./forkWaitid
