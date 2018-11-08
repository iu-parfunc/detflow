all:
	gcc -o macro macro.c

clean:
	rm macro

test:
	LD_PRELOAD=../../libdet.so make all #When compiled with libdet.so program behaves deterministically.
	./macro
