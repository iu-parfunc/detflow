

all: a.out process_bomb.exe

process_bomb.exe: process_bomb.c
	gcc -O3 -fcilkplus $^ -o $@

a.out:
	gcc -O3 empty.c
