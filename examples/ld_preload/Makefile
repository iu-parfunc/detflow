all: main custom_fopen.so test

main: main.c
	$(CC) main.c -o main

custom_fopen.so: custom_fopen.c
	$(CC) -shared -o custom_fopen.so custom_fopen.c

test: main custom_fopen.so
	LD_PRELOAD=./custom_fopen.so ./main
