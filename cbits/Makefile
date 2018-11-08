CC=gcc
# Not sure why "--no-as
CFLAGS= -g -Wall -Wextra -Wconversion -fPIC -shared -Wl,--no-as-needed -std=c99
LIBS=-ldl -lm


all: libdet

libdet: libdet.c
	$(CC) $(CFLAGS) -o libdet.so libdet.c $(LIBS)

test:
	valgrind -q ./sampleRun.sh

.PHONY: clean
clean:
	rm *.so testOpen
	echo "Clean done!"
