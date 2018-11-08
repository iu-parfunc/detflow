CC=gcc

all: foo.o foo.h bar.o bar.h

.c.h:
	$(CC) -c -o $@ $<

.c.o:
	$(CC) -c -o $@ $<

foo.o: foo.c
foo.h: foo.c
bar.o: bar.c
bar.h: bar.c

clean:
	-rm -f *.o
	-rm -f *.h
