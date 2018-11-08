CC=gcc

all: main

dep1.o: dep1.c dep1.h
	$(CC) -Wall -c $< -o $@

dep2.o: dep2.c dep2.h
	$(CC) -Wall -c $< -o $@

dep3.o: dep3.c dep3.h
	$(CC) -Wall -c $< -o $@

dep4.o: dep4.c dep4.h
	$(CC) -Wall -c $< -o $@

dep5.o: dep5.c dep5.h
	$(CC) -Wall -c $< -o $@

dep6.o: dep6.c dep6.h
	$(CC) -Wall -c $< -o $@

dep7.o: dep7.c dep7.h
	$(CC) -Wall -c $< -o $@

dep8.o: dep8.c dep8.h
	$(CC) -Wall -c $< -o $@

main.o: main.c
	$(CC) -Wall -c $< -o $@

main: main.o dep1.o dep2.o dep3.o dep4.o dep5.o dep6.o dep7.o dep8.o
	${CC} $^ -Wall -o $@

clean:
	-rm -f *.o
	-rm -f main
