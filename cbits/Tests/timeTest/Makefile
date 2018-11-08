all: build test

build:
	gcc -o time time.c

clean:
	rm time

test:
	DEBUG=5 ../../../bin/with_libdet ./time

test_old:
	DEBUG=4 DETIO_URAND_PATH=/dev/urandom DETIO_R_PERMS_LIST= DETIO_W_PERMS_LIST= \
         LD_PRELOAD=../../libdet.so ./time
