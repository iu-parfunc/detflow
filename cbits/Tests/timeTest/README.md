Tests related to time.

We verify we are able to catch general calls to time functions.

gettimeofday uses VDSO so we make sure this works with LD_PRELOAD.
