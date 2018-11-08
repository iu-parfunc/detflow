# Libdet: A determinzing runtime

This folder contains all the code and tests for the C parts of the program.
That is, the clib wrappers to catch calls to various souce on nondeterminism.
These include things like time, randomness, file system interaction and more.

## Install
Assuming a standard Linux enviornment, calling make will create `libet.so`.
A shared object to be linked against executable files.

## Running
libdet is called from detflow and relies on detflow for setting up the enviornment.
Mainly, enviornment variables that libdet expects e.g. DETIO_R_PERMS_LIST.

We sometimes want to run just libdet, either for testing or other. For this, we
provide a simple environment to set up the expected variables.

`exportVariables.sh` need to be sourced:

```
source exportVariables.sh
```

An executable may be ran as:

```
LD_PRELOAD=./libdet.so ./exectuable
```


