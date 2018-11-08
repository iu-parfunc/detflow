#!/bin/bash

# This should be run via "stack exec" or in an environment with the
# detflow executable in path.

set -e

rm -rf out/*
detflow -i in/ -o out/ Cat.hs

# cat out/blah.txt

