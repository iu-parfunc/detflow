#!/bin/bash

set -x -e -o pipefail

PROCS=`getconf _NPROCESSORS_ONLN`
DEFAULTTHREADS=`seq 1 ${PROCS}`
THREADSETTINGS=${THREADSETTINGS:-$DEFAULTTHREADS}

REGRESSES="--regress=allocated:iters --regress=bytesCopied:iters --regress=cycles:iters --regress=numGcs:iters --regress=mutatorWallSeconds:iters --regress=gcWallSeconds:iters --regress=cpuTime:iters"

# Build the tiny, almost-empty executable:
gcc -O3 empty.c

stack $STACK_ARGS setup
rm -f microbenchmarks_*.json microbenchmarks_*.csv microbenchmarks_*.html

(cd ../../benchmark_scripts/hsbencher && \
        stack $STACK_ARGS setup && \
        stack $STACK_ARGS install hsbencher-fusion)

HSB=../../benchmark_scripts/hsbencher-fusion-upload-criterion
# Needs Version: 0.3.16.2
$HSB -h

# Run all:
WHICH=
# WHICH='*readProcess*'

for T in $THREADSETTINGS ;
do
    set +x
    echo; echo "Running threads=$T"
    echo "============================================================"
    set -x
#    stack bench microbenchmarks --benchmark-arguments="$REGRESSES -o microbenchmarks_N${T}.html --json microbenchmarks_N${T}.json +RTS -s -N${T} -RTS -m glob ${WHICH}";
    stack exec -- detmonad-microbenchmarks $REGRESSES -o microbenchmarks_N${T}.html --json microbenchmarks_N${T}.json +RTS -s -N${T} -RTS -m glob ${WHICH};
    
    $HSB -j microbenchmarks_N"${T}".json --threads="${T}" --noupload --csv=microbenchmarks_N"${T}".csv;
done
