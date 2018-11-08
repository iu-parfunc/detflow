#!/bin/bash

set -xe

# Tuned for a small run, artifact evaluation:
HOWMANY=${HOWMANY:-8}

top=`dirname $0`/../../
benchdir=$top/benchmark_scripts/

. $benchdir/common.sh --source-only

$benchdir/intro.sh

hsbencher_bioinfo bwa bwa ${SUB_VARIANT}

# for variant in {nondet,det_rr,det_ld_preload}; do
#     hsbencher_bioinfo bwa bwa $variant
# done
