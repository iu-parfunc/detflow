#!/bin/bash

set -xe

export HOWMANY=${HOWMANY:-16}

top=`dirname $0`/../../
benchdir=$top/benchmark_scripts/

. $benchdir/common.sh --source-only

$benchdir/intro.sh

# Obey what the top-level script tells us to run:
hsbencher_bioinfo clustal clustal ${SUB_VARIANT}

# for variant in {det_rr,det_ld_preload,nondet}; do
#   hsbencher_bioinfo clustal clustal $variant
# done
