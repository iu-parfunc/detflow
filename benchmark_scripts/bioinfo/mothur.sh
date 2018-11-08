#!/bin/bash

set -xe

HOWMANY=${HOWMANY:-4}

top=`dirname $0`/../../
benchdir=$top/benchmark_scripts/

. $benchdir/common.sh --source-only

$benchdir/intro.sh

hsbencher_bioinfo mothur mothur ${SUB_VARIANT}

# for variant in {det_rr,det_ld_preload,nondet}; do
#   hsbencher_bioinfo mothur mothur $variant
# done
