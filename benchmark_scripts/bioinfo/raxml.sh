#!/bin/bash

set -xe

# HOWMANY=512

# Tuned for a small run, artifact evaluation:
HOWMANY=${HOWMANY:-128}

# For RR only:
# HOWMANY=1

top=`dirname $0`/../../
benchdir=$top/benchmark_scripts/

source $benchdir/common.sh --source-only

$benchdir/intro.sh

# Which variant to run is set by Jenkins, ultimately:
hsbencher_bioinfo raxml raxml ${SUB_VARIANT}

#for variant in {det_rr,det_ld_preload,nondet};
#do
# hsbencher_bioinfo raxml raxml det_rr # $variant
#done
