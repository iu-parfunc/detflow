#!/bin/bash

echo "Running 06_splash.sh"

set -xe

top=$(realpath `dirname $0`/../../)
benchdir=$top/benchmark_scripts

. $benchdir/common.sh --source-only

# TODO: Abstract this out
$benchdir/intro.sh
cd $top/examples/detmake/
./fetch_programs.sh
cd $top

if [ -z "$SUB_VARIANT" ];
then
  for variant in {det_ld_preload,nondet,gnumake}; do
    hsbencher_make "${BENCHMARK_NAME}" "${BENCHMARK_NAME}/src" $variant ""
  done
else
    export variant=$SUB_VARIANT
    hsbencher_make "${BENCHMARK_NAME}" "${BENCHMARK_NAME}/src" $SUB_VARIANT ""
fi
