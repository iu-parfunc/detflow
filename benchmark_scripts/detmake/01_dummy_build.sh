#!/bin/bash

set -xe

top=`pwd`
benchdir=$top/benchmark_scripts

. $benchdir/common.sh --source-only

# TODO: Abstract this out
$benchdir/intro.sh
cd $top/examples/detmake/
./fetch_programs.sh
cd $top

for variant in {det_ld_preload,nondet,gnumake};
do
    hsbencher_make 01_dummy_build 01_dummy_build $variant all
done

