#!/bin/bash

set -xe

top=`pwd`

if [ "${DOCKER}" == "1" ]; then
    STK="stack $STACK_ARGS --docker"
else
    STK="stack $STACK_ARGS"
fi

$STK install
./fetch_programs.sh

function run_detmake() {
    set +x
    echo;echo;echo " Running detmake in "$(basename `pwd`)
    echo "================================================================================"
    set -x
    DEBUG=1 $STK exec -- detmake $@
}

# --------------------------------------------------

cd $top/01_dummy_build
run_detmake

cd $top/02_readline
# make get detmake

cd $top/03_raxml
run_detmake raxmlHPC-AVX

cd $top/04_aspell
# run_detmake

cd $top/05_wildcardapalooza
run_detmake

cd $top/06_splash/apps/raytrace/src
run_detmake raytrace
