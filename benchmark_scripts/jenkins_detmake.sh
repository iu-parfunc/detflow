#!/bin/bash

# Run a SINGLE benchmark, corresponding to $BENCHMARK_NAME

set -xe
set -o pipefail

export STACK_LOCK=true

here=`dirname $0`
top=`realpath $here/../`

RESULTS_DIR=${HOME}/results_backup/detmonad/label_${NODE_NAME}/detmake/buildno_${BUILD_NUMBER}/${BENCHMARK_NAME}

mkdir -p ${RESULTS_DIR}

rm -f bench.log
rm -f bench.log.gz
rm -f bench.csv

if [[ ${BENCHMARK_NAME} = 06_splash* ]]; then
  $top/benchmark_scripts/detmake/06_splash.sh 2>&1 | tee bench.log
else
  $top/benchmark_scripts/detmake/${BENCHMARK_NAME}.sh 2>&1 | tee bench.log
fi

gzip -k -9 bench.log
cp bench.log.gz ${RESULTS_DIR}/

$top/benchmark_scripts/hsbencher-ingest-log -h
$top/benchmark_scripts/hsbencher-ingest-log bench.log bench.csv
if [ -e bench.csv  ]; then
    cp bench.csv ${RESULTS_DIR}/
fi
