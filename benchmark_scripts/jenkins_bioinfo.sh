#!/bin/bash

# -------------------------------------------------
# Run a SINGLE benchmark and record its CSV results
# -------------------------------------------------

# Env vars used:
#  * FINISHME - document these

set -xe -o pipefail

# IU-specific Hack for magramal:
if [[ ${HOSTNAME} = magramal* ]]; then
    export STACK_ROOT=$HOME/.stack_nfs
fi
    
set -o pipefail

# Start out in NFS when we run via Jenkins
here=`dirname $0`
origin=`realpath $here/../`
# FRESHWD=`mktemp -d`
FRESHWD=/tmp/jenkins_detmonad_bioinfo_tempwd

mkdir -p $FRESHWD
cd $FRESHWD
rsync -rplt --delete "$origin"/ ./

export STACK_LOCK=true

BENCHMARK_DIR=${BENCHMARK_NAME}

# Allow parallelism between different runs with different variants.
# I.e. between clustal_nondet and clustal_det_ld_preload
if [[ ${BENCHMARK_NAME} = clustal_* ]]; then
  SUB_VARIANTS=${BENCHMARK_NAME#clustal_}
  BENCHMARK_NAME=clustal
elif [[ ${BENCHMARK_NAME} = raxml_* ]]; then
  SUB_VARIANTS=${BENCHMARK_NAME#raxml_}
  BENCHMARK_NAME=raxml
else
  SUB_VARIANTS="nondet det_ld_preload"
fi

top=`pwd`
RESULTS_DIR=${HOME}/results_backup/detmonad/label_${NODE_NAME}/bioinfo/buildno_${BUILD_NUMBER}/${BENCHMARK_DIR}

mkdir -p ${RESULTS_DIR}

# had some I/O errors in long runs.  Moving this to /tmp:
LOG=/tmp/bench_`date '+%s'`.log

# This overrides the thread behavior to let Jenkins control the thread count.  
# But we need to prevent the output logs from colliding:
if [ "$THREADSETTINGS" == "" ]; then
    DESTROOT=bench
else
    THREADSFLAT=`echo $THREADSETTINGS | sed 's/ /_/g'`
    DESTROOT=bench_N${THREADSFLAT}
fi

rm -f "${LOG}"
rm -f "${LOG}.gz"
rm -f bench.csv

for sv in ${SUB_VARIANTS};
do
    SUB_VARIANT="${sv}" "$top/benchmark_scripts/bioinfo/${BENCHMARK_NAME}.sh" 2>&1 
done | tee "$LOG"

gzip -k -9 "$LOG"
cp "${LOG}.gz" "${RESULTS_DIR}/${DESTROOT}.log.gz"

"$top/benchmark_scripts/hsbencher-ingest-log" -h
"$top/benchmark_scripts/hsbencher-ingest-log" "${LOG}" "${RESULTS_DIR}/${DESTROOT}.csv"

cd "$origin"
rm -rf "$FRESHWD"
