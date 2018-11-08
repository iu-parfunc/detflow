#!/bin/bash

set -xe

top=`pwd`

if [ "${DOCKER}" == "1" ]; then
  TESTMODE=test_docker
else
  TESTMODE=test
fi

# BioInformatics Tests
# ----------------------------------------
# We can run "make test" twice if we want to compare two outputs:

for dir in raxml bwa clustal mothur
do
    set +x
    echo
    echo Testing bioinformatics app: $dir
    echo ================================================================================
    set -x
    cd $top/examples/bioinfo/$dir
    make clean
    make HOWMANY=2 $TESTMODE
done
