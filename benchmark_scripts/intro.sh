#!/bin/bash

set -xe

if [ "$SKIPINTRO" == "1" ]; then exit 0; fi 

echo "Running on machine: "`hostname -a`
uname -a

echo "Git commit:"
(git log | head) || echo ok
echo "Git commit depth: "
(git log --pretty=oneline | wc -l) || echo ok

top=`pwd`

if [[ "${DOCKER}" == "1" ]]; then
  DOCKERARGS=--docker
  TESTMODE=test_docker
else
  DOCKERARGS=
  TESTMODE=test
fi

# Flaky network things
function retry_it()
{
    for ix in {1..5}; do
        set +e    
        $@
        code=$?
        set -e
        if [ "$code" == 0 ]; then 
            break;
        elif [ $ix == 5 ]; then
            echo "retry_it: failed after several tries";
            exit 1;
        else
            echo "retry_it: command FAILED on attempt $ix.";
        fi
        sleep 5;
    done
}

stack --version
if [[ "${DOCKER}" == "1" ]]; then
  retry_it stack docker pull
fi

retry_it stack $STACK_ARGS $DOCKERARGS setup
retry_it stack $STACK_ARGS $DOCKERARGS build

# Finally install hsbencher for some of its command line tools:
cd `dirname $0`
(cd ./hsbencher && \
  retry_it stack $STACK_ARGS setup && \
  retry_it stack $STACK_ARGS $DOCKERARGS install hsbencher hsbencher-graph --local-bin-path ../)
