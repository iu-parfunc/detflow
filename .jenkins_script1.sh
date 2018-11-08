#!/bin/bash

# First phase of testing: build it and run unit tests.

set -xe

if [ "${DOCKER}" == "1" ]; then
  stack docker pull
#  stack --docker setup
  stack --docker $STACK_ARGS install
  stack --docker $STACK_ARGS test
else
  stack $STACK_ARGS setup
  stack $STACK_ARGS test
fi
