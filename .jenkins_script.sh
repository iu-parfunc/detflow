#!/bin/bash

# Responds to env var $STACK_ARGS

set -xe

if [ "$STACK_ARGS" == "" ]; then
  STACK_ARGS=$*
fi
export STACK_ARGS
export DOCKER

echo "Running on machine: "`hostname -a`
uname -a

echo "Git commit:"
(git log | head) || echo ok
echo "Git commit depth: "
(git log --pretty=oneline | wc -l) || echo ok

stack --version

./.jenkins_script1.sh
./.jenkins_script2.sh
./.jenkins_script3.sh
