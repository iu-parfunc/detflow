#!/bin/bash

set -e

if [ "$1" == "-v" ]; then
    TEE=/dev/stderr
    shift
else
    TEE=/dev/null
fi

dir=$1

if [ "$dir" == "" ]; then
    echo "hashdir.sh must take exactly one argument (directory name!)"
    exit 1
else
    cd "$dir"
    # By convention we ignore .git... there should probably be a better way to do this:
    hashdeep -lr . | grep -v "^#" | sort | grep -v ".git/" | tee $TEE | md5sum | awk '{ print $1 }'
fi
