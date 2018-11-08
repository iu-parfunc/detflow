#!/bin/bash

set -e
dir=$1

if [ "$dir" == "" ]; then
    echo "dirsize.sh must take exactly one argument (directory name!)"
    exit 1
else
    printf "Total size of all files in directory: $dir\n"
    echo "  "$(find "$dir" -type f | wc -l )" files found."
    ANS=$(find "$dir" -type f -print0 | xargs -0 du -scb | tail -n 1 | awk '{ print $1 }' )
    # LC_NUMERIC=en_US    
    printf "  %'.f bytes total\n" $ANS 
fi
